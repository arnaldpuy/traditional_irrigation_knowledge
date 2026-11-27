# SENSITIVITY OF THE STRENGTH-OF-EVIDENCE RESULT ##############################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

source_dirs <- strsplit(Sys.getenv("STAGE2_SOURCE_DIRS", "stage2_llm_v2,stage2_llm_supp"), ",")[[1]]
out_dir <- here("datasets", "output", "stage2_evidence")
le_dir <- here("datasets", "output", "link_extraction", "fable_full")

# Read -------------------------------------------------------------------------

read_sources_fun <- function(f) rbindlist(lapply(source_dirs, function(d) fread(here("datasets", "output", d, f))), fill = TRUE)
cit <- fread(file.path(out_dir, "stage2_citations_adjudicated.csv"))
votes <- read_sources_fun("stage2_llm_votes.csv")
answers <- read_sources_fun("stage2_llm_answers.csv")
links <- fread(file.path(le_dir, "link_rows_cited.csv"))
manifest <- fread(file.path(le_dir, "stage2_retrieval_manifest.csv"))
supp_manifest <- file.path(le_dir, "stage2_supplementary_manifest.csv")
if (file.exists(supp_manifest))
  manifest <- rbind(manifest, fread(supp_manifest, colClasses = list(character = c("date", "sha256", "note", "file", "route"))), fill = TRUE)
adj_fun <- fread(here("metadata", "stage2_function_adjudication.csv"))
adj_ans <- fread(here("metadata", "stage2_answer_adjudication.csv"))
setnames(links, "function", "func")
qs <- paste0("q", 1:9)
levels5 <- c("Very low", "Low", "Moderate", "High", "Very high")
to_level_fun <- function(x) factor(levels5[pmin(5L, findInterval(round(x, 9), c(0, 0.2, 0.4, 0.6, 0.8)))],
                                   levels = levels5)
level_count_fun <- function(x) {
  tab <- table(factor(x, levels = levels5))
  c(as.list(setNames(as.integer(tab), c("very_low", "low", "moderate", "high", "very_high"))),
    list(not_rated = sum(is.na(x))))
}

# Rebuild the citation table under each vote rule -----------------------------

grid <- CJ(rule = c("any_draw", "majority", "unanimous"), variant = c("extraction", "narrow"))
all_rules <- rbindlist(Map(function(rl, vv) {
  w <- dcast(votes[, .(cited_no, claim_id, question, v = get(rl))], cited_no + claim_id ~ question,
             value.var = "v")
  base <- cit[variant == vv, .(cited_no, claim_id, citation_group, technology, func,
                               direction, upstream, sample_part, design_weight)]
  x <- merge(base, w, by = c("cited_no", "claim_id"))
  x[, q2_statement := q2]
  x[adj_ans[scope == "both"], q2_statement := FALSE, on = .(cited_no, claim_id)]
  x[adj_ans, q2 := FALSE, on = .(cited_no, claim_id)]
  x[q1 == FALSE, (qs[-1]) := FALSE]
  x[q1 == FALSE, q2_statement := FALSE]
  x[q2 == FALSE, (qs[-(1:2)]) := FALSE]
  x[, n_q39 := q3 + q4 + q5 + q6 + q7 + q8 + q9]
  x[, `:=`(vote_rule = rl, variant = vv)]
  x[]
}, grid$rule, grid$variant))
stopifnot(all_rules[, .N, by = .(variant, vote_rule)][, uniqueN(N)] <= 2L,
          all_rules[variant == "extraction" & vote_rule == "majority", .N] == cit[variant == "extraction", .N])
deliv <- all_rules[direction == "delivers"]

# Headline numbers under each vote rule ---------------------------------------

headline_rules <- all_rules[, .(n_citations = .N, n_primary = sum(q1), n_supports_statement = sum(q2_statement),
                                n_own_result = sum(q2),
                                n_all_nine = sum(q1 & q2 & n_q39 == 7),
                                n_all_three_dimensions = sum(q2 & (q3 | q4) & (q5 | q6 | q7) & (q8 | q9))),
                            by = .(variant, vote_rule, population = fifelse(direction == "delivers",
                                                                            "delivery claims", "other directions"))]
headline_rules <- rbind(headline_rules,
                        all_rules[, .(population = "all directions", n_citations = .N, n_primary = sum(q1),
                                      n_supports_statement = sum(q2_statement), n_own_result = sum(q2), n_all_nine = sum(q1 & q2 & n_q39 == 7),
                                      n_all_three_dimensions = sum(q2 & (q3 | q4) & (q5 | q6 | q7) & (q8 | q9))),
                                  by = .(variant, vote_rule)], use.names = TRUE)
setorder(headline_rules, variant, population, vote_rule)
fwrite(headline_rules, file.path(out_dir, "stage2_headline_vote_rules.csv"))
cat("headline numbers by vote rule (delivery claims):\n")
print(headline_rules[population == "delivery claims"])

# Scoring rules for the composite level ---------------------------------------

score_cells_fun <- function(x, scoring) {
  x <- copy(x)
  x[, f := as.numeric(q1 & q2)]
  if (scoring == "filtered_mean") {
    s <- x[, .(score = mean(f * n_q39 / 7)), by = .(technology, func)]
  } else if (scoring == "filtered_median") {
    s <- x[, .(score = median(f * n_q39 / 7)), by = .(technology, func)]
  } else if (scoring == "filtered_equal_dimensions") {
    x[, dim_score := f * (((q3 + q4) / 2) + ((q5 + q6 + q7) / 3) + ((q8 + q9) / 2)) / 3]
    s <- x[, .(score = mean(dim_score)), by = .(technology, func)]
  } else if (scoring == "filtered_equal_documents") {
    d <- x[, .(doc_score = mean(f * n_q39 / 7)), by = .(technology, func, cited_no)]
    s <- d[, .(score = mean(doc_score)), by = .(technology, func)]
  } else if (scoring == "filtered_four_items") {
    s <- x[, .(score = mean(f * (q4 + q7 + q8 + q9) / 4)), by = .(technology, func)]
  } else if (scoring == "quality_among_passing") {
    s <- x[, .(score = if (sum(f) > 0) mean((n_q39 / 7)[f == 1]) else NA_real_), by = .(technology, func)]
  } else if (scoring == "nine_questions") {
    s <- x[, .(score = mean((q1 + q2 + n_q39) / 9)), by = .(technology, func)]
  }
  s[, level := to_level_fun(score)]
  s[]
}
scorings <- c("filtered_mean", "filtered_median", "filtered_equal_dimensions", "filtered_equal_documents",
              "filtered_four_items", "quality_among_passing", "nine_questions")
sens_levels <- rbindlist(lapply(scorings, function(sc) {
  deliv[, c(list(scoring = sc, n_cells = uniqueN(paste(technology, func))),
            level_count_fun(score_cells_fun(.SD, sc)$level)), by = .(variant, vote_rule)]
}))
setorder(sens_levels, variant, vote_rule, scoring)
fwrite(sens_levels, file.path(out_dir, "stage2_sensitivity_levels.csv"))
cat("\nstrength-of-evidence level counts by scoring rule (majority, extraction variant; NA cells unrated):\n")
print(sens_levels[variant == "extraction" & vote_rule == "majority"])

# Relevance share by vote rule and variant ------------------------------------

sens_rel <- deliv[, .(n_citations = .N, share_primary = mean(q1), share_own_result = mean(q2),
                      mean_q39_among_own = fifelse(sum(q2) > 0, mean(n_q39[q2]) / 7, NA_real_)),
                  by = .(variant, vote_rule, technology, func)]
setorder(sens_rel, variant, technology, func, vote_rule)
fwrite(sens_rel, file.path(out_dir, "stage2_sensitivity_relevance.csv"))
rel_range <- sens_rel[, .(min_share_own = min(share_own_result), max_share_own = max(share_own_result)),
                      by = .(variant, technology, func)]
cat("\ncells whose relevance share moves by more than 10 points across vote rules:",
    rel_range[max_share_own - min_share_own > 0.1, .N], "of", nrow(rel_range), "\n")

# Sample composition and design weights ---------------------------------------

maj_e <- deliv[vote_rule == "majority" & variant == "extraction"]
comp_cell <- maj_e[, .(n_original = sum(sample_part == "original"), n_supplement = sum(sample_part == "supplement"),
                       share_own_original = fifelse(any(sample_part == "original"), mean(q2[sample_part == "original"]), NA_real_),
                       share_own_combined = mean(q2),
                       share_own_weighted = sum(design_weight * q2) / sum(design_weight),
                       weight_min = min(design_weight), weight_max = max(design_weight)),
                   by = .(technology, func)]
comp_all <- maj_e[, .(technology = "all", func = "all", n_original = sum(sample_part == "original"),
                      n_supplement = sum(sample_part == "supplement"),
                      share_own_original = mean(q2[sample_part == "original"]), share_own_combined = mean(q2),
                      share_own_weighted = sum(design_weight * q2) / sum(design_weight),
                      weight_min = min(design_weight), weight_max = max(design_weight))]
eff <- c("water_conservation", "crop_production")
comp_type <- maj_e[, .(technology = "all", func = fifelse(func[1] %in% eff, "efficiency functions", "other functions"),
                       n_original = sum(sample_part == "original"), n_supplement = sum(sample_part == "supplement"),
                       share_own_original = mean(q2[sample_part == "original"]), share_own_combined = mean(q2),
                       share_own_weighted = sum(design_weight * q2) / sum(design_weight),
                       weight_min = min(design_weight), weight_max = max(design_weight)),
                   by = .(func_type = func %in% eff)][, func_type := NULL]
comp <- rbind(comp_all, comp_type, comp_cell)
setorder(comp, -n_supplement, -n_original)
fwrite(comp, file.path(out_dir, "stage2_sensitivity_sample_design.csv"))
cat("\nown-result share: original only / combined / design-weighted\n")
print(comp[n_supplement > 0 | technology == "all", .(technology, func, n_original, n_supplement,
                                                original = round(share_own_original, 3), combined = round(share_own_combined, 3),
                                                weighted = round(share_own_weighted, 3), weight_max = round(weight_max, 1))])

# Retrieval bounds -------------------------------------------------------------

missing <- unique(links[cited_no %in% manifest[status == "manual", cited_no],
                        .(cited_no, citation_group, technology, func, direction)])
missing[, claim_id := paste(citation_group, technology, func, sep = "_")]
missing[, missing_reason := "document not retrieved"]
wrong_doc <- adj_fun[action == "drop" & !is.na(cited_no),
                     .(cited_no, claim_id, technology, func = `function`, direction,
                       missing_reason = "retrieved document is not the cited work")]
wrong_doc[, citation_group := mapply(function(cid, t, f) sub(paste0("_", t, "_", f, "$"), "", cid),
                                     claim_id, technology, func, USE.NAMES = FALSE)]
missing <- rbind(missing, wrong_doc, use.names = TRUE)
missing <- missing[!paste(cited_no, claim_id) %in% cit[, paste(cited_no, claim_id)]]
missing <- missing[!claim_id %in% adj_fun[action == "drop" & is.na(cited_no) & variant == "all", claim_id]]
cat("missing citations by reason:\n"); print(missing[, .N, by = .(missing_reason, direction)])
rec <- adj_fun[action == "recode_direction", .(claim_id, new_direction)]
missing[rec, direction := i.new_direction, on = "claim_id"]
miss_cells <- missing[direction == "delivers", .(n_missing = .N), by = .(technology, func)]
rated <- deliv[vote_rule == "majority" & variant == "extraction",
               .(n_rated = .N, n_own = sum(q2), sum_score = sum(fifelse(q1 & q2, n_q39 / 7, 0))),
               by = .(technology, func)]
pi_tab <- fread(file.path(le_dir, "stage2_inclusion_probabilities.csv"))[, .(cited_no, pi_original)]
supp_s <- if (file.exists(file.path(le_dir, "stage2_supplementary_sample.csv")))
  fread(file.path(le_dir, "stage2_supplementary_sample.csv"))[, .(cited_no, pi_supplement, stratum)] else NULL
missing <- merge(missing, pi_tab, by = "cited_no", all.x = TRUE)
if (!is.null(supp_s)) missing <- merge(missing, supp_s[, .(cited_no, pi_supplement)], by = "cited_no", all.x = TRUE)
missing[, pi := fifelse(is.na(pi_supplement), pi_original, pi_original + (1 - pi_original) * pi_supplement)]
missing[, w := 1 / pi]
stopifnot(all(missing$pi > 0))
if (!is.null(supp_s)) {
  cat("supplement retrieval by stratum:\n")
  print(merge(supp_s, manifest[, .(cited_no, retrieved = status == "retrieved")], by = "cited_no")[,
        .(n = .N, retrieved = sum(retrieved), share = round(mean(retrieved), 2)), by = stratum])
}
rated_w <- deliv[vote_rule == "majority" & variant == "extraction",
                 .(w_rated = sum(design_weight), w_own = sum(design_weight * q2)), by = .(technology, func)]
miss_w <- missing[direction == "delivers", .(w_missing = sum(w)), by = .(technology, func)]
bounds <- merge(rated, miss_cells, by = c("technology", "func"), all = TRUE)
bounds <- merge(bounds, rated_w, by = c("technology", "func"), all.x = TRUE)
bounds <- merge(bounds, miss_w, by = c("technology", "func"), all.x = TRUE)
for (v in c("w_rated", "w_own", "w_missing")) bounds[is.na(get(v)), (v) := 0]
bounds[, share_own_weighted_lower := w_own / (w_rated + w_missing)]
bounds[, share_own_weighted_upper := (w_own + w_missing) / (w_rated + w_missing)]
bounds[is.na(n_rated), `:=`(n_rated = 0L, n_own = 0L, sum_score = 0)]
bounds[is.na(n_missing), n_missing := 0L]
bounds[, retrieval_coverage := n_rated / (n_rated + n_missing)]
bounds[, share_own_rated := fifelse(n_rated > 0, n_own / n_rated, NA_real_)]
bounds[, share_own_lower := n_own / (n_rated + n_missing)]
bounds[, share_own_upper := (n_own + n_missing) / (n_rated + n_missing)]
bounds[, composite_rated := fifelse(n_rated > 0, sum_score / n_rated, NA_real_)]
bounds[, composite_lower := sum_score / (n_rated + n_missing)]
bounds[, composite_upper := (sum_score + n_missing) / (n_rated + n_missing)]
bounds[, level_lower := to_level_fun(composite_lower)]
bounds[, level_upper := to_level_fun(composite_upper)]
bounds[, missing_full_yes_needed_for_moderate :=
         fifelse(n_missing > 0, pmax(0, ceiling(0.4 * (n_rated + n_missing) - sum_score)), NA_real_)]
setorder(bounds, -n_missing)
fwrite(bounds, file.path(out_dir, "stage2_retrieval_bounds.csv"))
cat(sprintf("\nmissing delivery citations: %d in %d cells; overall relevance share bounds [%.3f, %.3f] around %.3f\n",
            sum(bounds$n_missing), bounds[n_missing > 0, .N],
            sum(bounds$n_own) / sum(bounds$n_rated + bounds$n_missing),
            sum(bounds$n_own + bounds$n_missing) / sum(bounds$n_rated + bounds$n_missing),
            sum(bounds$n_own) / sum(bounds$n_rated)))
cat(sprintf("weighted bounds on the overall own-result share: [%.3f, %.3f] around %.3f (missing-data bounds, not confidence intervals)\n",
            sum(bounds$w_own) / sum(bounds$w_rated + bounds$w_missing),
            sum(bounds$w_own + bounds$w_missing) / sum(bounds$w_rated + bounds$w_missing),
            sum(bounds$w_own) / sum(bounds$w_rated)))
cat("cells reaching moderate if every missing citation passed both filters and met all seven criteria:",
    bounds[level_upper %in% c("Moderate", "High", "Very high") & n_rated > 0, .N], "\n")
print(bounds[n_missing > 0, .(technology, func, n_rated, n_missing, retrieval_coverage = round(retrieval_coverage, 2),
                              share_own_rated = round(share_own_rated, 2), share_own_lower = round(share_own_lower, 2),
                              share_own_upper = round(share_own_upper, 2), level_upper)][order(-n_missing)][1:15])

# Unit summaries ---------------------------------------------------------------

maj <- deliv[vote_rule == "majority"]
unit <- rbind(
  maj[, .(unit = "citation", n = .N, n_primary = sum(q1), n_own_result = sum(q2),
          share_own_result = mean(q2)), by = variant],
  maj[, .(q1 = any(q1), q2 = any(q2)), by = .(variant, cited_no)][, .(unit = "cited document", n = .N,
          n_primary = sum(q1), n_own_result = sum(q2), share_own_result = mean(q2)), by = variant],
  maj[, .(q1 = any(q1), q2 = any(q2)), by = .(variant, claim_id)][, .(unit = "review statement (claim)",
          n = .N, n_primary = sum(q1), n_own_result = sum(q2), share_own_result = mean(q2)), by = variant])
fwrite(unit, file.path(out_dir, "stage2_unit_summaries.csv"))
cat("\nunit summaries (delivery claims, majority):\n"); print(unit)

# Agreement conditional on eligibility ----------------------------------------

ag <- dcast(answers[, .(cited_no, claim_id, question, draw, answer)], cited_no + claim_id + question ~ draw,
            value.var = "answer")
setnames(ag, c("1", "2", "3"), c("d1", "d2", "d3"))
ag[, pair_agreement := ((d1 == d2) + (d1 == d3) + (d2 == d3)) / 3]
ag[, unanimous := d1 == d2 & d2 == d3]
elig <- cit[variant == "extraction" & q2 == TRUE, .(cited_no, claim_id)]
ag[, passes_q2 := paste(cited_no, claim_id) %in% paste(elig$cited_no, elig$claim_id)]
agree <- rbind(
  ag[, .(population = "all citations", n = .N, pair_agreement = mean(pair_agreement),
         unanimous_share = mean(unanimous)), by = question],
  ag[passes_q2 == TRUE, .(population = "citations passing Q2 (majority, function reading)", n = .N,
                          pair_agreement = mean(pair_agreement), unanimous_share = mean(unanimous)),
     by = question],
  ag[passes_q2 == FALSE, .(population = "citations not passing Q2", n = .N,
                           pair_agreement = mean(pair_agreement), unanimous_share = mean(unanimous)),
     by = question], use.names = TRUE)
stopifnot(agree[population == "all citations", unique(n)] == uniqueN(ag[, .(cited_no, claim_id)]))
setorder(agree, population, question)
fwrite(agree, file.path(out_dir, "stage2_agreement_conditional.csv"))
cat("\nunanimous share by population:\n")
print(dcast(agree[, .(question, population, u = round(unanimous_share, 3))], question ~ population, value.var = "u"))
