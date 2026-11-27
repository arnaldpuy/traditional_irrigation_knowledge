# STRENGTH OF EVIDENCE PER TECHNOLOGY AND FUNCTION ############################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "ggplot2", "cowplot", "scales", "openxlsx"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

source_dirs <- strsplit(Sys.getenv("STAGE2_SOURCE_DIRS", "stage2_llm_v2,stage2_llm_supp"), ",")[[1]]
out_dir <- here("datasets", "output", "stage2_evidence")
dir.create(out_dir, showWarnings = FALSE)
le_dir <- here("datasets", "output", "link_extraction", "fable_full")

# Read the reading, the sample and the adjudication ---------------------------

read_sources_fun <- function(f) rbindlist(lapply(source_dirs, function(d) {
  x <- fread(here("datasets", "output", d, f))
  x[, source_dir := d]
  x
}), fill = TRUE)
votes <- read_sources_fun("stage2_llm_votes.csv")
answers <- read_sources_fun("stage2_llm_answers.csv")
claims <- read_sources_fun("stage2_llm_claims.csv")
stopifnot(!anyDuplicated(claims[, .(cited_no, claim_id)]))
cat("sources:", paste(source_dirs, collapse = " + "), "->", uniqueN(claims$cited_no), "studies,", nrow(claims), "citations\n")
sample <- fread(file.path(le_dir, "stage2_sample.csv"))
supp_file <- file.path(le_dir, "stage2_supplementary_sample.csv")
pi_tab <- fread(file.path(le_dir, "stage2_inclusion_probabilities.csv"))[, .(cited_no, pi_original)]
design <- merge(sample[, .(cited_no, block)], pi_tab, by = "cited_no", all.x = TRUE)
design[, pi := pi_original]
if (file.exists(supp_file)) {
  supp <- fread(supp_file)
  supp_d <- merge(supp[, .(cited_no, block, pi_supplement)], pi_tab, by = "cited_no", all.x = TRUE)
  supp_d[, pi := pi_original + (1 - pi_original) * pi_supplement]
  design <- rbind(design, supp_d[, .(cited_no, block, pi_original, pi)])
  sample <- rbind(sample, supp[, intersect(names(sample), names(supp)), with = FALSE], fill = TRUE)
}
design[, design_weight := 1 / pi]
stopifnot(all(claims$cited_no %in% design$cited_no), all(design$pi > 0))
links <- fread(file.path(le_dir, "link_rows_cited.csv"))
adj_fun <- fread(here("metadata", "stage2_function_adjudication.csv"))
adj_ans <- fread(here("metadata", "stage2_answer_adjudication.csv"))
np_types <- fread(here("metadata", "stage2_nonprimary_types.csv"))
setnames(claims, "function", "func")
setnames(links, "function", "func")
votes[, c("technology", "function", "direction") := NULL]
stopifnot(all(adj_fun$claim_id %in% claims$claim_id),
          all(adj_fun$action %in% c("drop", "recode_direction")),
          all(adj_fun$variant %in% c("all", "narrow_social_cohesion")),
          all(adj_ans$question == "q2"), all(adj_ans$new_answer == "no"),
          all(adj_ans$scope %in% c("both", "function_only")))

# Citation-level table: majority answers, flags, link type --------------------

wide <- dcast(votes[, .(cited_no, claim_id, question, majority)], cited_no + claim_id ~ question,
              value.var = "majority")
flags <- answers[question == "q2", .(upstream = sum(upstream) >= 2,
                                     technology_mismatch = sum(technology_mismatch) >= 2),
                 by = .(cited_no, claim_id)]
link_type <- unique(links[, .(cited_no, citation_group, link_type)])
cit <- merge(claims[, .(cited_no, claim_id, rec_id, citation_group, technology, func, direction, passage)],
             wide, by = c("cited_no", "claim_id"))
cit <- merge(cit, flags, by = c("cited_no", "claim_id"))
cit <- merge(cit, link_type, by = c("cited_no", "citation_group"), all.x = TRUE)
cit <- merge(cit, np_types[, .(cited_no, document_type)], by = "cited_no", all.x = TRUE)
cit[q1 == TRUE, document_type := "primary_study"]
stopifnot(!anyNA(cit$document_type))
cit <- merge(cit, design[, .(cited_no, block, pi, design_weight)], by = "cited_no")
cit[, sample_part := fifelse(block == "supplement", "supplement", "original")]

# Adjudication -----------------------------------------------------------------

apply_adjudication_fun <- function(cit, variant = c("extraction", "narrow")) {
  variant <- match.arg(variant)
  x <- copy(cit)
  rows <- if (variant == "narrow") adj_fun else adj_fun[variant == "all"]
  drops <- rows[action == "drop"]
  drop_claim <- drops[is.na(cited_no), claim_id]
  x <- x[!claim_id %in% drop_claim]
  x <- x[!drops[!is.na(cited_no), .(cited_no, claim_id)], on = .(cited_no, claim_id)]
  rec <- rows[action == "recode_direction", .(claim_id, new_direction)]
  x[rec, direction := i.new_direction, on = "claim_id"]
  x[, q2_statement := any(q2), by = .(cited_no, passage)]
  neg <- adj_ans[scope == "both"][x, on = .(cited_no, claim_id), nomatch = 0L, .(cited_no, passage)]
  x[unique(neg), q2_statement := FALSE, on = .(cited_no, passage)]
  x[adj_ans, `:=`(q2 = FALSE, upstream = fifelse(i.set_upstream, TRUE, upstream)),
    on = .(cited_no, claim_id)]
  x[q2 == FALSE, c("q3", "q4", "q5", "q6", "q7", "q8", "q9") := FALSE]
  x[, variant := variant]
  x[]
}
cit_all <- rbind(apply_adjudication_fun(cit, "extraction"), apply_adjudication_fun(cit, "narrow"))
cit_all[, n_q39 := q3 + q4 + q5 + q6 + q7 + q8 + q9]
cit_all[, evidence_score := fifelse(q1 & q2, n_q39 / 7, 0)]
fwrite(cit_all, file.path(out_dir, "stage2_citations_adjudicated.csv"))
cat(sprintf("raw citations %d; after adjudication: extraction variant %d, narrow variant %d\n",
            nrow(cit), cit_all[variant == "extraction", .N], cit_all[variant == "narrow", .N]))
cat(sprintf("Q2 overrides applied: %d\n", nrow(adj_ans)))

# Headline numbers with their denominators ------------------------------------

headline_fun <- function(x, population) {
  x[, .(population = population, n_citations = .N, n_studies = uniqueN(cited_no),
        n_statements = uniqueN(citation_group),
        n_primary = sum(q1), n_supports_statement = sum(q2_statement), n_own_result = sum(q2),
        n_primary_not_on_claim = sum(q1 & !q2),
        n_upstream = sum(q1 & !q2 & upstream), n_technology_mismatch = sum(q1 & !q2 & technology_mismatch),
        n_rigour = sum(q2 & (q3 | q4)), n_applicability = sum(q2 & (q5 | q6 | q7)),
        n_transparency = sum(q2 & (q8 | q9)),
        n_all_three_dimensions = sum(q2 & (q3 | q4) & (q5 | q6 | q7) & (q8 | q9)),
        n_all_nine = sum(q1 & q2 & n_q39 == 7))]
}
headline <- rbind(
  headline_fun(cit_all[variant == "extraction" & sample_part == "original"], "original sample, all directions (extraction variant)"),
  headline_fun(cit_all[variant == "extraction" & sample_part == "original" & direction == "delivers"], "original sample, delivery claims (extraction variant)"),
  headline_fun(cit_all[variant == "extraction" & sample_part == "supplement" & direction == "delivers"], "supplement, delivery claims (extraction variant)"),
  headline_fun(cit_all[variant == "extraction"], "adjudicated, all directions (extraction variant)"),
  headline_fun(cit_all[variant == "extraction" & direction == "delivers"], "adjudicated, delivery claims (extraction variant)"),
  headline_fun(cit_all[variant == "extraction" & direction == "harms"], "adjudicated, harm claims (extraction variant)"),
  headline_fun(cit_all[variant == "narrow"], "adjudicated, all directions (narrow variant)"),
  headline_fun(cit_all[variant == "narrow" & direction == "delivers"], "adjudicated, delivery claims (narrow variant)"))
headline[, vote_rule := "majority of three draws"]
fwrite(headline, file.path(out_dir, "stage2_headline.csv"))
print(headline[, .(population, n_citations, n_primary, n_supports_statement, n_own_result, n_upstream, n_technology_mismatch,
                   n_all_three_dimensions, n_all_nine)])
cat("\nnon-primary citations by document type (extraction variant, all directions):\n")
print(cit_all[variant == "extraction" & q1 == FALSE, .(citations = .N, studies = uniqueN(cited_no)),
              by = document_type][order(-citations)])

# Cell tables ------------------------------------------------------------------

funcs <- c("water_conservation", "crop_production", "soil_health", "risk_management",
           "groundwater_recharge", "pest_control", "social_cohesion", "biodiversity",
           "microclimate_regulation", "recreation_wellbeing", "livestock", "medicinal_food")
techs <- c("drip", "soil_sensor", "remote_sensing", "app_scheduling", "on_demand", "flow_metering")
levels5 <- c("Very low", "Low", "Moderate", "High", "Very high")
to_level_fun <- function(x) factor(levels5[pmin(5L, findInterval(round(x, 9), c(0, 0.2, 0.4, 0.6, 0.8)))],
                                   levels = levels5)
deliv <- cit_all[direction == "delivers"]
cell <- deliv[, .(n_citations = .N, n_studies = uniqueN(cited_no), n_supplement = sum(sample_part == "supplement"),
                  n_primary = sum(q1),
                  n_supports_statement = sum(q2_statement), n_own_result = sum(q2),
                  share_primary = mean(q1), share_supports_statement = mean(q2_statement),
                  share_own_result = mean(q2),
                  share_own_result_weighted = sum(design_weight * q2) / sum(design_weight),
                  mean_q39_among_own_weighted = fifelse(sum(q2) > 0, sum((design_weight * n_q39 / 7)[q2]) / sum(design_weight[q2]), NA_real_),
                  share_own_among_primary = fifelse(sum(q1) > 0, sum(q2) / sum(q1), NA_real_),
                  mean_q39_among_own = fifelse(sum(q2) > 0, mean(n_q39[q2]) / 7, NA_real_),
                  n_rigour = sum(q2 & (q3 | q4)), n_applicability = sum(q2 & (q5 | q6 | q7)),
                  n_transparency = sum(q2 & (q8 | q9)),
                  evidence_score = mean(evidence_score)), by = .(variant, technology, func)]
cell[, evidence_level := to_level_fun(evidence_score)]
cell <- merge(CJ(variant = c("extraction", "narrow"), technology = techs, func = funcs), cell,
              by = c("variant", "technology", "func"), all.x = TRUE)
setorder(cell, variant, technology, func)
fwrite(cell, file.path(out_dir, "stage2_cell_summary.csv"))
cell_q <- deliv[q2 == TRUE, lapply(.SD, mean), by = .(variant, technology, func),
                .SDcols = paste0("q", 3:9)]
cell_q <- merge(cell_q, deliv[, .(n_citations = .N, n_own_result = sum(q2)),
                              by = .(variant, technology, func)], by = c("variant", "technology", "func"))
cell_q <- melt(cell_q, id.vars = c("variant", "technology", "func", "n_citations", "n_own_result"),
               variable.name = "question", value.name = "share_yes_among_own")
setorder(cell_q, variant, technology, func, question)
fwrite(cell_q, file.path(out_dir, "stage2_cell_questions.csv"))
for (v in c("extraction", "narrow")) {
  cat(sprintf("\n%s variant: %d occupied cells; strength-of-evidence levels:\n", v,
              cell[variant == v & !is.na(n_citations), .N]))
  print(cell[variant == v & !is.na(n_citations), .N, by = evidence_level][order(evidence_level)])
}

# Labels shared by the figures ------------------------------------------------

promised <- c("water_conservation", "crop_production", "soil_health", "risk_management",
              "groundwater_recharge", "social_cohesion", "biodiversity")
func_lab <- c(water_conservation = "Water conservation", crop_production = "Crop production",
              soil_health = "Soil health", risk_management = "Risk management",
              groundwater_recharge = "Groundwater recharge", pest_control = "Pest control",
              social_cohesion = "Social cohesion", biodiversity = "Biodiversity",
              microclimate_regulation = "Microclimate regulation",
              recreation_wellbeing = "Recreation/wellbeing", livestock = "Livestock",
              medicinal_food = "Medicinal/food plants")
func_lab[promised] <- paste0(func_lab[promised], "*")
tech_lab <- c(drip = "Drip\nirrigation", soil_sensor = "Soil\nsensors",
              remote_sensing = "Remote\nsensing", app_scheduling = "App\nscheduling",
              on_demand = "On-demand\nsupply", flow_metering = "Flow\nmetering")
level_cols <- c("Very low" = "#d73027", "Low" = "#fc8d59", "Moderate" = "#fee08b",
                "High" = "#91cf60", "Very high" = "#1a9850")
fig <- cell[variant == "extraction"]
fig[, technology := factor(technology, levels = techs)]
fig[, func := factor(func, levels = rev(funcs))]
grid_theme <- theme_AP() +
  theme(panel.border = element_blank(), axis.ticks = element_blank(),
        axis.text.x.top = element_text(size = 6), legend.position = "bottom",
        legend.key.height = unit(0.2, "cm"), legend.key.width = unit(0.5, "cm"),
        legend.title.align = 0.5,
        legend.title = element_text(size = 6.5), legend.text = element_text(size = 5.5),
        plot.margin = margin(2, 4, 2, 2))

# Main figure: relevance and quality side by side -----------------------------

fig[, text_a := fifelse(is.na(n_citations), "", sprintf("%d/%d", n_own_result, n_citations))]
fig[, text_b := fifelse(is.na(mean_q39_among_own), "",
                        sprintf("%.0f%%\n(%d)", 100 * mean_q39_among_own, n_own_result))]
plot_relevance <- ggplot(fig, aes(x = technology, y = func)) +
  geom_tile(aes(fill = share_own_result), colour = "white", linewidth = 0.8) +
  geom_tile(data = fig[is.na(n_citations)], fill = "grey93", colour = "white", linewidth = 0.8) +
  geom_text(aes(label = text_a, colour = share_own_result > 0.5), size = 1.8, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = "black", `TRUE` = "white"), na.value = "black") +
  scale_fill_distiller(palette = "Blues", direction = 1, limits = c(0, 1), labels = percent,
                       name = "Citations with an own result on the function", na.value = "grey93") +
  guides(fill = guide_colourbar(title.position = "top")) +
  scale_x_discrete(labels = tech_lab, position = "top") +
  scale_y_discrete(labels = func_lab) +
  labs(x = NULL, y = NULL) + grid_theme
plot_quality <- ggplot(fig, aes(x = technology, y = func)) +
  geom_tile(aes(fill = mean_q39_among_own), colour = "white", linewidth = 0.8) +
  geom_tile(data = fig[is.na(mean_q39_among_own)], fill = "grey93", colour = "white", linewidth = 0.8) +
  geom_text(aes(label = text_b, colour = mean_q39_among_own > 0.5), size = 1.7, lineheight = 0.8,
            show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = "black", `TRUE` = "white"), na.value = "black") +
  scale_fill_distiller(palette = "Oranges", direction = 1, limits = c(0, 1), labels = percent,
                       name = "Study-property questions met (Q3-Q9)", na.value = "grey93") +
  guides(fill = guide_colourbar(title.position = "top")) +
  scale_x_discrete(labels = tech_lab, position = "top") +
  scale_y_discrete(labels = func_lab) +
  labs(x = NULL, y = NULL) + grid_theme + theme(axis.text.y = element_blank())
plot_grid_main <- plot_grid(plot_relevance, plot_quality, ncol = 2L, labels = "auto",
                            label_size = 9, rel_widths = c(0.58, 0.42))
ggsave(here("manuscript", "fig-evidence-grid.pdf"), plot_grid_main, width = 6, height = 3.2)
cat("\nrepaired cells (original vs supplement citations, unweighted and weighted own-result share):\n")
print(cell[variant == "extraction" & n_supplement > 0,
           .(technology, func, n_citations, n_supplement, n_own_result, share_own = round(share_own_result, 2),
             share_own_weighted = round(share_own_result_weighted, 2), quality = round(mean_q39_among_own, 2))])
ggsave(here("manuscript", "fig-evidence-grid.png"), plot_grid_main, width = 6, height = 3.2,
       dpi = 300, bg = "white")

# Human reading: the LLM answers replaced by the readers' answers -------------

d_main <- deliv[variant == "extraction"]
qs <- paste0("q", 1:9)
human_file <- file.path(out_dir, "stage2_human_study_answers.csv")
stopifnot(file.exists(human_file))
human <- fread(human_file)
human_q <- human[, c("cited_no", qs), with = FALSE]
setnames(human_q, qs, paste0("h_", qs))
d_human <- merge(d_main, human_q, by = "cited_no", all.x = TRUE, sort = FALSE)
d_human[, human_rated := cited_no %in% human$cited_no]
for (q in qs) {
  h <- paste0("h_", q)
  d_human[!is.na(get(h)), (q) := get(h)]
}
d_human[, paste0("h_", qs) := NULL]
d_human[, n_q39 := q3 + q4 + q5 + q6 + q7 + q8 + q9]
d_human[, evidence_score := fifelse(q1 & q2, n_q39 / 7, 0)]
cat(sprintf("\nhuman reading: %d of %d delivery citations (%d of %d studies) carry human answers\n",
            d_human[human_rated == TRUE, .N], nrow(d_human), d_human[human_rated == TRUE, uniqueN(cited_no)],
            uniqueN(d_human$cited_no)))
both <- rbind(d_main[, c("technology", "func", qs, "n_q39", "evidence_score"), with = FALSE][, rater := "LLM"],
              d_human[, c("technology", "func", qs, "n_q39", "evidence_score"), with = FALSE][, rater := "Human"])
both[, rater := factor(rater, levels = c("LLM", "Human"))]
cell_both <- both[, .(n_citations = .N, n_pass = sum(q1 & q2), evidence_score = mean(evidence_score)),
                  by = .(rater, technology, func)]
cell_both[, evidence_level := to_level_fun(evidence_score)]
setorder(cell_both, technology, func, rater)
fwrite(cell_both, file.path(out_dir, "stage2_cell_summary_by_rater.csv"))
print(dcast(cell_both[, .N, by = .(rater, evidence_level)], evidence_level ~ rater, value.var = "N"))

# Ratings workbook: the LLM and the human answers behind the main figure -------

q_names <- c(q1 = "primary_study", q2 = "empirical", q3 = "triangulation", q4 = "causality",
             q5 = "representativeness", q6 = "scalability", q7 = "support_factors",
             q8 = "limitations", q9 = "uncertainty")
yn_fun <- function(x) fifelse(x, "yes", "no")
llm_cols <- paste0("llm_", qs, "_", q_names)
human_cols <- paste0("human_", qs, "_", q_names)
ratings <- d_main[, c("cited_no", "claim_id", "rec_id", "technology", "func", "passage", qs,
                      "evidence_score"), with = FALSE]
ratings[, (qs) := lapply(.SD, yn_fun), .SDcols = qs]
setnames(ratings, c(qs, "evidence_score"), c(llm_cols, "llm_score"))
ratings <- human_q[ratings, on = "cited_no"]
ratings[, paste0("h_", qs) := lapply(.SD, yn_fun), .SDcols = paste0("h_", qs)]
setnames(ratings, paste0("h_", qs), human_cols)
ratings <- merge(ratings, d_human[, .(cited_no, claim_id, human_rated,
                                      human_reading_score = evidence_score)],
                 by = c("cited_no", "claim_id"))
setcolorder(ratings, c("cited_no", "claim_id", "rec_id", "technology", "func", "passage",
                       llm_cols, "llm_score", "human_rated", human_cols, "human_reading_score"))
setorder(ratings, technology, func, cited_no, claim_id)
readers_w <- dcast(fread(file.path(out_dir, "stage2_human_answers.csv")),
                   reader + tab + cited_no ~ q, value.var = "answer")
setnames(readers_w, qs, paste0(qs, "_", q_names))
ratings_readme <- data.table(
  column = c("citations", "cited_no", "claim_id", "rec_id", "technology, func", "passage",
             "llm_q*", "llm_score", "human_rated", "human_q*", "human_reading_score",
             "human_readers", "cells"),
  description = c(
    "One row per citation (cited document x review claim that the technology delivers the function), after adjudication",
    "Identifier of the cited document",
    "Identifier of the review claim",
    "Review paper making the claim",
    "Technology and function of the claim",
    "The review sentence carrying the citation",
    "LLM answer (majority of three draws) to questions Q1-Q9, after adjudication",
    "Evidence score of the citation from the LLM answers: 0 unless Q1 and Q2 are yes, otherwise the share of Q3-Q9 answered yes",
    "TRUE when at least one human reader rated the cited document",
    "Human answer (majority of the readers who answered; ties count as no; empty where no reader rated the document or answered the question)",
    "Evidence score of the human reading: human answers where available, LLM answers otherwise (right half of the cells in Fig. 3b)",
    "Every reader's answer per document and question (calibration tab: 28 documents shared by the readers; main tab: documents unique to each reader); empty = blank",
    "Per technology x function and rater: citations, citations passing Q1 and Q2, mean evidence score and its five-level band"))
write.xlsx(list(readme = ratings_readme, citations = ratings, human_readers = readers_w,
                cells = cell_both),
           file.path(out_dir, "stage2_ratings.xlsx"), overwrite = TRUE, creator = "")

# Main figure: the evidence funnel and the strength-of-evidence grid ----------

rater_cols <- c(LLM = "#4575b4", Human = "#f4a261")
n_read <- nrow(d_main)
crit_lab <- c(q3 = "Triangulation (Q3)", q4 = "Causality (Q4)", q5 = "Representativeness (Q5)",
              q6 = "Scalability (Q6)", q7 = "Support factors (Q7)", q8 = "Limitations (Q8)",
              q9 = "Uncertainty (Q9)")
funnel <- both[, {
  pass <- q1 & q2
  data.table(item = c("q1", "q2", names(crit_lab), "all7"),
             n = c(sum(q1), sum(pass), sapply(names(crit_lab), function(q) sum(get(q)[pass])),
                   sum(n_q39[pass] == 7)),
             denom = c(.N, .N, rep(sum(pass), 8L)))
}, by = rater]
funnel[, pct := 100 * n / denom]
crit_order <- funnel[rater == "LLM" & item %in% names(crit_lab)][order(-n), item]
items <- c("q1", "q2", crit_order, "all7")
item_lab <- c(q1 = "Primary study (Q1)", q2 = "Empirical data (Q2)", crit_lab,
              all7 = "All seven (Q3-Q9)")
funnel[, ypos := (length(items) + 1) - match(item, items)]
funnel[item %in% c("q1", "q2"), ypos := ypos + 1.6]
funnel[, ybar := ypos + fifelse(rater == "LLM", 0.19, -0.19)]
n_pass <- funnel[item == "q2", setNames(n, as.character(rater))]
funnel_hdr <- data.table(ypos = c(max(funnel$ypos) + 0.95, funnel[!item %in% c("q1", "q2"), max(ypos)] + 0.95),
                         lab = c(sprintf("Filters (of %d citations)", n_read), "Quality criteria"))
y_items <- unique(funnel[, .(ypos, item)])
plot_funnel <- ggplot(funnel, aes(y = ybar, x = pct, fill = rater)) +
  geom_col(width = 0.36, orientation = "y") +
  geom_text(aes(label = sprintf("%.0f%%", pct)), hjust = 0, nudge_x = 1.5, size = 1.7) +
  geom_text(data = funnel_hdr, aes(y = ypos, label = lab), x = 0, hjust = 0, size = 2,
            fontface = "bold", colour = "grey25", inherit.aes = FALSE) +
  scale_fill_manual(values = rater_cols, name = NULL) +
  scale_y_continuous(breaks = y_items$ypos, labels = item_lab[y_items$item],
                     expand = expansion(add = c(0.5, 0.6))) +
  scale_x_continuous(limits = c(0, 115), breaks = c(0, 50, 100), labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Share of citations", y = NULL) +
  coord_cartesian(clip = "off") +
  theme_AP() +
  theme(panel.border = element_blank(), axis.ticks.y = element_blank(),
        axis.line.x = element_line(colour = "grey40", linewidth = 0.3),
        legend.position = c(1.04, 0.14), legend.justification = c(1, 0),
        legend.margin = margin(0, 0, 0, 0), legend.text = element_text(size = 5.5),
        legend.key.size = unit(0.2, "cm"))
gap <- 0.06
grid_all <- CJ(xi = seq_along(techs), yi = seq_along(funcs))
halves <- copy(cell_both)
halves[, `:=`(xi = match(technology, techs), yi = length(funcs) + 1L - match(func, funcs))]
halves[, `:=`(xmin = fifelse(rater == "LLM", xi - 0.5 + gap / 2, xi),
              xmax = fifelse(rater == "LLM", xi, xi + 0.5 - gap / 2))]
halves[, text_col := fifelse(evidence_level %in% c("Very low", "Very high"), "white", "black")]
bars <- CJ(xi = seq_along(techs), rater = c("LLM", "Human"))
bars[, `:=`(xmin = fifelse(rater == "LLM", xi - 0.5 + gap / 2, xi),
            xmax = fifelse(rater == "LLM", xi, xi + 0.5 - gap / 2))]
plot_levels_main <- ggplot() +
  geom_rect(data = grid_all, aes(xmin = xi - 0.5 + gap / 2, xmax = xi + 0.5 - gap / 2,
                                 ymin = yi - 0.5 + gap / 2, ymax = yi + 0.5 - gap / 2), fill = "grey93") +
  geom_rect(data = halves, aes(xmin = xmin, xmax = xmax, ymin = yi - 0.5 + gap / 2, ymax = yi + 0.5 - gap / 2,
                               fill = evidence_level), colour = "white", linewidth = 0.25) +
  geom_rect(data = bars, aes(xmin = xmin, xmax = xmax, ymin = length(funcs) + 0.6, ymax = length(funcs) + 0.85),
            fill = rater_cols[bars$rater]) +
  geom_text(data = halves, aes(x = (xmin + xmax) / 2, y = yi, label = sprintf("%.0f", 100 * evidence_score),
                               colour = text_col), size = 1.7) +
  scale_colour_identity() +
  scale_fill_manual(values = level_cols, limits = levels5, drop = FALSE, name = "Strength of evidence",
                    guide = guide_legend(nrow = 1L)) +
  scale_x_continuous(breaks = seq_along(techs), labels = tech_lab[techs], position = "top", expand = c(0, 0)) +
  scale_y_continuous(breaks = length(funcs):1, labels = func_lab[funcs], expand = expansion(add = c(0, 0.75))) +
  labs(x = NULL, y = NULL) +
  theme_AP() +
  theme(panel.border = element_blank(), axis.ticks = element_blank(),
        axis.text.x.top = element_text(size = 5.8, lineheight = 0.85), legend.position = "none")
legend_main <- get_legend(ggplot(data.table(l = factor(levels5, levels5)), aes(l, fill = l)) +
                            geom_bar() +
                            scale_fill_manual(values = level_cols, name = "Strength of evidence",
                                              guide = guide_legend(nrow = 1L)) +
                            theme_AP() +
                            theme(legend.position = "bottom", legend.text = element_text(size = 6.5),
                                  legend.title = element_text(size = 7),
                                  legend.key.height = unit(0.28, "cm"), legend.key.width = unit(0.28, "cm")))
plot_strength <- plot_grid(plot_grid(plot_funnel, plot_levels_main, ncol = 2L, labels = "auto",
                                     rel_widths = c(0.43, 0.57)),
                           legend_main, ncol = 1L, rel_heights = c(0.92, 0.08))
ggsave(here("manuscript", "fig-evidence-strength.pdf"), plot_strength, width = 6, height = 2.8)
ggsave(here("manuscript", "fig-evidence-strength.png"), plot_strength, width = 6, height = 2.8,
       dpi = 300, bg = "white")
print(dcast(funnel, item ~ rater, value.var = "pct")[order(match(item, items))])

# SM figure: the five-level grid on its own ----------------------------------

fig[, text_c := fifelse(is.na(n_citations), "", sprintf("%.0f%%\n(%d)", 100 * evidence_score, n_citations))]
fig[, text_col := fifelse(evidence_level %in% c("Very low", "Very high"), "white", "black")]
plot_levels <- ggplot(fig, aes(x = technology, y = func)) +
  geom_tile(aes(fill = evidence_level), colour = "white", linewidth = 0.8, show.legend = TRUE) +
  geom_tile(data = fig[is.na(n_citations)], fill = "grey93", colour = "white", linewidth = 0.8,
            show.legend = FALSE) +
  geom_text(aes(label = text_c, colour = text_col), size = 1.8, lineheight = 0.8, show.legend = FALSE) +
  scale_colour_identity() +
  scale_fill_manual(values = level_cols, limits = levels5, na.value = "grey93",
                    name = "Strength of evidence", drop = FALSE, na.translate = FALSE) +
  scale_x_discrete(labels = tech_lab, position = "top") +
  scale_y_discrete(labels = func_lab) +
  labs(x = NULL, y = NULL) +
  theme_AP() +
  theme(panel.border = element_blank(), axis.ticks = element_blank(),
        axis.text.x.top = element_text(size = 6.5), legend.position = "right",
        legend.key.height = unit(0.35, "cm"), legend.key.width = unit(0.35, "cm"))
ggsave(here("manuscript", "figSM-evidence-levels.pdf"), plot_levels, width = 5.2, height = 2.7)
ggsave(here("manuscript", "figSM-evidence-levels.png"), plot_levels, width = 5.2, height = 2.7,
       dpi = 200, bg = "white")

# SM figure: the nine questions per cell --------------------------------------

q_all <- deliv[variant == "extraction", c(lapply(.SD, mean), .(n_citations = .N)),
               by = .(technology, func), .SDcols = paste0("q", 1:9)]
q_long <- melt(q_all, id.vars = c("technology", "func", "n_citations"), variable.name = "question",
               value.name = "share")
question_lab <- c(q1 = "Primary study", q2 = "Own result", q3 = "Triangulation",
                  q4 = "Mechanism measured", q5 = "3+ settings", q6 = "Above field scale",
                  q7 = "Conditions stated", q8 = "Limitations", q9 = "Uncertainty reported")
q_long[, question := factor(question_lab[question], levels = question_lab)]
q_long[, technology := factor(gsub("\n", " ", tech_lab[technology]), levels = gsub("\n", " ", tech_lab))]
q_long[, func := factor(func, levels = rev(funcs))]
plot_questions <- ggplot(q_long, aes(x = question, y = func, fill = share)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(100 * share), colour = share > 0.5), size = 1.7, show.legend = FALSE) +
  geom_text(data = unique(q_long[, .(technology, func, n_citations)]),
            aes(x = 10.4, y = func, label = n_citations), inherit.aes = FALSE, size = 1.7,
            colour = "grey30") +
  scale_colour_manual(values = c(`FALSE` = "black", `TRUE` = "white")) +
  scale_fill_gradient(low = "white", high = "grey10", limits = c(0, 1), labels = percent,
                      name = "Citations answering yes") +
  scale_x_discrete(expand = expansion(add = c(0.6, 1.6))) +
  scale_y_discrete(labels = func_lab, drop = FALSE) +
  coord_cartesian(clip = "off") +
  facet_wrap(~ technology, ncol = 2L) +
  labs(x = NULL, y = NULL) +
  theme_AP() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5.5),
        axis.text.y = element_text(size = 5.5), panel.border = element_blank(),
        axis.ticks = element_blank(), legend.position = "bottom", panel.spacing.x = unit(0.8, "cm"))
ggsave(here("manuscript", "figSM-evidence-questions.pdf"), plot_questions, width = 6, height = 7)
ggsave(here("manuscript", "figSM-evidence-questions.png"), plot_questions, width = 6, height = 7,
       dpi = 200, bg = "white")
