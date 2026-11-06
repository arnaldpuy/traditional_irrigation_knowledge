# IS REDUCIBILITY DRIVEN BY CONTENT OR BY TOPIC? ##############################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "jsonlite"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

out <- here("datasets", "output")
arm_dir <- file.path(out, "arm")
chk_dir <- file.path(out, "checkpoint")

# Corpus metadata -------------------------------------------------------------

dt <- load_coded_corpus_fun()
dt[, domain_short := fcase(
  main_category == "Crop knowledge and management", "Crop",
  main_category == "Irrigation practices and communal water management", "Irrigation",
  main_category == "Soil and moisture management", "Soil & moisture",
  main_category == "Weather and climate conditions", "Weather & climate")]

funcs12 <- c("crop_production", "water_conservation", "soil_health",
             "risk_management", "groundwater_recharge",
             "microclimate_regulation", "biodiversity", "pest_control",
             "livestock", "medicinal_food", "social_cohesion",
             "recreation_wellbeing")
core2 <- c("crop_production", "water_conservation")

# The removal-test draws, deepseek leg on v4 ----------------------------------

read_arm <- function(f) as.data.table(utils::read.csv(f, stringsAsFactors = FALSE,
                                                      encoding = "UTF-8"))
parse_json_field <- function(txt, field) {
  vapply(txt, function(t) {
    if (is.na(t) || t == "") return(NA_character_)
    o <- tryCatch(fromJSON(t), error = function(e) NULL)
    v <- o[[field]]
    if (is.null(v)) NA_character_ else as.character(v)
  }, character(1), USE.NAMES = FALSE)
}

bin_files <- c(standard = file.path(chk_dir, "chk_standard_raw_all.csv"),
               short = file.path(arm_dir, "arm_short_raw_all.csv"),
               strict = file.path(arm_dir, "arm_strict_raw_all.csv"))
bin <- rbindlist(lapply(names(bin_files), function(nm) {
  d <- read_arm(bin_files[[nm]])
  d[, legible := as.logical(parse_json_field(text, "efficiency_legible"))]
  d[, .(prompt = nm, provider, id_cite = sub("__s.*$", "", custom_id), legible)]
}))[!is.na(legible)]
v4 <- rbindlist(lapply(names(bin_files), function(nm) {
  d <- read_arm(file.path(out, sprintf("v4_ruleonly_%s_deepseek_raw_all.csv", nm)))
  d[, legible := as.logical(parse_json_field(text, "efficiency_legible"))]
  d[, .(prompt = nm, provider = "deepseek",
        id_cite = sub("__s.*$", "", custom_id), legible)]
}))[!is.na(legible)]
bin <- rbind(bin[provider != "deepseek"], v4)

draw_share <- merge(bin[, .(s_irr = mean(!legible)), by = .(prompt, provider, id_cite)],
                    dt[, .(id_cite, site, domain_short)], by = "id_cite")

vote_rules <- c(`any draw` = 0.2, majority = 0.6, unanimous = 1.0)
spec_grid <- CJ(vote = names(vote_rules), drop = c("none", unique(dt$site)),
                sorted = FALSE)

# Function sets under the nine attribution arms -------------------------------

arm_file <- function(fam, ph) {
  if (fam == "anthropic" && ph == "standard")
    file.path(out, "coding_results_raw_analytical_anthropic.csv")
  else file.path(out, sprintf("coding_results_raw_attribution_%s_%s.csv", ph, fam))
}
families <- c("anthropic", "openai", "deepseek")
phrasings <- c("strict", "standard", "broad")

draws <- rbindlist(lapply(families, function(fam)
  rbindlist(lapply(phrasings, function(ph) {
    d <- read_arm(arm_file(fam, ph))[!duplicated(custom_id)]
    rbindlist(lapply(seq_len(nrow(d)), function(i) {
      o <- tryCatch(fromJSON(d$text[i]), error = function(e) NULL)
      fs <- if (is.null(o$functions)) character(0) else
        intersect(unlist(o$functions), funcs12)
      if (!length(fs)) return(NULL)
      data.table(family = fam, phrasing = ph,
                 id_cite = sub("__s.*$", "", d$custom_id[i]), f = fs)
    }))
  }))))

# Non-core function load under the primary coding and under each arm ----------

noncore_primary <- dt[, .(id_cite, n_noncore = vapply(
  strsplit(functions, ";", fixed = TRUE),
  function(f) sum(!trimws(f) %in% core2), integer(1)))]

noncore_arm <- draws[, .(n_noncore = sum(!unique(f) %in% core2)),
                     by = .(family, phrasing, id_cite)]

# Odds ratios -----------------------------------------------------------------

or_haldane <- function(a, b, c, d) ((a + 0.5) * (d + 0.5)) / ((b + 0.5) * (c + 0.5))

cells_fun <- function(x) {
  x[, .(a = sum(core_only & !irr), b = sum(core_only & irr),
        c = sum(!core_only & !irr), d = sum(!core_only & irr)),
    by = domain_short]
}

mh_or <- function(c4) {
  n <- c4[, a + b + c + d]
  num <- sum(c4$a * c4$d / n)
  den <- sum(c4$b * c4$c / n)
  if (den == 0) NA_real_ else num / den
}

run_all <- function(nc, tag) {
  rbindlist(lapply(split(draw_share, by = c("prompt", "provider")), function(ds) {
    ds2 <- copy(ds)
    rbindlist(lapply(seq_len(nrow(spec_grid)), function(i) {
      sub <- ds2[spec_grid$drop[i] == "none" | site != spec_grid$drop[i]]
      sub <- merge(sub, nc, by = "id_cite")
      sub[, irr := s_irr >= vote_rules[[spec_grid$vote[i]]]]
      sub[, core_only := n_noncore == 0L]
      c4 <- cells_fun(sub)
      marg <- sub[, .(a = sum(core_only & !irr), b = sum(core_only & irr),
                      c = sum(!core_only & !irr), d = sum(!core_only & irr))]
      dom <- c4[, .(domain_short, or = or_haldane(a, b, c, d))]
      data.table(coding = tag, prompt = ds2$prompt[1], provider = ds2$provider[1],
                 vote = spec_grid$vote[i], drop = spec_grid$drop[i],
                 or_marginal = marg[, or_haldane(a, b, c, d)],
                 rr_marginal = marg[, (a / (a + b)) / (c / (c + d))],
                 or_mh = mh_or(c4),
                 or_crop = dom[domain_short == "Crop", or],
                 or_irrigation = dom[domain_short == "Irrigation", or],
                 or_soil = dom[domain_short == "Soil & moisture", or],
                 or_weather = dom[domain_short == "Weather & climate", or])
    }))
  }))
}

res_primary <- run_all(noncore_primary, "primary")

res_arms <- rbindlist(lapply(families, function(fam)
  rbindlist(lapply(phrasings, function(ph)
    run_all(noncore_arm[family == fam & phrasing == ph, .(id_cite, n_noncore)],
            sprintf("%s/%s", fam, ph))))))

res <- rbind(res_primary, res_arms)
fwrite(res, file.path(out, "within_domain_or.csv"))

# Reducible share by domain and by non-core load ------------------------------

dose <- rbindlist(lapply(split(draw_share, by = c("prompt", "provider")), function(ds) {
  ds2 <- merge(copy(ds), noncore_primary, by = "id_cite")
  ds2[, ncat := fifelse(n_noncore >= 2L, "two or more", as.character(n_noncore))]
  rbindlist(lapply(seq_len(nrow(spec_grid)), function(i) {
    sub <- ds2[spec_grid$drop[i] == "none" | site != spec_grid$drop[i]]
    sub[, .(red = mean(s_irr < vote_rules[[spec_grid$vote[i]]]), n = .N),
        by = .(domain_short, ncat)][
          , .(domain_short, ncat, red, n, vote = spec_grid$vote[i],
              drop = spec_grid$drop[i], prompt = sub$prompt[1],
              provider = sub$provider[1])]
  }))
}))
fwrite(dose, file.path(out, "within_domain_dose.csv"))

# The same contrast under the three human raters ------------------------------

human <- merge(fread(file.path(out, "human_validation_ratings.csv"))[
  !is.na(irreducible), .(rater, id_cite, irr = irreducible == 1L)],
  merge(noncore_primary, dt[, .(id_cite, domain_short)], by = "id_cite"),
  by = "id_cite")
human[, core_only := n_noncore == 0L]
res_human <- human[, {
  a <- sum(core_only & !irr); b <- sum(core_only & irr)
  c <- sum(!core_only & !irr); d <- sum(!core_only & irr)
  .(a = a, b = b, c = c, d = d, p_core = a / (a + b), p_noncore = c / (c + d),
    rr_marginal = (a / (a + b)) / (c / (c + d)),
    or_marginal = or_haldane(a, b, c, d), or_mh = mh_or(cells_fun(.SD)))
}, by = rater, .SDcols = c("core_only", "irr", "domain_short")]
fwrite(res_human, file.path(out, "within_domain_or_human.csv"))

# Report ----------------------------------------------------------------------

q <- function(x) sprintf("%.1f (IQR %.1f-%.1f)", median(x, na.rm = TRUE),
                         quantile(x, 0.25, na.rm = TRUE),
                         quantile(x, 0.75, na.rm = TRUE))

cat(sprintf("\nPrimary function coding, %d label specifications\n", nrow(res_primary)))
cat("Odds of being classified reducible, core-only against non-core:\n\n")
cat(sprintf("  pooled, ignoring topic       %s\n", q(res_primary$or_marginal)))
cat(sprintf("  holding topic constant (MH)  %s\n", q(res_primary$or_mh)))
dom_cols <- c(or_crop = "Crop", or_irrigation = "Irrigation",
              or_soil = "Soil & moisture", or_weather = "Weather & climate")
cat("\n  within each knowledge domain:\n")
for (nm in names(dom_cols))
  cat(sprintf("    %-18s %s\n", dom_cols[[nm]], q(res_primary[[nm]])))

cat(sprintf("\n  specifications with MH odds ratio above 1: %.0f%%\n",
            100 * mean(res_primary$or_mh > 1, na.rm = TRUE)))
cat(sprintf("  specifications with MH odds ratio above 2: %.0f%%\n",
            100 * mean(res_primary$or_mh > 2, na.rm = TRUE)))

cat(sprintf("\nAcross the nine function-attribution arms (%d specifications)\n",
            nrow(res_arms)))
cat(sprintf("  holding topic constant (MH)  %s\n", q(res_arms$or_mh)))
cat(sprintf("  above 1 in %.0f%% of them\n", 100 * mean(res_arms$or_mh > 1, na.rm = TRUE)))
cat("\n  by arm:\n")
print(res_arms[, .(median_mh = round(median(or_mh, na.rm = TRUE), 1),
                   above_1 = sprintf("%.0f%%", 100 * mean(or_mh > 1, na.rm = TRUE))),
               by = coding])

cat("\nReducible share, core-only over non-core (times more likely):\n")
cat(sprintf("  LLMs, primary coding, median over %d specifications  %s\n",
            nrow(res_primary), q(res_primary$rr_marginal)))
cat("  human raters:\n")
print(res_human[, .(rater, p_core = round(p_core, 2), p_noncore = round(p_noncore, 2),
                    rr = round(rr_marginal, 2), or = round(or_marginal, 2),
                    or_mh = round(or_mh, 2))])

cat("\nReducible share within each domain, by functions served outside the core:\n")
print(dcast(dose[, .(red = median(red)), by = .(domain_short, ncat)],
            domain_short ~ ncat, value.var = "red")[
              , lapply(.SD, function(x) if (is.numeric(x)) sprintf("%.0f%%", 100 * x) else x)])
