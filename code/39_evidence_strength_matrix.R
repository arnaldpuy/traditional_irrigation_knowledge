# WHAT THE MODERNIZATION LITERATURE ACTUALLY DOCUMENTS, AND HOW WELL ##########

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "jsonlite"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

out <- here("datasets", "output")
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a

funcs12 <- c("crop_production", "water_conservation", "soil_health",
             "risk_management", "groundwater_recharge",
             "microclimate_regulation", "biodiversity", "pest_control",
             "livestock", "medicinal_food", "social_cohesion",
             "recreation_wellbeing")
core2 <- c("crop_production", "water_conservation")
fun_lab <- c(crop_production = "Crop production",
             water_conservation = "Water conservation",
             soil_health = "Soil health", risk_management = "Risk management",
             social_cohesion = "Social cohesion", biodiversity = "Biodiversity",
             groundwater_recharge = "Groundwater recharge",
             microclimate_regulation = "Microclimate regulation",
             pest_control = "Pest control", livestock = "Livestock",
             medicinal_food = "Medicinal/food plants",
             recreation_wellbeing = "Recreation/wellbeing")

# Parse the per-draw extractions ----------------------------------------------

parse_one <- function(f, tag) {
  x <- as.data.table(readRDS(file.path(out, f)))
  rbindlist(lapply(seq_len(nrow(x)), function(i) {
    o <- tryCatch(fromJSON(x$text[i]), error = function(e) NULL)
    ff <- o$function_findings
    if (is.null(ff) || !length(ff)) return(NULL)
    ff <- as.data.table(ff)
    ff[, `:=`(rec_id = x$custom_id[i], draw = x$draw[i], model = tag,
              tier = o$evidence_tier %||% NA_character_,
              n_tech = length(unlist(o$technologies)),
              techs = paste(sort(unlist(o$technologies)), collapse = "|"))]
    ff[]
  }), fill = TRUE)
}

d <- rbind(parse_one("literature_extraction_raw_claude.rds", "claude"),
           parse_one("literature_extraction_raw_deepseek.rds", "deepseek"),
           fill = TRUE)
d <- d[`function` %in% funcs12]
setnames(d, "function", "f")

# Human adjudication of flagged attributions (2026-08-14) ----------------------

adj <- fread(file.path(out, "evidence_flags_adjudicated.csv"))
drop_pairs <- adj[verdict == "drop"]
drop_recs <- adj[verdict == "exclude_paper", unique(rec_id)]
n0 <- nrow(d)
d <- d[!drop_pairs, on = c(rec_id = "rec_id", f = "fn")]
d <- d[!rec_id %in% drop_recs]
cat(sprintf("adjudication: %d draws dropped (%d false pairs, %d excluded paper)\n",
            n0 - nrow(d), nrow(drop_pairs), length(drop_recs)))

cat(sprintf("%d findings from %d records, %d extractions\n", nrow(d),
            uniqueN(d$rec_id), uniqueN(d[, paste(rec_id, model, draw)])))

# Grades are ordered, so "weakest" and "strongest" are well defined ------------

grades <- c(weak = 1L, moderate = 2L, strong = 3L)
d[, grade := grades[evidence_strength]]
d <- d[!is.na(grade)]

n_extr <- d[, .(n_extr = uniqueN(paste(model, draw))), by = rec_id]

deliv <- d[direction == "delivered"]
per_rf <- deliv[, .(n_say = uniqueN(paste(model, draw)),
                    n_models = uniqueN(model),
                    g_min = min(grade), g_max = max(grade),
                    g_mode = as.integer(names(sort(table(grade),
                                                   decreasing = TRUE))[1])),
                by = .(rec_id, f)]
per_rf <- merge(per_rf, n_extr, by = "rec_id")

# Nine specifications ----------------------------------------------------------

pres_rules <- list(
  `any extraction` = function(x) x$n_say >= 1,
  `most extractions` = function(x) x$n_say / x$n_extr > 0.5,
  `both models` = function(x) x$n_models == 2L)
grade_rules <- list(weakest = "g_min", modal = "g_mode", strongest = "g_max")

specs <- CJ(presence = names(pres_rules), grading = names(grade_rules),
            sorted = FALSE)

res <- rbindlist(lapply(seq_len(nrow(specs)), function(i) {
  s <- specs[i]
  keep <- per_rf[pres_rules[[s$presence]](per_rf)]
  keep[, g := get(grade_rules[[s$grading]])]
  keep[, .(presence = s$presence, grading = s$grading, n_records = .N,
           n_strong = sum(g == 3L), n_moderate = sum(g == 2L),
           n_weak = sum(g == 1L)), by = f]
}))
fwrite(res, file.path(out, "evidence_strength_matrix.csv"))

# Report ------------------------------------------------------------------------

rng <- function(x) sprintf("%.0f (%.0f-%.0f)", median(x), min(x), max(x))

cat("\nRecords documenting each function as delivered, by grade of evidence\n")
cat("(median across the nine specifications):\n\n")
tab <- res[, .(records = as.numeric(median(n_records)),
               strong = as.numeric(median(n_strong)),
               moderate = as.numeric(median(n_moderate)),
               weak = as.numeric(median(n_weak))), by = f]
tab[, label := fun_lab[f]]
setorder(tab, -records)
print(tab[, .(label, records, strong, moderate, weak)])

cat("\nConcentration of the strongest evidence:\n")
conc <- res[, .(core = sum(n_strong[f %in% core2]), all = sum(n_strong)),
            by = .(presence, grading)][, share := core / all]
cat(sprintf("  share of strongly evidenced records falling on crop production\n"))
cat(sprintf("  and water conservation: %.0f%% (%.0f-%.0f%%)\n",
            100 * median(conc$share), 100 * min(conc$share), 100 * max(conc$share)))

cat("\nThe functions modernization promises publicly:\n")
for (v in c("social_cohesion", "biodiversity")) {
  r <- res[f == v]
  cat(sprintf("  %-22s documented in %s records, of which strong: %s\n",
              fun_lab[v], rng(r$n_records), rng(r$n_strong)))
}

cat("\nHighest grade ever reached, over every extraction of every record:\n")
top <- deliv[, .(best = max(grade)), by = .(f)][order(-best)]
top[, label := fun_lab[f]]
print(top[, .(label, best_grade = names(grades)[best])])

cat("\nEvidence tier of the records:\n")
print(unique(d[, .(rec_id, model, draw, tier)])[, .(extractions = .N), by = tier][
  order(-extractions)])

# Technology by function, for the matrix ---------------------------------------

tech_set <- c("drip", "remote_sensing", "soil_sensor", "app_scheduling",
              "on_demand", "flow_metering")
gen <- deliv[, .(rec_id, f, grade, model, draw,
                 tech = strsplit(techs, "|", fixed = TRUE))]
gen <- gen[, .(tech = unlist(tech)), by = .(rec_id, f, grade, model, draw)]
gen <- gen[tech %in% tech_set]

per_rtf <- gen[, .(n_models = uniqueN(model),
                   g = as.integer(names(sort(table(grade), decreasing = TRUE))[1])),
               by = .(rec_id, tech, f)]
cells <- per_rtf[n_models == 2L, .(n_records = uniqueN(rec_id),
                                   best_grade = max(g)), by = .(tech, f)]
full <- CJ(tech = tech_set, f = funcs12, unique = TRUE)
cells <- merge(full, cells, by = c("tech", "f"), all.x = TRUE)
cells[is.na(n_records), `:=`(n_records = 0L, best_grade = 0L)]
cells[, grade_label := c("none", "weak", "moderate", "strong")[best_grade + 1L]]
fwrite(cells, file.path(out, "evidence_tech_function.csv"))

cat("\nTechnology by function, best grade reached (generous attribution):\n")
print(dcast(cells, f ~ tech, value.var = "grade_label", fill = "none"))
cat("\nPapers behind each cell:\n")
print(dcast(cells, f ~ tech, value.var = "n_records", fill = 0))

# Secondary: the single-technology records --------------------------------------

st <- deliv[n_tech == 1L]
cat(sprintf("\nSingle-technology records only (%d extractions, %d records):\n",
            uniqueN(st[, paste(rec_id, model, draw)]), uniqueN(st$rec_id)))
stt <- st[, .(records = uniqueN(rec_id), strong = uniqueN(rec_id[grade == 3L])),
          by = .(techs, f)]
print(dcast(stt, f ~ techs, value.var = "records", fill = 0))
cat("\nRecords with STRONG evidence, by technology and function:\n")
print(dcast(stt, f ~ techs, value.var = "strong", fill = 0))
