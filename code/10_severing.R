# WHAT MODERNIZATION WOULD SEVER ##############################################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "readxl", "jsonlite"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

out <- here("datasets", "output")
arm_dir <- file.path(out, "arm"); chk_dir <- file.path(out, "checkpoint")
techs <- c("drip", "soil_sensor", "remote_sensing", "on_demand", "app_scheduling")
tech_lab <- c(drip = "Drip irrigation", soil_sensor = "Soil sensors",
              remote_sensing = "Remote sensing", on_demand = "On-demand supply",
              app_scheduling = "App scheduling")

read_arm <- function(f) as.data.table(utils::read.csv(f, stringsAsFactors = FALSE,
                                                      encoding = "UTF-8"))

# Corpus metadata --------------------------------------------------------------

dt <- load_coded_corpus_fun()
dt[, domain_short := fcase(
  main_category == "Crop knowledge and management", "Crop",
  main_category == "Irrigation practices and communal water management", "Irrigation",
  main_category == "Soil and moisture management", "Soil & moisture",
  main_category == "Weather and climate conditions", "Weather & climate")]

# Severing draws ---------------------------------------------------------------

sev_raw <- read_arm(file.path(arm_dir, "arm_severing_raw_all.csv"))
parsed <- lapply(sev_raw$text, function(t) tryCatch(fromJSON(t), error = function(e) NULL))
keep <- !vapply(parsed, is.null, logical(1))
sev <- rbindlist(lapply(which(keep), function(i) {
  o <- parsed[[i]]
  data.table(id_cite = as.character(o$id_cite)[1], provider = sev_raw$provider[i],
             draw = sev_raw$custom_id[i], tech = techs,
             val = as.character(unlist(o[techs])))
}))
cat(sprintf("severing draws parsed: %d of %d | assumptions %d | families %d\n",
            sum(keep), nrow(sev_raw), uniqueN(sev$id_cite), uniqueN(sev$provider)))

# Reducibility, from the rule-only draws of the headline grid -------------------

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
irr <- bin[, .(s_irr = mean(!legible)), by = .(prompt, provider, id_cite)]

# The grid ---------------------------------------------------------------------

vote_rules <- c(`any draw` = 0.2, majority = 0.6, unanimous = 1.0)
thresholds <- list(direct = "direct", `direct or indirect` = c("direct", "indirect"))
grid <- CJ(community = c("none", unique(dt$site)),
           domain = c("none", unique(dt$domain_short)),
           family = unique(sev$provider), vote = names(vote_rules),
           threshold = names(thresholds), sorted = FALSE)
cat(sprintf("grid: %d cells\n", nrow(grid)))

n_draws <- sev[, .(nd = uniqueN(draw)), by = .(provider, id_cite, tech)]
sev_counts <- merge(sev[, .N, by = .(provider, id_cite, tech, val)], n_draws,
                    by = c("provider", "id_cite", "tech"))
sev_counts <- merge(sev_counts, dt[, .(id_cite, site, domain_short)], by = "id_cite")

cell <- function(i) {
  g <- grid[i]
  lv <- thresholds[[g$threshold]]
  sub <- sev_counts[provider == g$family & val %in% lv &
                      (g$community == "none" | site != g$community) &
                      (g$domain == "none" | domain_short != g$domain)]
  if (!nrow(sub)) return(NULL)
  hit <- sub[, .(share_draws = sum(N) / nd[1]), by = .(id_cite, tech)]
  sev_flag <- hit[share_draws >= vote_rules[[g$vote]]]
  pool <- unique(sev_counts[provider == g$family &
                              (g$community == "none" | site != g$community) &
                              (g$domain == "none" | domain_short != g$domain), id_cite])

  ir <- irr[provider == g$family & prompt == "standard" & id_cite %in% pool]
  ir[, irreducible := s_irr >= 0.6]

  by_tech <- sev_flag[, .(severed = uniqueN(id_cite) / length(pool)), by = tech]
  any_tech <- uniqueN(sev_flag$id_cite) / length(pool)
  sv <- unique(sev_flag$id_cite)
  a <- ir[id_cite %in% sv, mean(irreducible)]
  b <- ir[!id_cite %in% sv, mean(irreducible)]
  cbind(grid[i], dcast(by_tech, . ~ tech, value.var = "severed")[, -1],
        data.table(any_tech = any_tech, irr_among_severed = a,
                   irr_among_untouched = b, n_severed = length(sv)))
}
res <- rbindlist(lapply(seq_len(nrow(grid)), cell), fill = TRUE)

for (tc in techs) res[is.na(get(tc)), (tc) := 0]
fwrite(res, file.path(out, "severing_grid.csv"))

# What each technology would sever ---------------------------------------------

cat("\n=== Share of the corpus each technology would sever ===\n")
for (tc in techs)
  cat(sprintf("%-16s median %3.0f%%  range %3.0f-%3.0f%%\n", tech_lab[tc],
              100 * median(res[[tc]], na.rm = TRUE), 100 * min(res[[tc]], na.rm = TRUE),
              100 * max(res[[tc]], na.rm = TRUE)))
cat(sprintf("%-16s median %3.0f%%  range %3.0f-%3.0f%%\n", "ANY of the five",
            100 * median(res$any_tech), 100 * min(res$any_tech),
            100 * max(res$any_tech)))

cat("\nBy severity threshold (median over the other factors):\n")
print(res[, c(.(any = sprintf("%.0f%%", 100 * median(any_tech))),
              lapply(.SD, function(z) sprintf("%.0f%%", 100 * median(z)))),
          by = threshold, .SDcols = techs])
cat("\nBy LLM family (median over the other factors, inclusive threshold):\n")
print(res[threshold == "direct or indirect",
          c(.(any = sprintf("%.0f%%", 100 * median(any_tech))),
            lapply(.SD, function(z) sprintf("%.0f%%", 100 * median(z)))),
          by = family, .SDcols = techs])

# Is what is severed the knowledge the metric cannot see? ----------------------

cat("\n=== Irreducible share among severed versus untouched assumptions ===\n")
cat(sprintf("severed:   median %.0f%% (%.0f-%.0f%%)\n",
            100 * median(res$irr_among_severed, na.rm = TRUE),
            100 * min(res$irr_among_severed, na.rm = TRUE),
            100 * max(res$irr_among_severed, na.rm = TRUE)))
cat(sprintf("untouched: median %.0f%% (%.0f-%.0f%%)\n",
            100 * median(res$irr_among_untouched, na.rm = TRUE),
            100 * min(res$irr_among_untouched, na.rm = TRUE),
            100 * max(res$irr_among_untouched, na.rm = TRUE)))
res[, gap := irr_among_severed - irr_among_untouched]
cat(sprintf("difference: median %+.1f points; severed are MORE irreducible in %.0f%% of cells\n",
            100 * median(res$gap, na.rm = TRUE), 100 * mean(res$gap > 0, na.rm = TRUE)))
