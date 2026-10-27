# GLOBAL UNCERTAINTY OF THE HEADLINE SHARE ###################################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "readxl", "jsonlite"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# Load coded corpus and metadata ----------------------------------------------

out <- here("datasets", "output")
arm_dir <- file.path(out, "arm")
chk_dir <- file.path(out, "checkpoint")
dt <- load_coded_corpus_fun()
dt[, domain_short := fcase(
  main_category == "Crop knowledge and management", "Crop",
  main_category == "Irrigation practices and communal water management", "Irrigation",
  main_category == "Soil and moisture management", "Soil & moisture",
  main_category == "Weather and climate conditions", "Weather & climate")]
anchor <- mean(dt$efficiency_legible == "FALSE")

# Rule-only draws: three families x three phrasings x five draws ---------------

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

# DeepSeek leg: retired model versus current one ------------------------------

deepseek_source <- Sys.getenv("DEEPSEEK_SOURCE", "v4")
stopifnot(deepseek_source %in% c("v4", "chat"))
suffix <- if (deepseek_source == "v4") "" else "_deepseekchat"

if (deepseek_source == "v4") {
  v4 <- rbindlist(lapply(names(bin_files), function(nm) {
    f <- file.path(out, sprintf("v4_ruleonly_%s_deepseek_raw_all.csv", nm))
    d <- read_arm(f)
    d[, legible := as.logical(parse_json_field(text, "efficiency_legible"))]
    d[, .(prompt = nm, provider = "deepseek",
          id_cite = sub("__s.*$", "", custom_id), legible)]
  }))[!is.na(legible)]
  bin <- rbind(bin[provider != "deepseek"], v4)
}
cat(sprintf("DeepSeek leg: %s\n", deepseek_source))

cat("Draw coverage per prompt x provider (assumptions, draws):\n")
print(bin[, .(n_assumptions = uniqueN(id_cite), n_draws = .N),
          by = .(prompt, provider)])

# Per-assumption irreducible draw share, joined to site and domain -------------

draw_share <- bin[, .(s_irr = mean(!legible)), by = .(prompt, provider, id_cite)]
draw_share <- merge(draw_share, dt[, .(id_cite, site, domain_short)],
                    by = "id_cite")

# Full factorial grid ----------------------------------------------------------

vote_rules <- c(`any draw` = 0.2, majority = 0.6, unanimous = 1.0)
grid <- CJ(community = c("none", unique(dt$site)),
           domain = c("none", unique(dt$domain_short)),
           model = unique(bin$provider),
           prompt = names(bin_files),
           vote = names(vote_rules))
grid[, c("share", "n") := {
  sub <- draw_share[prompt == .BY$prompt & provider == .BY$model &
                      (.BY$community == "none" | site != .BY$community) &
                      (.BY$domain == "none" | domain_short != .BY$domain)]
  .(mean(sub$s_irr >= vote_rules[[.BY$vote]]), nrow(sub))
}, by = .(community, domain, model, prompt, vote)]
cat(sprintf(paste0("\nGrid: %d cells | corpus per cell %d-%d | share range ",
                   "%.3f-%.3f | grand mean %.3f | SD %.3f\n"),
            nrow(grid), min(grid$n), max(grid$n), min(grid$share),
            max(grid$share), mean(grid$share), sd(grid$share)))
cat(sprintf("Full-instrument primary anchor (Claude majority): %.3f\n\n", anchor))

# Exact functional-ANOVA (Sobol') decomposition via Moebius inversion ---------

fac <- c("community", "domain", "model", "prompt", "vote")
pop_var <- function(x) mean((x - mean(x))^2)
vy <- pop_var(grid$share)
subsets <- unlist(lapply(seq_along(fac), function(k)
  combn(fac, k, simplify = FALSE)), recursive = FALSE)
partial <- list()
for (s in subsets) {
  key <- paste(s, collapse = ":")
  cvar <- pop_var(grid[, .(mu = mean(share)), by = s]$mu)
  lower <- Filter(function(t) all(t %in% s) && length(t) < length(s),
                  subsets)
  partial[[key]] <- cvar - sum(vapply(lower, function(t)
    partial[[paste(t, collapse = ":")]], numeric(1)))
}
stopifnot(abs(sum(unlist(partial)) - vy) < 1e-12)
indices <- data.table(term = names(partial),
                      order = lengths(subsets),
                      si = unlist(partial) / vy)
totals <- data.table(factor = fac)
totals[, ti := vapply(factor, function(f)
  indices[grepl(f, term, fixed = TRUE), sum(si)], numeric(1))]
totals[, si_first := indices[order == 1][match(factor, term), si]]
cat("First- and total-order indices:\n")
print(totals[order(-si_first),
             .(factor, si_first = round(si_first, 4), ti = round(ti, 4))])
cat("\nSecond-order terms above 0.01:\n")
print(indices[order == 2 & si > 0.01][order(-si),
              .(term, si = round(si, 4))])
cat(sprintf("\nVariance in interactions of any order: %.4f\n",
            indices[order > 1, sum(si)]))

# Write summaries --------------------------------------------------------------

summary_tab <- data.table(
  quantity = c("grand_mean", "sd", "min", "max", "anchor_full_instrument",
               "var_total", "var_interactions_share"),
  value = round(c(mean(grid$share), sd(grid$share), min(grid$share),
                  max(grid$share), anchor, vy,
                  indices[order > 1, sum(si)]), 4))
fwrite(grid, file.path(out, sprintf("sobol_headline_grid%s.csv", suffix)))
fwrite(indices[, .(term, order, si = round(si, 6))],
       file.path(out, sprintf("sobol_headline_indices%s.csv", suffix)))
fwrite(totals[, .(factor, si_first = round(si_first, 6), ti = round(ti, 6))],
       file.path(out, sprintf("sobol_headline_totals%s.csv", suffix)))
fwrite(summary_tab, file.path(out, sprintf("sobol_headline_summary%s.csv", suffix)))
