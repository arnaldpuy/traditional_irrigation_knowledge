# GLOBAL UNCERTAINTY OF THE LADDER AND THE BUNDLE STRUCTURE ###################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "readxl", "jsonlite"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

out <- here("datasets", "output")
funcs12 <- c("crop_production", "water_conservation", "soil_health",
             "risk_management", "groundwater_recharge",
             "microclimate_regulation", "biodiversity", "pest_control",
             "livestock", "medicinal_food", "social_cohesion",
             "recreation_wellbeing")

# The nested widening order named in Results 2 --------------------------------

rungs <- list(
  `Water and yield` = c("crop_production", "water_conservation"),
  `+ soil and risk` = c("soil_health", "risk_management"),
  `+ hydrological` = "groundwater_recharge",
  `+ ecological` = c("microclimate_regulation", "biodiversity", "pest_control"),
  `+ provisioning` = c("livestock", "medicinal_food"),
  `+ social` = c("social_cohesion", "recreation_wellbeing"))
stopifnot(setequal(unlist(rungs), funcs12))
ladder_sets <- lapply(seq_along(rungs), function(i) unlist(rungs[1:i]))
rung_lab <- names(rungs)
names(ladder_sets) <- sprintf("rung%d", seq_along(rungs))
names(rung_lab) <- names(ladder_sets)

# Corpus metadata --------------------------------------------------------------

meta <- as.data.table(read_xlsx(here("datasets", "inputs",
                                     "assumption_list_updated.xlsx")))
setnames(meta, tolower(names(meta)))
meta <- meta[, .(id_cite, site, main_category)]
meta[, domain := fcase(
  main_category == "Crop knowledge and management", "Crop",
  main_category == "Irrigation practices and communal water management", "Irrigation",
  main_category == "Soil and moisture management", "Soil & moisture",
  main_category == "Weather and climate conditions", "Weather & climate")]

# Per-draw records for all nine arms -------------------------------------------

arm_file <- function(fam, ph) {
  if (fam == "anthropic" && ph == "standard")
    file.path(out, "coding_results_raw_analytical_anthropic.csv")
  else
    file.path(out, sprintf("coding_results_raw_attribution_%s_%s.csv", ph, fam))
}
first_chr <- function(x) if (is.null(x) || !length(x)) NA_character_ else
  as.character(x)[1]
read_draws <- function(fam, ph) {
  d <- as.data.table(utils::read.csv(arm_file(fam, ph), stringsAsFactors = FALSE,
                                     encoding = "UTF-8"))
  d <- d[!duplicated(custom_id)]
  parsed <- lapply(seq_len(nrow(d)), function(i)
    tryCatch(fromJSON(d$text[i]), error = function(e) NULL))
  keep <- !vapply(parsed, is.null, logical(1))
  parsed <- parsed[keep]
  ids <- sub("__s.*$", "", d$custom_id[keep])
  dr <- d$custom_id[keep]
  long <- rbindlist(lapply(seq_along(parsed), function(i) {
    fs <- intersect(unlist(parsed[[i]]$functions), funcs12)
    if (!length(fs)) return(NULL)
    data.table(family = fam, phrasing = ph, id_cite = ids[i], draw = dr[i], f = fs)
  }))
  wide <- data.table(
    family = fam, phrasing = ph, id_cite = ids, draw = dr,
    practice = vapply(parsed, function(o) first_chr(o$practice), character(1)),
    epistemic_type = vapply(parsed, function(o) first_chr(o$epistemic_type),
                            character(1)),
    measured_variable = vapply(parsed, function(o) {
      v <- o$measured_variable
      if (is.null(v) || !length(v)) NA_character_ else
        paste(sort(unique(as.character(v))), collapse = ";")
    }, character(1)),
    sensory = vapply(parsed, function(o) {
      v <- o$sensory_channels
      if (is.null(v) || !length(v)) NA_character_ else
        paste(sort(unique(as.character(v))), collapse = ";")
    }, character(1)),
    n_env = vapply(parsed, function(o) {
      v <- o$n_env_variables
      if (is.null(v) || !length(v)) NA_real_ else suppressWarnings(as.numeric(v)[1])
    }, numeric(1)))
  list(long = long, wide = wide)
}
families <- c("anthropic", "openai", "deepseek")
phrasings <- c("strict", "standard", "broad")
all_arms <- lapply(families, function(fam)
  lapply(phrasings, function(ph) read_draws(fam, ph)))
draws <- rbindlist(lapply(all_arms, function(x) rbindlist(lapply(x, `[[`, "long"))))
wide <- rbindlist(lapply(all_arms, function(x) rbindlist(lapply(x, `[[`, "wide"))))
cat(sprintf("draws: %d function rows | %d draw records | %d assumptions\n",
            nrow(draws), nrow(wide), uniqueN(draws$id_cite)))

# Modal categorical judgement per assumption within each arm -------------------

modal <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}
per_assum <- wide[, .(practice = modal(practice),
                      epistemic_type = modal(epistemic_type),
                      measured_variable = modal(measured_variable),
                      sensory = modal(sensory),
                      n_env = median(n_env, na.rm = TRUE)),
                  by = .(family, phrasing, id_cite)]
per_assum <- merge(per_assum, meta, by = "id_cite")

cat("\nPractices by frequency (modal judgement, Claude standard arm):\n")
print(per_assum[family == "anthropic" & phrasing == "standard" &
                  practice != "none",
                .N, by = practice][order(-N)][1:20])

# Twelve keystone practices, fixed across cells so the claim is comparable ------

top12 <- per_assum[family == "anthropic" & phrasing == "standard" &
                     practice != "none", .N, by = practice][
                       order(-N)][1:12, practice]
cat(sprintf("\nKeystone practices: %s\n", paste(top12, collapse = ", ")))

# Vote rule over function attribution ------------------------------------------

n_draws <- draws[, .(nd = uniqueN(draw)), by = .(family, phrasing, id_cite)]
counts <- merge(draws[, .(k = uniqueN(draw)), by = .(family, phrasing, id_cite, f)],
                n_draws, by = c("family", "phrasing", "id_cite"))
counts <- merge(counts, meta, by = "id_cite")
vote_rules <- c(`any draw` = 0.2, majority = 0.6, unanimous = 1.0)

# The full factorial ------------------------------------------------------------

grid <- CJ(community = c("none", unique(meta$site)),
           domain = c("none", unique(meta$domain)),
           family = families, phrasing = phrasings,
           vote = names(vote_rules), sorted = FALSE)
cat(sprintf("grid: %d cells\n", nrow(grid)))

cell_stats <- function(i) {
  g <- grid[i]
  in_cell <- function(d) d[family == g$family & phrasing == g$phrasing &
                             (g$community == "none" | site != g$community) &
                             (g$domain == "none" | domain != g$domain)]
  keep <- in_cell(counts)[k / nd >= vote_rules[[g$vote]]]
  pa <- in_cell(per_assum)
  if (!nrow(keep) || !nrow(pa)) return(NULL)

  fs <- split(keep$f, keep$id_cite)
  lad <- vapply(ladder_sets, function(s)
    mean(vapply(fs, function(f) any(!f %in% s), logical(1))), numeric(1))

  pf <- unique(merge(keep[, .(id_cite, f)], pa[, .(id_cite, practice)],
                     by = "id_cite")[practice %in% top12, .(practice, f)])
  per_practice <- pf[, .N, by = practice]
  per_function <- pf[, .N, by = f]

  ind <- pa[epistemic_type == "indicator" & !is.na(n_env)]

  cbind(as.data.table(as.list(lad)), data.table(
    practices_covered = nrow(per_practice),
    fun_per_practice_med = if (nrow(per_practice)) median(per_practice$N) else NA_real_,
    fun_per_practice_min = if (nrow(per_practice)) min(per_practice$N) else NA_real_,
    fun_per_practice_max = if (nrow(per_practice)) max(per_practice$N) else NA_real_,
    prac_per_function_med = if (nrow(per_function)) median(per_function$N) else NA_real_,
    functions_single_practice = per_function[N == 1, .N],
    n_indicators = nrow(ind),
    ind_ge2_vars = if (nrow(ind)) mean(ind$n_env >= 2) else NA_real_,
    ind_med_vars = if (nrow(ind)) median(ind$n_env) else NA_real_,
    n_distinct_vars = uniqueN(setdiff(trimws(unlist(strsplit(
      ind$measured_variable, ";", fixed = TRUE))), c("none", NA)))))
}
res <- rbindlist(lapply(seq_len(nrow(grid)), function(i) {
  s <- cell_stats(i)
  if (is.null(s)) NULL else cbind(grid[i], s)
}))
fwrite(res, file.path(out, "sobol_ladder_bundles_grid.csv"))

# What the ladder looks like across specifications ------------------------------

cat("\n=== The widening ladder, across specifications ===\n")
lad_tab <- rbindlist(lapply(names(ladder_sets), function(nm)
  data.table(rung = rung_lab[[nm]], k = length(ladder_sets[[nm]]),
             med = median(res[[nm]]), lo = min(res[[nm]]),
             hi = max(res[[nm]]))))
print(lad_tab[, .(rung, k, omitted = sprintf("%.0f%% [%.0f-%.0f%%]", 100 * med,
                                             100 * lo, 100 * hi))])

# Bundle quantities across specifications --------------------------------------

qshow <- c("fun_per_practice_med", "fun_per_practice_max", "fun_per_practice_min",
           "prac_per_function_med", "functions_single_practice",
           "ind_ge2_vars", "n_distinct_vars")
cat("\n=== Bundle quantities, across specifications ===\n")
for (q in qshow)
  cat(sprintf("%-26s median %6.2f  range %6.2f - %6.2f\n", q,
              median(res[[q]], na.rm = TRUE), min(res[[q]], na.rm = TRUE),
              max(res[[q]], na.rm = TRUE)))

# Per-practice function counts, the claim Results 3 states by name -------------

cat("\n=== Functions served by each keystone practice (median [range]) ===\n")
prac_long <- rbindlist(lapply(seq_len(nrow(grid)), function(i) {
  g <- grid[i]
  keep <- counts[family == g$family & phrasing == g$phrasing &
                   k / nd >= vote_rules[[g$vote]] &
                   (g$community == "none" | site != g$community) &
                   (g$domain == "none" | domain != g$domain)]
  pa <- per_assum[family == g$family & phrasing == g$phrasing]
  if (!nrow(keep)) return(NULL)
  pf <- unique(merge(keep[, .(id_cite, f)], pa[, .(id_cite, practice)],
                     by = "id_cite")[practice %in% top12, .(practice, f)])
  cbind(cell = i, pf)
}))
prac_cell <- prac_long[, .N, by = .(cell, practice)]
print(prac_cell[, .(median = median(N), lo = min(N), hi = max(N)),
                by = practice][order(-median)])
fwrite(prac_cell, file.path(out, "practice_function_counts_grid.csv"))

# Which practice is the narrowest, and is it the same one everywhere? ----------

narrowest <- prac_cell[, .SD[N == min(N)], by = cell]
cat("\nPractice with the fewest functions, by share of specifications:\n")
print(narrowest[, .(specifications = .N,
                    share = sprintf("%.0f%%", 100 * .N / uniqueN(prac_cell$cell))),
                by = practice][order(-specifications)])

# Which functions each practice is attributed, and how consistently ------------

n_cells <- uniqueN(prac_long$cell)
incidence <- prac_long[, .(share = .N / n_cells), by = .(practice, f)]
fwrite(incidence[order(practice, -share)],
       file.path(out, "practice_function_incidence.csv"))

# How many practices carry each function, per cell -----------------------------

fun_prac <- merge(CJ(cell = unique(prac_long$cell), f = funcs12),
                  prac_long[, .(n_practices = uniqueN(practice)), by = .(cell, f)],
                  by = c("cell", "f"), all.x = TRUE)
fun_prac[is.na(n_practices), n_practices := 0L]
fwrite(fun_prac, file.path(out, "function_practice_counts_grid.csv"))
cat("\nPractices carrying each function (median [range] over specifications):\n")
print(fun_prac[, .(median = median(n_practices), lo = min(n_practices),
                   hi = max(n_practices)), by = f][order(-median)])

# SENSORY CHANNELS #############################################################

sens_cell <- rbindlist(lapply(seq_len(nrow(grid)), function(i) {
  g <- grid[i]
  pa <- per_assum[family == g$family & phrasing == g$phrasing &
                    (g$community == "none" | site != g$community) &
                    (g$domain == "none" | domain != g$domain) &
                    !is.na(sensory)]
  if (!nrow(pa)) return(NULL)
  ch <- pa[, .(ch = trimws(unlist(strsplit(sensory, ";", fixed = TRUE)))),
           by = .(id_cite, practice)][ch != "none"]
  n_sensed <- uniqueN(ch$id_cite)
  multi <- ch[, .(nch = uniqueN(ch)), by = id_cite]
  cbind(cell = i, ch[, .(n = uniqueN(id_cite)), by = .(ch, practice)],
        n_sensed = n_sensed,
        share_multi = multi[nch >= 2, .N] / max(n_sensed, 1))
}))
fwrite(sens_cell, file.path(out, "sensory_practice_grid.csv"))

cat("\nAssumptions per sensory channel (median [range] over specifications):\n")
print(sens_cell[, .(n = sum(n)), by = .(cell, ch)][
  , .(median = as.numeric(median(n)), lo = min(n), hi = max(n)),
  by = ch][order(-median)])
per_cell_multi <- unique(sens_cell[, .(cell, n_sensed, share_multi)])
cat(sprintf("\nAssumptions with a sensory channel: median %.0f (%d-%d)\n",
            median(per_cell_multi$n_sensed), min(per_cell_multi$n_sensed),
            max(per_cell_multi$n_sensed)))
cat(sprintf("Of those, share reading two or more channels: median %.0f%% (%.0f-%.0f%%)\n",
            100 * median(per_cell_multi$share_multi),
            100 * min(per_cell_multi$share_multi),
            100 * max(per_cell_multi$share_multi)))
for (p in c("plot_leveling", "flood_irrigation")) {
  cat(sprintf("\n%s -- share of specifications attributing each function:\n", p))
  print(incidence[practice == p][order(-share),
                  .(f, share = sprintf("%.0f%%", 100 * share))])
}

# Functions served BEYOND the water/yield core, per practice -------------------

core2 <- rungs[["Water and yield"]]
skeleton <- unique(prac_cell[, .(cell, practice)])
noncore <- prac_long[!f %in% core2, .N, by = .(cell, practice)]
skeleton <- merge(skeleton, noncore, by = c("cell", "practice"), all.x = TRUE)
skeleton[is.na(N), N := 0L]
cat("\nFunctions beyond crop production and water conservation, per practice:\n")
print(skeleton[, .(median = median(N), lo = min(N), hi = max(N)),
               by = practice][order(-median)])

# Exact functional-ANOVA decomposition, per quantity ---------------------------

fac <- c("community", "domain", "family", "phrasing", "vote")
pop_var <- function(x) mean((x - mean(x))^2)
subsets <- unlist(lapply(seq_along(fac), function(k)
  combn(fac, k, simplify = FALSE)), recursive = FALSE)

partials_fun <- function(y) {
  d <- res[!is.na(get(y))]
  vy <- pop_var(d[[y]])
  partial <- list()
  for (s in subsets) {
    key <- paste(s, collapse = ":")
    cvar <- pop_var(d[, .(mu = mean(get(y))), by = s]$mu)
    lower <- Filter(function(t) all(t %in% s) && length(t) < length(s), subsets)
    partial[[key]] <- cvar - sum(vapply(lower, function(t)
      partial[[paste(t, collapse = ":")]], numeric(1)))
  }
  stopifnot(abs(sum(unlist(partial)) - vy) < 1e-9)
  lapply(partial, function(z) z / vy)
}
closed <- function(pl, group)
  sum(vapply(subsets, function(s)
    if (all(s %in% group)) pl[[paste(s, collapse = ":")]] else 0, numeric(1)))

quantities <- c("rung2", "fun_per_practice_med", "prac_per_function_med",
                "ind_ge2_vars")
idx_all <- rbindlist(lapply(quantities, function(y) {
  pl <- partials_fun(y)
  tot <- data.table(quantity = y, factor = fac)
  tot[, si := vapply(factor, function(f) pl[[f]], numeric(1))]
  tot[, ti := vapply(factor, function(f)
    sum(vapply(subsets, function(s)
      if (f %in% s) pl[[paste(s, collapse = ":")]] else 0, numeric(1))), numeric(1))]
  cc <- closed(pl, c("community", "domain"))
  cr <- closed(pl, c("family", "phrasing", "vote"))
  stopifnot(cc >= -1e-9, cr >= -1e-9)
  cat(sprintf("\n=== %s ===\ncorpus %.3f | reading %.3f | cross %.3f\n",
              y, cc, cr, 1 - cc - cr))
  print(tot[order(-ti), .(factor, Si = round(si, 3), Ti = round(ti, 3))])
  tot
}))
fwrite(idx_all, file.path(out, "sobol_ladder_bundles_totals.csv"))
