# GLOBAL UNCERTAINTY OF THE FUNCTION-BASED RESULTS ############################

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
core2 <- c("crop_production", "water_conservation")

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

# Per-draw function sets for all nine arms -------------------------------------

arm_file <- function(fam, ph) {
  if (fam == "anthropic" && ph == "standard")
    file.path(out, "coding_results_raw_analytical_anthropic.csv")
  else
    file.path(out, sprintf("coding_results_raw_attribution_%s_%s.csv", ph, fam))
}
read_draws <- function(fam, ph) {
  f <- arm_file(fam, ph)
  d <- as.data.table(utils::read.csv(f, stringsAsFactors = FALSE,
                                     encoding = "UTF-8"))
  d <- d[!duplicated(custom_id)]
  rbindlist(lapply(seq_len(nrow(d)), function(i) {
    o <- tryCatch(fromJSON(d$text[i]), error = function(e) NULL)
    fs <- if (is.null(o$functions)) character(0) else
      intersect(unlist(o$functions), funcs12)
    if (!length(fs)) return(NULL)
    data.table(family = fam, phrasing = ph,
               id_cite = sub("__s.*$", "", d$custom_id[i]),
               draw = d$custom_id[i], f = fs)
  }))
}
families <- c("anthropic", "openai", "deepseek")
phrasings <- c("strict", "standard", "broad")
draws <- rbindlist(lapply(families, function(fam)
  rbindlist(lapply(phrasings, function(ph) read_draws(fam, ph)))))
cat(sprintf("draws loaded: %d rows | %d arms | %d assumptions\n",
            nrow(draws), uniqueN(draws[, .(family, phrasing)]),
            uniqueN(draws$id_cite)))

# Vote rule: attribute a function named in >= t of an assumption's draws -------

n_draws <- draws[, .(nd = uniqueN(draw)), by = .(family, phrasing, id_cite)]
counts <- draws[, .(k = uniqueN(draw)), by = .(family, phrasing, id_cite, f)]
counts <- merge(counts, n_draws, by = c("family", "phrasing", "id_cite"))
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
  keep <- counts[family == g$family & phrasing == g$phrasing &
                   k / nd >= vote_rules[[g$vote]] &
                   (g$community == "none" | site != g$community) &
                   (g$domain == "none" | domain != g$domain)]
  if (!nrow(keep)) return(data.table(loss = NA_real_, k90 = NA_real_,
                                     core_recovers = NA_real_))
  fs <- split(keep$f, keep$id_cite)
  m <- dcast(unique(keep[, .(id_cite, f)]), id_cite ~ f,
             fun.aggregate = length, value.var = "f")
  mat <- as.matrix(m[, -1]); mat[mat > 0] <- 1

  s <- svd(mat)
  rec <- vapply(seq_len(ncol(mat)), function(k) {
    a <- s$u[, 1:k, drop = FALSE] %*% diag(s$d[1:k], k, k) %*%
      t(s$v[, 1:k, drop = FALSE])
    mean(rowSums(((a >= 0.5) * 1) != mat) == 0)
  }, numeric(1))
  fixed <- mat
  fixed[, setdiff(colnames(mat), core2)] <- 0
  data.table(loss = mean(vapply(fs, function(f) any(!f %in% core2), logical(1))),
             k90 = if (any(rec >= 0.9)) min(which(rec >= 0.9)) else NA_real_,
             core_recovers = mean(rowSums(fixed != mat) == 0))
}
res <- cbind(grid, rbindlist(lapply(seq_len(nrow(grid)), cell_stats)))
fwrite(res, file.path(out, "sobol_functions_grid.csv"))

# Exact functional-ANOVA decomposition, per output quantity --------------------

fac <- c("community", "domain", "family", "phrasing", "vote")
pop_var <- function(x) mean((x - mean(x))^2)
subsets <- unlist(lapply(seq_along(fac), function(k)
  combn(fac, k, simplify = FALSE)), recursive = FALSE)

decompose <- function(y) {
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
  ind <- data.table(term = names(partial), order = lengths(subsets),
                    si = unlist(partial) / vy)
  tot <- data.table(factor = fac)
  tot[, ti := vapply(factor, function(f)
    ind[grepl(f, term, fixed = TRUE), sum(si)], numeric(1))]
  tot[, si_first := ind[order == 1][match(factor, term), si]]
  list(indices = ind, totals = tot, var = vy,
       range = range(d[[y]]), median = median(d[[y]]))
}

quantities <- c(loss = "share with a function outside the efficiency core",
                core_recovers = "share fully recovered by the water/yield core",
                k90 = "functions needed to recover 90%")
all_tot <- rbindlist(lapply(names(quantities), function(y) {
  z <- decompose(y)
  cat(sprintf("\n=== %s ===\nrange %.3f-%.3f | median %.3f\n",
              quantities[[y]], z$range[1], z$range[2], z$median))
  print(z$totals[order(-ti), .(factor, Si = round(si_first, 3),
                               Ti = round(ti, 3))])
  cat("second-order terms above 0.02:\n")
  print(z$indices[order == 2 & si > 0.02][order(-si), .(term, si = round(si, 3))])
  fwrite(z$indices, file.path(out, sprintf("sobol_functions_indices_%s.csv", y)))
  cbind(quantity = y, z$totals)
}))
fwrite(all_tot, file.path(out, "sobol_functions_totals.csv"))

# Grouped indices: corpus composition versus reading of the corpus ------------

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
  lapply(partial, function(z) z / vy)
}
closed <- function(pl, group) {
  sum(vapply(subsets, function(s)
    if (all(s %in% group)) pl[[paste(s, collapse = ":")]] else 0, numeric(1)))
}
cat("\n=== corpus composition vs reading of the corpus (closed indices) ===\n")
for (y in names(quantities)) {
  pl <- partials_fun(y)
  cc <- closed(pl, c("community", "domain"))
  cr <- closed(pl, c("family", "phrasing", "vote"))
  stopifnot(cc >= -1e-9, cr >= -1e-9, cc + cr <= 1 + 1e-9)
  cat(sprintf("%-16s corpus %.3f | reading %.3f | cross %.3f\n",
              y, cc, cr, 1 - cc - cr))
}
