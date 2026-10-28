# HOW RELIABLE IS THE EVIDENCE GRADE? #########################################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "jsonlite"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

out <- here("datasets", "output")
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a

# Krippendorff's alpha with an ordinal metric ---------------------------------

kripp_ordinal_fun <- function(units, levels_ord) {
  units <- lapply(units, function(x) x[!is.na(x) & x != ""])
  units <- units[vapply(units, length, integer(1)) >= 2L]
  if (!length(units)) return(list(alpha = NA_real_, n_units = 0L))
  v <- levels_ord
  o <- matrix(0, length(v), length(v), dimnames = list(v, v))
  for (u in units) {
    m <- length(u)
    tab <- table(factor(u, levels = v))
    for (a in v) for (b in v) {
      o[a, b] <- o[a, b] + if (a == b) tab[a] * (tab[a] - 1) / (m - 1) else
        tab[a] * tab[b] / (m - 1)
    }
  }
  n_c <- rowSums(o)
  n <- sum(n_c)
  if (n < 2) return(list(alpha = NA_real_, n_units = length(units)))

  idx <- setNames(seq_along(v), v)
  d2 <- outer(v, v, Vectorize(function(a, b) {
    ia <- idx[[a]]; ib <- idx[[b]]
    if (ia == ib) return(0)
    lo <- min(ia, ib); hi <- max(ia, ib)
    (sum(n_c[lo:hi]) - (n_c[lo] + n_c[hi]) / 2)^2
  }))
  dimnames(d2) <- list(v, v)
  alpha <- 1 - sum(o * d2) / (sum(outer(n_c, n_c) * d2) / (n - 1))
  list(alpha = alpha, n_units = length(units), n_ratings = as.integer(n))
}

# Parse the per-draw extractions ----------------------------------------------

parse_one <- function(f, tag) {
  x <- as.data.table(readRDS(file.path(out, f)))
  rbindlist(lapply(seq_len(nrow(x)), function(i) {
    o <- tryCatch(fromJSON(x$text[i]), error = function(e) NULL)
    ff <- o$function_findings
    if (is.null(ff) || !length(ff)) return(NULL)
    ff <- as.data.table(ff)
    ff[, `:=`(rec_id = x$custom_id[i], draw = x$draw[i], model = tag)]
    ff[]
  }), fill = TRUE)
}

d <- rbind(parse_one("literature_extraction_raw_claude.rds", "claude"),
           parse_one("literature_extraction_raw_deepseek.rds", "deepseek"),
           fill = TRUE)
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

d <- d[direction == "delivered" & evidence_strength %in%
         c("weak", "moderate", "strong")]
ord <- c("weak", "moderate", "strong")

cat(sprintf("%d delivered findings, %d records\n\n", nrow(d), uniqueN(d$rec_id)))

# 1. Within-model stability across draws ---------------------------------------

within <- d[, .(n_draws = .N, n_distinct = uniqueN(evidence_strength)),
            by = .(model, rec_id, f)][n_draws >= 2]
cat("Stability of one model across repeated readings of the same paper:\n")
print(within[, .(units = .N,
                 all_draws_agree = sprintf("%.0f%%", 100 * mean(n_distinct == 1))),
             by = model])

for (m in c("claude", "deepseek")) {
  u <- split(d[model == m]$evidence_strength,
             d[model == m][, paste(rec_id, f)])
  u <- u[lengths(u) >= 2]
  a_o <- kripp_ordinal_fun(u, ord)
  a_n <- kripp_alpha_fun(u, metric = "nominal")
  cat(sprintf("  %-9s ordinal alpha %.2f | nominal alpha %.2f (%d units)\n",
              m, a_o$alpha, a_n$alpha, a_o$n_units))
}

# 2. Between-model agreement on the three-level scale ---------------------------

modal <- function(x) names(sort(table(x), decreasing = TRUE))[1]
per_model <- d[, .(g = modal(evidence_strength)), by = .(model, rec_id, f)]
wide <- dcast(per_model, rec_id + f ~ model, value.var = "g")
both <- wide[!is.na(claude) & !is.na(deepseek)]

cat(sprintf("\n\nBetween the two models, on %d paper-function pairs graded by both:\n",
            nrow(both)))
cat(sprintf("  exact agreement            %.0f%%\n",
            100 * mean(both$claude == both$deepseek)))
cat(sprintf("  agreement within one grade %.0f%%\n",
            100 * mean(abs(match(both$claude, ord) - match(both$deepseek, ord)) <= 1)))
cat(sprintf("  strong confused with weak  %.0f%% of pairs\n",
            100 * mean(abs(match(both$claude, ord) - match(both$deepseek, ord)) == 2)))

u2 <- lapply(seq_len(nrow(both)), function(i) c(both$claude[i], both$deepseek[i]))
a2_o <- kripp_ordinal_fun(u2, ord)
a2_n <- kripp_alpha_fun(u2, metric = "nominal")
cat(sprintf("  ordinal alpha              %.2f\n", a2_o$alpha))
cat(sprintf("  nominal alpha              %.2f\n", a2_n$alpha))

cat("\n  confusion matrix (rows Claude, columns DeepSeek):\n")
print(dcast(both[, .N, by = .(claude, deepseek)], claude ~ deepseek,
            value.var = "N", fill = 0))

# 3. The distinction the paper actually uses -----------------------------------

both[, `:=`(cl_strong = claude == "strong", ds_strong = deepseek == "strong")]
a3 <- kripp_alpha_fun(lapply(seq_len(nrow(both)), function(i)
  c(as.character(both$cl_strong[i]), as.character(both$ds_strong[i]))),
  metric = "nominal")
cat(sprintf("\n\nOn the strong / not-strong distinction the manuscript relies on:\n"))
cat(sprintf("  exact agreement %.0f%%\n", 100 * mean(both$cl_strong == both$ds_strong)))
cat(sprintf("  alpha           %.2f\n", a3$alpha))
cat(sprintf("  both call it strong in %d pairs, neither in %d, one of the two in %d\n",
            both[cl_strong & ds_strong, .N], both[!cl_strong & !ds_strong, .N],
            both[cl_strong != ds_strong, .N]))

rel <- data.table(
  measure = c("within_claude_ordinal", "within_deepseek_ordinal",
              "between_models_ordinal", "between_models_nominal",
              "between_models_strong_vs_not"),
  alpha = c(NA_real_, NA_real_, a2_o$alpha, a2_n$alpha, a3$alpha),
  n_units = c(NA_integer_, NA_integer_, nrow(both), nrow(both), nrow(both)))
for (m in c("claude", "deepseek")) {
  u <- split(d[model == m]$evidence_strength, d[model == m][, paste(rec_id, f)])
  u <- u[lengths(u) >= 2]
  rel[measure == sprintf("within_%s_ordinal", m),
      `:=`(alpha = kripp_ordinal_fun(u, ord)$alpha, n_units = length(u))]
}
fwrite(rel, file.path(out, "evidence_strength_reliability.csv"))
print(rel)
