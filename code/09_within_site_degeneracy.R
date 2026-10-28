# IS THE DEGENERACY REAL WITHIN A COMMUNITY, OR ONLY AFTER POOLING? ###########

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "jsonlite"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

out <- here("datasets", "output")
set.seed(20260728)

funcs12 <- c("crop_production", "water_conservation", "soil_health",
             "risk_management", "groundwater_recharge",
             "microclimate_regulation", "biodiversity", "pest_control",
             "livestock", "medicinal_food", "social_cohesion",
             "recreation_wellbeing")

canon12 <- c("flood_irrigation", "sky_reading", "open_channel", "tillage",
             "turn_exchange", "weed_tolerance", "mulching", "ridge_furrow",
             "plot_leveling", "crop_rotation", "cover_crop", "channel_cleaning")

# Per-draw practice and function sets under the nine attribution arms ---------

read_arm <- function(f) as.data.table(utils::read.csv(f, stringsAsFactors = FALSE,
                                                      encoding = "UTF-8"))
arm_file <- function(fam, ph) {
  if (fam == "anthropic" && ph == "standard")
    file.path(out, "coding_results_raw_analytical_anthropic.csv")
  else file.path(out, sprintf("coding_results_raw_attribution_%s_%s.csv", ph, fam))
}
families <- c("anthropic", "openai", "deepseek")
phrasings <- c("strict", "standard", "broad")
first_chr <- function(v) if (is.null(v) || !length(v)) NA_character_ else
  as.character(v)[1]

parsed_arms <- rbindlist(lapply(families, function(fam)
  rbindlist(lapply(phrasings, function(ph) {
    d <- read_arm(arm_file(fam, ph))[!duplicated(custom_id)]
    rbindlist(lapply(seq_len(nrow(d)), function(i) {
      o <- tryCatch(fromJSON(d$text[i]), error = function(e) NULL)
      if (is.null(o)) return(NULL)
      fs <- intersect(unlist(o$functions), funcs12)
      if (!length(fs)) return(NULL)
      data.table(family = fam, phrasing = ph,
                 id_cite = sub("__s.*$", "", d$custom_id[i]),
                 draw = d$custom_id[i], practice = first_chr(o$practice), f = fs)
    }))
  }))))

meta <- load_coded_corpus_fun()
parsed_arms <- merge(parsed_arms, meta[, .(id_cite, site)], by = "id_cite")

vote_rules <- c(`any draw` = 0.2, majority = 0.6, unanimous = 1.0)

nd <- parsed_arms[, .(nd = uniqueN(draw)), by = .(family, phrasing, id_cite)]
fcnt <- parsed_arms[, .(k = uniqueN(draw)), by = .(family, phrasing, id_cite, f)]
fcnt <- merge(fcnt, nd, by = c("family", "phrasing", "id_cite"))

modal_practice <- parsed_arms[, .(n = uniqueN(draw)),
                              by = .(family, phrasing, id_cite, practice)][
  order(-n), .SD[1], by = .(family, phrasing, id_cite)][, .(family, phrasing,
                                                            id_cite, practice)]

# Counting degeneracy ---------------------------------------------------------

degen_fun <- function(x) {
  pf <- unique(x[, .(f, practice)])
  per_f <- pf[, .(np = .N), by = f]
  list(n_functions = nrow(per_f), n_degenerate = per_f[np >= 2, .N],
       med_practices = as.numeric(median(per_f$np)))
}

n_perm <- 200L

codings <- CJ(family = families, phrasing = phrasings,
              vote = names(vote_rules), sorted = FALSE)
sites <- sort(unique(meta$site))

res <- rbindlist(lapply(seq_len(nrow(codings)), function(i) {
  g <- codings[i]
  keep <- fcnt[family == g$family & phrasing == g$phrasing &
                 k / nd >= vote_rules[[g$vote]], .(id_cite, f)]
  keep <- merge(keep, modal_practice[family == g$family & phrasing == g$phrasing,
                                     .(id_cite, practice)], by = "id_cite")
  keep <- merge(keep, meta[, .(id_cite, site)], by = "id_cite")
  named <- keep[!is.na(practice) & practice != "none"]
  retained <- uniqueN(named[practice %in% canon12]$id_cite) / uniqueN(named$id_cite)
  keep <- keep[practice %in% canon12]

  pooled <- degen_fun(keep)

  per_site <- rbindlist(lapply(sites, function(s) {
    x <- keep[site == s]
    if (!nrow(x)) return(NULL)
    obs <- degen_fun(x)

    ap <- unique(x[, .(id_cite, practice)])
    nul <- vapply(seq_len(n_perm), function(b) {
      shuffled <- data.table(id_cite = ap$id_cite,
                             practice = sample(ap$practice))
      y <- merge(x[, .(id_cite, f)], shuffled, by = "id_cite")
      degen_fun(y)$n_degenerate
    }, numeric(1))

    data.table(family = g$family, phrasing = g$phrasing, vote = g$vote,
               site = s, n_assumptions = uniqueN(x$id_cite),
               n_practices = uniqueN(x$practice),
               n_functions = obs$n_functions, n_degenerate = obs$n_degenerate,
               med_practices = obs$med_practices,
               null_degenerate = mean(nul), retained = retained)
  }))
  per_site[, `:=`(pooled_functions = pooled$n_functions,
                  pooled_degenerate = pooled$n_degenerate)]
  per_site[]
}))
fwrite(res, file.path(out, "within_site_degeneracy.csv"))

# Report ----------------------------------------------------------------------

rng <- function(x) sprintf("%.0f (%.0f-%.0f)", median(x), min(x), max(x))

cat(sprintf("\n%d codings x %d communities = %d combinations\n",
            nrow(codings), length(sites), nrow(res)))
cat(sprintf("Assumptions retained by the twelve-practice vocabulary: %.0f%% (%.0f-%.0f%%)\n\n",
            100 * median(res$retained), 100 * min(res$retained),
            100 * max(res$retained)))

cat("Pooled across the seven communities:\n")
cat(sprintf("  functions served                 %s of twelve\n",
            rng(res$pooled_functions)))
cat(sprintf("  carried by two or more practices %s\n", rng(res$pooled_degenerate)))

cat("\nWithin a single community:\n")
cat(sprintf("  assumptions                      %s\n", rng(res$n_assumptions)))
cat(sprintf("  practices in use                 %s\n", rng(res$n_practices)))
cat(sprintf("  functions served                 %s\n", rng(res$n_functions)))
cat(sprintf("  carried by two or more practices %s\n", rng(res$n_degenerate)))
cat(sprintf("  as a share of functions served   %.0f%% (%.0f-%.0f%%)\n",
            100 * median(res$n_degenerate / res$n_functions),
            100 * min(res$n_degenerate / res$n_functions),
            100 * max(res$n_degenerate / res$n_functions)))
cat(sprintf("  expected if practices and functions were paired at random %s\n",
            rng(res$null_degenerate)))

cat(sprintf("\n  combinations where at least half the functions served are\n"))
cat(sprintf("  carried by two or more practices: %d of %d (%.0f%%)\n",
            res[n_degenerate / n_functions >= 0.5, .N], nrow(res),
            100 * res[n_degenerate / n_functions >= 0.5, .N] / nrow(res)))

cat("\nBy community (median across the 27 codings):\n")
print(res[, .(assumptions = as.numeric(median(n_assumptions)),
              practices = as.numeric(median(n_practices)),
              functions = as.numeric(median(n_functions)),
              degenerate = as.numeric(median(n_degenerate)),
              null = round(median(null_degenerate), 1)),
          by = site][order(-degenerate)])
