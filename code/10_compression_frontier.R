# THE COMPRESSION FRONTIER ####################################################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "jsonlite"))

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
fun_lab <- c(crop_production = "Crop production",
             water_conservation = "Water conservation",
             soil_health = "Soil health", risk_management = "Risk management",
             social_cohesion = "Social cohesion", biodiversity = "Biodiversity",
             groundwater_recharge = "Groundwater recharge",
             microclimate_regulation = "Microclimate regulation",
             pest_control = "Pest control", livestock = "Livestock",
             medicinal_food = "Medicinal/food plants",
             recreation_wellbeing = "Recreation/wellbeing")

# Per-draw function sets under the nine attribution arms ----------------------

read_arm <- function(f) as.data.table(utils::read.csv(f, stringsAsFactors = FALSE,
                                                      encoding = "UTF-8"))
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
                 id_cite = sub("__s.*$", "", d$custom_id[i]),
                 draw = d$custom_id[i], f = fs)
    }))
  }))))

meta <- load_coded_corpus_fun()
meta[, domain := fcase(
  main_category == "Crop knowledge and management", "Crop",
  main_category == "Irrigation practices and communal water management", "Irrigation",
  main_category == "Soil and moisture management", "Soil & moisture",
  main_category == "Weather and climate conditions", "Weather & climate")]
draws <- merge(draws, meta[, .(id_cite, site, domain)], by = "id_cite")

vote_rules <- c(`any draw` = 0.2, majority = 0.6, unanimous = 1.0)
nd <- draws[, .(nd = uniqueN(draw)), by = .(family, phrasing, id_cite)]
counts <- draws[, .(k = uniqueN(draw)), by = .(family, phrasing, id_cite, f, site, domain)]
counts <- merge(counts, nd, by = c("family", "phrasing", "id_cite"))

# Subset machinery ------------------------------------------------------------

n_sub <- 2L^12L
bits <- bitwShiftL(1L, 0:11)
popcnt <- vapply(0:(n_sub - 1L), function(s) sum(bitwAnd(s, bits) != 0L), integer(1))
sub_by_k <- split(0:(n_sub - 1L), popcnt)

all_sub <- 0:(n_sub - 1L)
has_bit <- lapply(bits, function(b) all_sub[bitwAnd(all_sub, b) != 0L])

zeta_fun <- function(cnt) {
  f <- cnt
  for (i in seq_along(bits)) {
    s <- has_bit[[i]]
    f[s + 1L] <- f[s + 1L] + f[bitwXor(s, bits[i]) + 1L]
  }
  f
}

mask_of <- function(fs) sum(bits[match(fs, funcs12)])
core_mask <- mask_of(core2)

# Walk the grid ---------------------------------------------------------------

grid <- CJ(community = c("none", unique(meta$site)),
           domain = c("none", unique(meta$domain)),
           family = families, phrasing = phrasings,
           vote = names(vote_rules), sorted = FALSE)
cat(sprintf("grid: %d cells\n", nrow(grid)))

cell_fun <- function(i) {
  g <- grid[i]
  keep <- counts[family == g$family & phrasing == g$phrasing &
                   k / nd >= vote_rules[[g$vote]] &
                   (g$community == "none" | site != g$community) &
                   (g$domain == "none" | domain != g$domain)]
  if (!nrow(keep)) return(NULL)
  masks <- keep[, .(m = mask_of(unique(f))), by = id_cite]
  n <- nrow(masks)
  cnt <- numeric(n_sub)
  tb <- masks[, .N, by = m]
  cnt[tb$m + 1L] <- tb$N
  f <- zeta_fun(cnt)

  best <- rbindlist(lapply(1:12, function(k) {
    s <- sub_by_k[[as.character(k)]]
    v <- f[s + 1L]
    j <- which.max(v)
    data.table(k = k, best_share = v[j] / n, best_set = s[j])
  }))
  best[, `:=`(cell = i, community = g$community, domain = g$domain,
              family = g$family, phrasing = g$phrasing, vote = g$vote,
              n = n, core_share = f[core_mask + 1L] / n)]
  best[]
}

res <- rbindlist(lapply(seq_len(nrow(grid)), cell_fun))
res[, best_names := vapply(best_set, function(s)
  paste(sort(funcs12[bitwAnd(s, bits) != 0L]), collapse = "|"), character(1))]
fwrite(res, file.path(out, "compression_frontier.csv"))

# Report ----------------------------------------------------------------------

pc <- function(x) sprintf("%.0f%% (%.0f-%.0f)", 100 * median(x), 100 * min(x),
                          100 * max(x))

cat(sprintf("\n%d cells\n\nBest achievable recovery at each width k:\n\n", uniqueN(res$cell)))
print(res[, .(`best k functions` = pc(best_share)), by = k][order(k)])

cat("\nThe conventional pair against the best pair:\n\n")
k2 <- res[k == 2]
cat(sprintf("  crop production + water conservation  %s\n", pc(k2$core_share)))
cat(sprintf("  best possible pair                    %s\n", pc(k2$best_share)))
cat(sprintf("  cells where the best pair IS the conventional pair: %d of %d (%.0f%%)\n",
            k2[best_set == core_mask, .N], nrow(k2),
            100 * k2[best_set == core_mask, .N] / nrow(k2)))
cat("\n  when it is not, the best pair is:\n")
alt <- k2[best_set != core_mask, .N, by = best_names][order(-N)]
if (nrow(alt)) print(head(alt, 5)) else cat("    (never)\n")

cat("\nWidth needed to reach a given recovery (median across cells):\n\n")
for (target in c(0.5, 0.75, 0.9, 0.95)) {
  kk <- res[, .(k = if (any(best_share >= target)) min(k[best_share >= target])
                else NA_integer_), by = cell]$k
  cat(sprintf("  %2.0f%% recovery needs %s of the twelve functions\n", 100 * target,
              if (all(is.na(kk))) "more than twelve" else
                sprintf("%.0f (%.0f-%.0f)", median(kk, na.rm = TRUE),
                        min(kk, na.rm = TRUE), max(kk, na.rm = TRUE))))
}

cat("\nWhich functions enter the best set, by width:\n\n")
for (width in c(2, 3, 4, 6)) {
  tb <- res[k == width, .N, by = best_names][order(-N)]
  cat(sprintf("  k = %d, most frequent best set (%d of %d cells):\n    %s\n", width,
              tb$N[1], sum(tb$N),
              paste(fun_lab[strsplit(tb$best_names[1], "|", fixed = TRUE)[[1]]],
                    collapse = ", ")))
}
