# WHAT IF THE NOTEBOOK MISSED ASSUMPTIONS? ####################################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "readxl", "jsonlite"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

out <- here("datasets", "output")
arm_dir <- file.path(out, "arm"); chk_dir <- file.path(out, "checkpoint")
set.seed(20260730)

n_rep <- as.integer(Sys.getenv("SUBSAMPLE_REPS", "200"))
keep_fracs <- c(1, 0.9, 0.75, 0.5, 0.25)
vote_rules <- c(`any draw` = 0.2, majority = 0.6, unanimous = 1.0)

# THE DRAWS ###################################################################

dt <- load_coded_corpus_fun()
dt[, domain_short := fcase(
  main_category == "Crop knowledge and management", "Crop",
  main_category == "Irrigation practices and communal water management", "Irrigation",
  main_category == "Soil and moisture management", "Soil & moisture",
  main_category == "Weather and climate conditions", "Weather & climate")]

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

draw_share <- bin[, .(s_irr = mean(!legible)), by = .(prompt, provider, id_cite)]
draw_share <- merge(draw_share, dt[, .(id_cite, site, domain_short)], by = "id_cite")

# Per-assumption verdicts, one column per vote rule -----------------------------

flags <- rbindlist(lapply(names(vote_rules), function(v)
  draw_share[, .(vote = v, flag = as.integer(s_irr >= vote_rules[[v]])),
             by = .(prompt, provider, id_cite, site, domain_short)]))

sites <- sort(unique(dt$site)); domains <- sort(unique(dt$domain_short))
ids <- unique(draw_share$id_cite)
cat(sprintf("assumptions %d | sites %d | domains %d | grid cells %d\n",
            length(ids), length(sites), length(domains),
            (length(sites) + 1L) * (length(domains) + 1L) * 3L * 3L * 3L))

# THE GRID, RECOMPUTED ON A SUBSET ############################################

grid_on <- function(keep_ids) {
  f <- flags[id_cite %in% keep_ids]
  tab <- f[, .(s = sum(flag), n = .N),
           by = .(prompt, provider, vote, site, domain_short)]
  tab[, .(share = {
    m_s <- matrix(0, length(sites), length(domains),
                  dimnames = list(sites, domains))
    m_n <- m_s
    m_s[cbind(site, domain_short)] <- s
    m_n[cbind(site, domain_short)] <- n
    tot_s <- sum(m_s); tot_n <- sum(m_n)
    rs <- rowSums(m_s); rn <- rowSums(m_n)
    cs <- colSums(m_s); cn <- colSums(m_n)
    cell <- CJ(community = c("none", sites), domain = c("none", domains),
               sorted = FALSE)
    drop_c <- cell$community != "none"; drop_d <- cell$domain != "none"

    add_s <- add_n <- numeric(nrow(cell))
    if (any(drop_c & drop_d)) {
      j <- which(drop_c & drop_d)
      idx <- cbind(cell$community[j], cell$domain[j])
      add_s[j] <- m_s[idx]; add_n[j] <- m_n[idx]
    }
    num <- tot_s - fifelse(drop_c, rs[cell$community], 0) -
      fifelse(drop_d, cs[cell$domain], 0) + add_s
    den <- tot_n - fifelse(drop_c, rn[cell$community], 0) -
      fifelse(drop_d, cn[cell$domain], 0) + add_n
    num / den
  }), by = .(prompt, provider, vote)]
}

full <- grid_on(ids)
cat(sprintf("full corpus: %d cells | median %.3f | floor %.3f\n",
            nrow(full), median(full$share), min(full$share)))

published <- fread(file.path(out, "sobol_headline_grid.csv"))
stopifnot(nrow(full) == nrow(published),
          abs(median(full$share) - median(published$share)) < 1e-9,
          abs(min(full$share) - min(published$share)) < 1e-9)
cat("reproduces sobol_headline_grid.csv exactly\n")

# RANDOM OMISSION #############################################################

curve <- rbindlist(lapply(keep_fracs, function(k) {
  reps <- if (k == 1) 1L else n_rep
  rbindlist(lapply(seq_len(reps), function(r) {
    g <- grid_on(sample(ids, round(k * length(ids))))
    data.table(keep = k, rep = r, median_share = median(g$share),
               floor_share = min(g$share), above_70 = mean(g$share >= 0.70))
  }))
}))
fwrite(curve, file.path(out, "subsampling_curve.csv"))

cat("\n=== Random omission: the grid recomputed on a thinned corpus ===\n")
cat("keep   n     median share (2.5-97.5%)      floor of the grid       cells >= 70%\n")
for (k in keep_fracs) {
  z <- curve[keep == k]
  cat(sprintf("%3.0f%%  %3d   %.3f (%.3f-%.3f)   %.3f (%.3f-%.3f)   %.1f%%\n",
              100 * k, round(k * length(ids)), median(z$median_share),
              quantile(z$median_share, 0.025), quantile(z$median_share, 0.975),
              median(z$floor_share), quantile(z$floor_share, 0.025),
              quantile(z$floor_share, 0.975), 100 * median(z$above_70)))
}

# ADVERSARIAL OMISSION ########################################################

tip <- full[, .(prompt, provider, vote, share)]
n_cell <- flags[, .(n = uniqueN(id_cite)), by = .(prompt, provider, vote)]
tip <- merge(tip, n_cell, by = c("prompt", "provider", "vote"))
for (target in c(0.70, 0.50))
  tip[, sprintf("k_%02.0f", 100 * target) :=
        pmax(0, round(n * (share - target) / target))]

cat("\n=== Adversarial omission: missed assumptions needed to reach a target ===\n")
cat("(all of them reducible; k as a count and as a share of the recorded corpus)\n")
for (target in c(0.70, 0.50)) {
  kk <- tip[[sprintf("k_%02.0f", 100 * target)]]
  cat(sprintf("to bring a cell to %2.0f%%: median %3.0f assumptions (%3.0f-%3.0f), ",
              100 * target, median(kk), min(kk), max(kk)))
  cat(sprintf("%.0f%% of the corpus (%.0f-%.0f%%)\n",
              100 * median(kk / tip$n), 100 * min(kk / tip$n),
              100 * max(kk / tip$n)))
}
cat(sprintf("\ncells already at or below 70%%: %d of %d\n",
            sum(full$share <= 0.70), nrow(full)))
fwrite(tip, file.path(out, "subsampling_tipping_point.csv"))

cat("\nWritten: subsampling_curve.csv, subsampling_tipping_point.csv\n")
