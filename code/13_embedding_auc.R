# HOW WELL DOES A LABEL-FREE AXIS REPRODUCE THE REDUCIBLE/IRREDUCIBLE SPLIT? ###

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "readxl", "jsonlite"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

out <- here("datasets", "output")
arm_dir <- file.path(out, "arm")
chk_dir <- file.path(out, "checkpoint")
set.seed(20260728)

# Corpus, embedding similarity and the removal-test draws ---------------------

dt <- load_coded_corpus_fun()
emb <- fread(file.path(out, "emb_efficiency_distance.csv"))
meta_ids <- as.data.table(read_xlsx(here("datasets", "inputs",
                                         "assumption_list_updated.xlsx")))
setnames(meta_ids, tolower(names(meta_ids)))
demb <- merge(emb, meta_ids[, .(id = as.integer(our_id), id_cite)], by = "id")
demb <- merge(demb, dt[, .(id_cite, site, assumption)], by = "id_cite")
demb[, n_char := nchar(assumption)]

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
                    demb[, .(id_cite, site, sim_centroid, n_char)], by = "id_cite")

vote_rules <- c(`any draw` = 0.2, majority = 0.6, unanimous = 1.0)
spec_grid <- CJ(vote = names(vote_rules), drop = c("none", unique(dt$site)),
                sorted = FALSE)

# The statistic ---------------------------------------------------------------

auc_fun <- function(score, pos) {
  if (!any(pos) || all(pos)) return(NA_real_)
  r <- rank(score)
  (sum(r[pos]) - sum(pos) * (sum(pos) + 1) / 2) / (sum(pos) * sum(!pos))
}

resid_len_fun <- function(sim, n_char) residuals(lm(sim ~ n_char))

n_perm <- 10000L
null_cache <- new.env(parent = emptyenv())
null_auc_fun <- function(n_pos, n_tot) {
  k <- sprintf("%d_%d", n_pos, n_tot)
  if (!is.null(null_cache[[k]])) return(null_cache[[k]])
  base <- seq_len(n_tot)
  d <- vapply(seq_len(n_perm), function(i) {
    idx <- sample.int(n_tot, n_pos)
    (sum(base[idx]) - n_pos * (n_pos + 1) / 2) / (n_pos * (n_tot - n_pos))
  }, numeric(1))
  null_cache[[k]] <- d
  d
}

# Across the 216 label specifications ------------------------------------------

res <- rbindlist(lapply(split(draw_share, by = c("prompt", "provider")), function(ds) {
  rbindlist(lapply(seq_len(nrow(spec_grid)), function(i) {
    sub <- ds[spec_grid$drop[i] == "none" | site != spec_grid$drop[i]]
    pos <- sub$s_irr < vote_rules[[spec_grid$vote[i]]]
    if (!any(pos) || all(pos)) return(NULL)
    a <- auc_fun(sub$sim_centroid, pos)
    a_len <- auc_fun(resid_len_fun(sub$sim_centroid, sub$n_char), pos)
    nd <- null_auc_fun(sum(pos), nrow(sub))
    data.table(prompt = ds$prompt[1], provider = ds$provider[1],
               vote = spec_grid$vote[i], drop = spec_grid$drop[i],
               n_reducible = sum(pos), n = nrow(sub),
               auc = a, auc_len_adjusted = a_len,
               p_perm = (sum(nd >= a) + 1) / (n_perm + 1),
               d_sim = mean(sub$sim_centroid[pos]) - mean(sub$sim_centroid[!pos]))
  }))
}))
fwrite(res, file.path(out, "embedding_auc.csv"))

# Report ----------------------------------------------------------------------

rng <- function(x) sprintf("%.2f (%.2f-%.2f)", median(x), min(x), max(x))

cat(sprintf("\n%d label specifications retained (of 216; a specification with no\n", nrow(res)))
cat("reducible assumption at all has no AUC to report).\n\n")
cat(sprintf("  AUC                        %s\n", rng(res$auc)))
cat(sprintf("  AUC, length-adjusted       %s\n", rng(res$auc_len_adjusted)))
cat(sprintf("  difference in similarity   %s\n", rng(res$d_sim)))
cat(sprintf("\n  AUC above 0.5 in            %d of %d specifications\n",
            sum(res$auc > 0.5), nrow(res)))
cat(sprintf("  permutation p below 0.05 in %d of %d\n",
            sum(res$p_perm < 0.05), nrow(res)))
cat(sprintf("  largest permutation p       %.4f\n", max(res$p_perm)))

# Where the witness fails, and why --------------------------------------------

res[, bin := cut(n_reducible, c(0, 10, 20, 40, 80, Inf),
                 labels = c("1-10", "11-20", "21-40", "41-80", ">80"))]
cat("\nAUC and power against the number of reducible assumptions:\n")
print(res[, .(specs = .N, median_auc = round(median(auc), 2),
              min_auc = round(min(auc), 2),
              significant = sprintf("%d/%d", sum(p_perm < 0.05), .N)),
          by = bin][order(bin)])

ok <- res[n_reducible >= 11]
cat(sprintf(paste0("\nRestricted to the %d specifications leaving at least eleven assumptions\n",
                   "reducible: AUC %.2f (%.2f-%.2f), every one above 0.5, every\n",
                   "permutation p below %.3f.\n"),
            nrow(ok), median(ok$auc), min(ok$auc), max(ok$auc), max(ok$p_perm)))

cat("\nBy LLM family of the removal test:\n")
print(res[, .(auc = sprintf("%.2f (%.2f-%.2f)", median(auc), min(auc), max(auc)),
              max_p = sprintf("%.4f", max(p_perm))), by = provider])
cat("\nBy wording of the removal test:\n")
print(res[, .(auc = sprintf("%.2f (%.2f-%.2f)", median(auc), min(auc), max(auc)),
              max_p = sprintf("%.4f", max(p_perm))), by = prompt])
