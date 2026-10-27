# HOW MUCH DOES THE FUNCTION CODING DISCARD? ##################################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "readxl", "jsonlite", "reticulate"))

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

# Embedding, keyed to the corpus ----------------------------------------------

np <- import("numpy")
emb <- np$load(file.path(out, "emb_assumption.npy"))
emb_ids <- fread(file.path(out, "emb_ids.csv"))$id
meta <- as.data.table(read_xlsx(here("datasets", "inputs",
                                     "assumption_list_updated.xlsx")))
setnames(meta, tolower(names(meta)))
meta[, our_id := as.integer(our_id)]
ids <- meta[match(emb_ids, our_id), id_cite]

dt <- load_coded_corpus_fun()
keep <- ids %in% dt$id_cite & !is.na(ids)
emb <- emb[keep, , drop = FALSE]
ids <- ids[keep]
emb <- scale(emb, center = TRUE, scale = FALSE)
ss_tot <- sum(emb^2)

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

vote_rules <- c(`any draw` = 0.2, majority = 0.6, unanimous = 1.0)

# Variance explained by a grouping, and its permutation null ------------------

eta2_fun <- function(g) {
  gm <- rowsum(emb, g) / as.vector(table(g))
  sum(as.vector(table(g)) * rowSums(gm^2)) / ss_tot
}

n_perm <- 200L
eta2_null_fun <- function(g) {
  vapply(seq_len(n_perm), function(i) eta2_fun(sample(g)), numeric(1))
}

# Walk the 27 codings ---------------------------------------------------------

grid <- CJ(family = families, phrasing = phrasings, vote = names(vote_rules),
           sorted = FALSE)

res <- rbindlist(lapply(seq_len(nrow(grid)), function(i) {
  g <- grid[i]
  sub <- draws[family == g$family & phrasing == g$phrasing]
  nd <- sub[, uniqueN(draw), by = id_cite]
  cnt <- sub[, .(k = uniqueN(draw)), by = .(id_cite, f)]
  cnt <- merge(cnt, nd, by = "id_cite")
  kept <- cnt[k / V1 >= vote_rules[[g$vote]]]
  prof <- kept[, .(prof = paste(sort(f), collapse = "|")), by = id_cite]

  prof <- merge(data.table(id_cite = ids), prof, by = "id_cite", all.x = TRUE)
  prof[is.na(prof), prof := "(none)"]
  setkey(prof, id_cite)
  gvec <- prof[J(ids), prof]

  sizes <- prof[, .N, by = prof][order(-N)]
  obs <- eta2_fun(gvec)
  nul <- eta2_null_fun(gvec)
  data.table(family = g$family, phrasing = g$phrasing, vote = g$vote,
             n_profiles = nrow(sizes),
             largest_profile = sizes$N[1] / length(ids),
             singleton_profiles = sizes[N == 1, .N],
             eta2 = obs, eta2_null = mean(nul),
             eta2_excess = obs - mean(nul))
}))
fwrite(res, file.path(out, "coding_collapse.csv"))

# Report ----------------------------------------------------------------------

rng <- function(x, d = 2) sprintf(paste0("%.", d, "f (%.", d, "f-%.", d, "f)"),
                                  median(x), min(x), max(x))
rngi <- function(x) sprintf("%.0f (%.0f-%.0f)", median(x), min(x), max(x))

cat(sprintf("\n%d assumptions, %d codings of the function grid\n\n",
            length(ids), nrow(res)))
cat(sprintf("  distinct function profiles      %s\n", rngi(res$n_profiles)))
cat(sprintf("  share in the largest profile    %s\n",
            sprintf("%.0f%% (%.0f-%.0f%%)", 100 * median(res$largest_profile),
                    100 * min(res$largest_profile), 100 * max(res$largest_profile))))
cat(sprintf("  profiles holding one assumption %s\n", rngi(res$singleton_profiles)))

cat("\nShare of the embedding's variance explained by the function profile:\n\n")
cat(sprintf("  observed                        %s\n", rng(res$eta2)))
cat(sprintf("  expected from the group count   %s\n", rng(res$eta2_null)))
cat(sprintf("  excess over that null           %s\n", rng(res$eta2_excess)))
cat(sprintf("\n  observed exceeds null in        %d of %d codings\n",
            sum(res$eta2 > res$eta2_null), nrow(res)))
cat(sprintf("  variance NOT explained          %s\n",
            sprintf("%.0f%% (%.0f-%.0f%%)", 100 * (1 - median(res$eta2)),
                    100 * (1 - max(res$eta2)), 100 * (1 - min(res$eta2)))))

cat("\nBy LLM family:\n")
print(res[, .(profiles = as.numeric(median(n_profiles)),
              eta2 = round(median(eta2), 2),
              excess = round(median(eta2_excess), 2)), by = family])
cat("\nBy vote rule:\n")
print(res[, .(profiles = as.numeric(median(n_profiles)),
              eta2 = round(median(eta2), 2),
              excess = round(median(eta2_excess), 2)), by = vote])
