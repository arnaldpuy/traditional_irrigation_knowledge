# IS THE EMBEDDING'S DIMENSIONALITY LINGUISTIC OR SUBSTANTIVE? ################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "readxl", "reticulate"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

out <- here("datasets", "output")

# Embedding, keyed to the corpus metadata -------------------------------------

np <- import("numpy")
emb <- np$load(file.path(out, "emb_assumption.npy"))
emb_ids <- fread(file.path(out, "emb_ids.csv"))$id
meta <- as.data.table(read_xlsx(here("datasets", "inputs",
                                     "assumption_list_updated.xlsx")))
setnames(meta, tolower(names(meta)))
meta[, our_id := as.integer(our_id)]
ids <- meta[match(emb_ids, our_id), id_cite]

dt <- load_coded_corpus_fun()
dt[, domain_short := fcase(
  main_category == "Crop knowledge and management", "Crop",
  main_category == "Irrigation practices and communal water management", "Irrigation",
  main_category == "Soil and moisture management", "Soil & moisture",
  main_category == "Weather and climate conditions", "Weather & climate")]
cov <- dt[match(ids, id_cite),
          .(id_cite, site, domain_short,
            n_char = nchar(assumption),
            n_word = lengths(strsplit(assumption, "\\s+")))]
keep <- !is.na(cov$id_cite)
emb <- emb[keep, , drop = FALSE]
cov <- cov[keep]
cat(sprintf("embedding: %d assumptions x %d dimensions\n", nrow(emb), ncol(emb)))
cat(sprintf("assumption length: %d-%d words (median %d)\n",
            min(cov$n_word), max(cov$n_word), as.integer(median(cov$n_word))))

# Effective dimensionality --------------------------------------------------

eff_dim <- function(m) {
  m <- scale(m, center = TRUE, scale = FALSE)
  d <- svd(m)$d
  p <- d^2 / sum(d^2)
  c(pr = 1 / sum(p^2), shannon = exp(-sum(p * log(p + 1e-300))),
    k90 = which(cumsum(p) >= 0.90)[1])
}

# Variance removed by a nuisance model, and dimensionality of what remains ----

residualise <- function(form) {
  x <- model.matrix(form, data = cov)
  fit <- lm.fit(x, emb)
  list(res = fit$residuals,
       var_removed = 1 - sum(fit$residuals^2) /
         sum(scale(emb, center = TRUE, scale = FALSE)^2))
}

stages <- list(
  `none (raw embedding)` = ~ 1,
  `length` = ~ n_char + n_word,
  `length + community` = ~ n_char + n_word + site,
  `length + community + domain` = ~ n_char + n_word + site + domain_short)

res <- rbindlist(lapply(names(stages), function(nm) {
  if (nm == "none (raw embedding)") {
    e <- eff_dim(emb); vr <- 0
  } else {
    r <- residualise(stages[[nm]]); e <- eff_dim(r$res); vr <- r$var_removed
  }
  data.table(stage = nm, var_removed = vr, pr = e[["pr"]],
             shannon = e[["shannon"]], k90 = e[["k90"]])
}))
fwrite(res, file.path(out, "embedding_nuisance.csv"))

cat("\nEffective dimensionality after removing nuisance variation:\n\n")
print(res[, .(stage, `variance removed` = sprintf("%.1f%%", 100 * var_removed),
              `participation ratio` = sprintf("%.1f", pr),
              `Shannon eff. rank` = sprintf("%.0f", shannon),
              `components for 90%` = k90)])

base_pr <- res[stage == "none (raw embedding)", pr]
full_pr <- res[stage == "length + community + domain", pr]
cat(sprintf(paste0("\nRemoving length, community and knowledge domain removes %.0f%% of the\n",
                   "variance and changes the participation ratio from %.1f to %.1f (%+.0f%%).\n"),
            100 * res[stage == "length + community + domain", var_removed],
            base_pr, full_pr, 100 * (full_pr - base_pr) / base_pr))

# How much of the leading components is nuisance ------------------------------

pc <- prcomp(emb, center = TRUE, scale. = FALSE)$x[, 1:10]
r2 <- vapply(seq_len(ncol(pc)), function(j)
  summary(lm(pc[, j] ~ n_char + n_word + site + domain_short, data = cov))$r.squared,
  numeric(1))
cat("\nVariance of each leading component explained by length, community and domain:\n")
print(data.table(component = 1:10, r_squared = sprintf("%.2f", r2)))
cat(sprintf("\nmedian across the first ten components: %.2f\n", median(r2)))
