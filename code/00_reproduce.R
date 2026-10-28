# OFFLINE REPRODUCTION ########################################################

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "digest"))

code_dir <- here("code")
scripts <- list.files(code_dir, pattern = "^[0-9]{2}_.*\\.R$")
scripts <- scripts[scripts != "00_reproduce.R"]

offline <- c("05_intermodel_agreement.R", "06_sobol_headline.R", "07_sobol_functions.R",
             "08_ladder_bundles.R", "09_within_site_degeneracy.R",
             "10_compression_frontier.R", "11_severing.R", "14_embedding_auc.R",
             "15_embedding_nuisance.R", "16_coding_collapse.R", "17_planted_items.R",
             "18_subsampling_coverage.R", "19_paraphrase_retest.R",
             "20_language_conditions.R", "29_policy_irrigation_recode.R",
             "37_literature_extraction.R", "39_evidence_strength_matrix.R",
             "40_evidence_strength_reliability.R", "42_provenance_checks.R")
needs_external <- c(
  `01_parse_assumptions.py` = "the seven assumption-table PDFs (included) and pdfplumber",
  `12_embeddings.py` = "sentence-transformers and the model weights (download)",
  `13_embedding_efficiency_distance.py` = "same",
  `27_policy_linearize.py` = "the policy PDFs (public documents, not redistributed)",
  `31_quote_locations.R` = "the policy PDFs, for page numbers",
  `38_evidence_source_screen.R` = "the review full texts (copyrighted, not redistributed)",
  `41_link_extraction.R` = "the review full texts; archived replies are parsed if present")

stopifnot(all(offline %in% scripts))
Sys.unsetenv(c("CODING_RUN_ENABLED", "PLANTED_RUN_ENABLED", "PARAPHRASE_RUN_ENABLED",
               "LANGUAGE_RUN_ENABLED", "LITEXTRACT_RUN_ENABLED", "POLICY_RUN_ENABLED",
               "POLICY_RECODE_ENABLED", "LINK_RUN_ENABLED"))
before <- fread(file.path(here("datasets", "output"), "checksums.csv"))

for (s in offline) {
  cat(sprintf("\n==== %s ====\n", s))
  status <- system2("Rscript", shQuote(file.path(code_dir, s)), stdout = TRUE, stderr = TRUE)
  cat(tail(status, 8), sep = "\n")
  if (!is.null(attr(status, "status")) && attr(status, "status") != 0)
    stop(sprintf("%s failed with status %s", s, attr(status, "status")))
}

# CHECKSUMS ####################################################################

files <- list.files(here("datasets", "output"), recursive = TRUE)
files <- files[files != "checksums.csv"]
after <- data.table(file = files,
                    md5 = vapply(file.path(here("datasets", "output"), files),
                                 digest, character(1), algo = "md5", file = TRUE))
cmp <- merge(before, after, by = "file", all = TRUE, suffixes = c("_release", "_now"))
changed <- cmp[!is.na(md5_release) & !is.na(md5_now) & md5_release != md5_now, file]
cat(sprintf("\nOutputs compared: %d; identical: %d; changed: %d; new: %d; missing: %d\n",
            nrow(cmp), cmp[md5_release == md5_now, .N], length(changed),
            cmp[is.na(md5_release), .N], cmp[is.na(md5_now), .N]))
if (length(changed)) { cat("Changed files:\n"); cat(paste(" ", changed), sep = "\n") }

cat("\nSkipped (need files not distributed):\n")
for (nm in names(needs_external)) cat(sprintf("  %s: %s\n", nm, needs_external[[nm]]))

# NOTEBOOK #####################################################################

rmarkdown::render(file.path(code_dir, "traditional_irrigation_knowledge.Rmd"))
