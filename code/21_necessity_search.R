# NECESSITY-PREMISE CORPUS SEARCH #############################################

# GATED: set NECSEARCH_RUN_ENABLED=true plus SCOPUS_API_KEY / DIMENSIONS_API_KEY

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "httr2"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# CONFIGURATION ###############################################################

run_enabled <- tolower(Sys.getenv("NECSEARCH_RUN_ENABLED", "false")) == "true"
out_dir <- here("datasets", "output")
in_dir <- here("datasets", "inputs", "necessity_corpus")
if (!dir.exists(in_dir)) dir.create(in_dir, recursive = TRUE)

s1_block <- paste0(
  '"irrigation modernization" OR "irrigation modernisation" OR ',
  '"modernization of irrigation" OR "modernisation of irrigation" OR ',
  '"modernized irrigation" OR "modernised irrigation" OR ',
  '"irrigation rehabilitation" OR "irrigation upgrading"')

tech_block <- paste0(
  '"drip irrigation" OR "pressurized irrigation" OR "pressurised irrigation" OR ',
  '"precision irrigation" OR "smart irrigation" OR "digital irrigation" OR ',
  '"remote sensing" OR "soil moisture sensor*" OR "irrigation technolog*" OR ',
  'moderniz* OR modernis*')

necessity_block <- paste0(
  'necessary OR essential OR imperative OR indispensable OR unavoidable OR ',
  'require* OR "depends on" OR precondition OR "only way" OR "key to" OR must')

stake_block <- paste0(
  'sustainab* OR viab* OR surviv* OR future OR "climate change" OR ',
  'adaptation OR "water scarcity" OR "food security" OR competitiv*')

strata <- data.table(
  stratum = c("S1_core", "S2_broad"),
  scopus_query = c(
    sprintf('TITLE-ABS-KEY(%s)', s1_block),
    sprintf('TITLE-ABS-KEY(irrigat* AND (%s) AND (%s) AND (%s))',
            tech_block, necessity_block, stake_block)),
  dimensions_query = c(
    gsub('"', '\\\\"', s1_block),
    gsub('"', '\\\\"', sprintf('irrigation AND (%s) AND (%s) AND (%s)',
                               gsub("[*]", "", tech_block),
                               gsub("[*]", "", necessity_block),
                               gsub("[*]", "", stake_block)))))

# RUN #########################################################################

cat("Queries built for", nrow(strata), "strata\n\n")
print(strata[, .(stratum, scopus_query = substr(scopus_query, 1, 70))])

if (!run_enabled) {
  cat("\nGATED: NECSEARCH_RUN_ENABLED != true — no API contacted.\n")
} else {

  # Scopus + Dimensions per stratum -------------------------------------------

  log_rows <- list()
  hits <- list()
  for (i in seq_len(nrow(strata))) {
    st <- strata$stratum[i]
    cat(sprintf("\n[%s] Scopus ... ", st))
    sc <- tryCatch(search_scopus_fun(strata$scopus_query[i], max_records = 5000),
                   error = function(e) {message(e$message); data.table()})
    cat(sprintf("%d hits | Dimensions ... ", nrow(sc)))
    dm <- tryCatch(search_dimensions_fun(strata$dimensions_query[i]),
                   error = function(e) {message(e$message); data.table()})
    cat(sprintf("%d hits", nrow(dm)))
    if (nrow(sc) > 0) sc[, stratum := st]
    if (nrow(dm) > 0) dm[, stratum := st]
    hits[[st]] <- rbind(sc, dm, fill = TRUE)
    log_rows[[st]] <- data.table(
      stratum = st, run_date = as.character(Sys.Date()),
      scopus_query = strata$scopus_query[i], scopus_hits = nrow(sc),
      dimensions_query = strata$dimensions_query[i], dimensions_hits = nrow(dm))
  }
  raw <- rbindlist(hits, fill = TRUE)
  fwrite(raw, file.path(out_dir, "necessity_search_raw.csv"))
  fwrite(rbindlist(log_rows), file.path(out_dir, "necessity_search_log.csv"))

  # Deduplicate: DOI first, then normalised title; S1 wins over S2 ------------

  raw[, doi_norm := tolower(trimws(doi))]
  raw[, title_norm := tolower(gsub("[^a-z0-9 ]", "", tolower(title)))]
  setorder(raw, stratum, doi_norm, na.last = TRUE)
  dedup <- unique(raw[!is.na(doi_norm) & doi_norm != ""], by = "doi_norm")
  no_doi <- raw[is.na(doi_norm) | doi_norm == ""]
  no_doi <- no_doi[!title_norm %in% dedup$title_norm]
  dedup <- rbind(dedup, unique(no_doi, by = "title_norm"))
  fwrite(dedup, file.path(out_dir, "necessity_screening_queue.csv"))
  cat(sprintf("\n%d raw -> %d after dedup -> necessity_screening_queue.csv\n",
              nrow(raw), nrow(dedup)))
}
