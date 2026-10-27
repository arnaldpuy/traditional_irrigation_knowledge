# LITERATURE CORPUS SEARCH ####################################################

# GATED: set LITSEARCH_RUN_ENABLED=true plus SCOPUS_API_KEY / DIMENSIONS_API_KEY

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "httr2"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# CONFIGURATION ###############################################################

run_enabled <- tolower(Sys.getenv("LITSEARCH_RUN_ENABLED", "false")) == "true"
out_dir <- here("datasets", "output")
in_dir <- here("datasets", "inputs", "literature_corpus")
if (!dir.exists(in_dir)) dir.create(in_dir, recursive = TRUE)

tech_terms <- data.table(
  technology = c("drip", "soil_sensor", "remote_sensing", "on_demand",
                 "app_scheduling", "flow_metering"),
  terms = c(
    '"drip irrigation" OR "trickle irrigation" OR "localized irrigation" OR "micro-irrigation"',
    '"soil moisture sensor" OR "soil moisture probe" OR "capacitance probe" OR "tensiometer"',
    '"remote sensing" AND (irrigation OR "crop water")',
    '"on-demand irrigation" OR "pressurized irrigation network" OR "automated irrigation network"',
    '"irrigation scheduling" AND (app OR software OR "decision support")',
    '"water metering" OR "flow meter" OR telemetry AND irrigation'))

effect_block <- '(effect* OR impact* OR benefit* OR "trade-off*" OR performance)'

# Scopus query: TITLE-ABS-KEY + review doctype + irrigation anchor ------------

tech_terms[, scopus_query := sprintf(
  'TITLE-ABS-KEY((%s) AND %s AND irrigat*) AND DOCTYPE(re)', terms, effect_block)]

# Dimensions query: plain term block (type/review handled downstream) ---------

tech_terms[, dimensions_query := gsub('"', '\\\\"', sprintf(
  '(%s) AND %s AND irrigation', terms, gsub("[*]", "", effect_block)))]

# RUN #########################################################################

cat("Queries built for", nrow(tech_terms), "technologies\n\n")
print(tech_terms[, .(technology, scopus_query = substr(scopus_query, 1, 70))])

if (!run_enabled) {
  cat("\nGATED: LITSEARCH_RUN_ENABLED != true — no API contacted.\n")
} else {

  # Scopus + Dimensions per technology ----------------------------------------

  log_rows <- list()
  hits <- list()
  for (i in seq_len(nrow(tech_terms))) {
    tech <- tech_terms$technology[i]
    cat(sprintf("\n[%s] Scopus ... ", tech))
    sc <- tryCatch(search_scopus_fun(tech_terms$scopus_query[i]),
                   error = function(e) {message(e$message); data.table()})
    cat(sprintf("%d hits | Dimensions ... ", nrow(sc)))
    dm <- tryCatch(search_dimensions_fun(tech_terms$dimensions_query[i]),
                   error = function(e) {message(e$message); data.table()})
    cat(sprintf("%d hits", nrow(dm)))
    if (nrow(sc) > 0) sc[, technology := tech]
    if (nrow(dm) > 0) dm[, technology := tech]
    hits[[tech]] <- rbind(sc, dm, fill = TRUE)
    log_rows[[tech]] <- data.table(
      technology = tech, run_date = as.character(Sys.Date()),
      scopus_query = tech_terms$scopus_query[i], scopus_hits = nrow(sc),
      dimensions_query = tech_terms$dimensions_query[i], dimensions_hits = nrow(dm))
  }
  raw <- rbindlist(hits, fill = TRUE)
  fwrite(raw, file.path(out_dir, "literature_search_raw.csv"))
  fwrite(rbindlist(log_rows), file.path(out_dir, "literature_search_log.csv"))

  # Merge portal exports if present (WoS / CAB, RIS-to-CSV converted) ---------

  portal_files <- list.files(in_dir, pattern = "^(wos|cab)_.*\\.csv$",
                             full.names = TRUE)
  if (length(portal_files) > 0) {
    portal <- rbindlist(lapply(portal_files, fread), fill = TRUE)
    raw <- rbind(raw, portal, fill = TRUE)
    cat(sprintf("\nMerged %d portal-export files\n", length(portal_files)))
  }

  review <- raw[(database == "scopus" & subtype == "Review") |
                (database == "dimensions" & subtype == "REVIEW_ARTICLE") |
                (!database %in% c("scopus", "dimensions"))]

  # Deduplicate: DOI first, then normalised title -----------------------------

  review[, doi_norm := tolower(trimws(doi))]
  review[, title_norm := tolower(gsub("[^a-z0-9 ]", "", tolower(title)))]
  setorder(review, doi_norm, na.last = TRUE)
  dedup <- unique(review[!is.na(doi_norm) & doi_norm != ""], by = "doi_norm")
  no_doi <- review[is.na(doi_norm) | doi_norm == ""]
  no_doi <- no_doi[!title_norm %in% dedup$title_norm]
  dedup <- rbind(dedup, unique(no_doi, by = "title_norm"))
  fwrite(dedup, file.path(out_dir, "literature_screening_queue.csv"))
  cat(sprintf("\n%d raw -> %d review stratum -> %d after dedup -> literature_screening_queue.csv\n",
              nrow(raw), nrow(review), nrow(dedup)))
  cat("Full raw retained in literature_search_raw.csv for landmark selection.\n")
}
