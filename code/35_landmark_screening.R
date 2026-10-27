# LANDMARK-PRIMARY SCREENING #################################################

# technology. GATED behind LANDMARK_RUN_ENABLED.

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "httr2", "jsonlite", "writexl"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# CONFIGURATION ###############################################################

run_enabled <- tolower(Sys.getenv("LANDMARK_RUN_ENABLED", "false")) == "true"
out_dir <- here("datasets", "output")
target_tech <- c("on_demand", "flow_metering")
top_n_screen <- 15L   # most-cited candidates per technology to screen
n_landmark <- 3L      # landmarks to keep per technology

# CANDIDATE SET ###############################################################

raw <- fread(file.path(out_dir, "literature_search_raw.csv"), encoding = "UTF-8")
raw[, doi_norm := tolower(trimws(doi))]
raw[, key := fifelse(!is.na(doi_norm) & doi_norm != "", doi_norm,
                     tolower(trimws(title)))]

setorder(raw, -cited_by)
prim <- unique(raw[technology %in% target_tech &
                   !grepl("review", subtype, ignore.case = TRUE)], by = "key")
cand <- prim[, .SD[order(-cited_by)][1:min(.N, top_n_screen)], by = technology]
cand[, rec_id := sprintf("LMK_%04d", .I)]
cat(sprintf("Landmark candidates to screen: %d (%s)\n", nrow(cand),
            paste(cand[, .N, by = technology][, sprintf("%s %d", technology, N)],
                  collapse = ", ")))

# Abstract enrichment (keyless OpenAlex) --------------------------------------

abs_dt <- fetch_openalex_abstracts_fun(cand[!is.na(doi_norm) & doi_norm != "", doi_norm])
cand <- merge(cand, abs_dt, by = "doi_norm", all.x = TRUE)
cand[, user_text := sprintf(
  "TITLE: %s\nSOURCE: %s (%s)\nCITATIONS: %s\nABSTRACT: %s",
  title, source_title, year, cited_by,
  fifelse(is.na(abstract) | abstract == "", "(not available)", abstract))]
fwrite(cand[, !c("user_text")], file.path(out_dir, "landmark_screening_queue.csv"))

# INSTRUMENT ##################################################################

landmark_prompt <- 'SYSTEM ROLE
You screen candidate LANDMARK PRIMARY studies for a corpus on the field effects
of two irrigation-modernisation technologies.

TASK
Decide whether a record is an on-topic primary study of EITHER:
  - on-demand / pressurised / automated collective irrigation networks, OR
  - irrigation flow metering, water metering, or telemetry of water delivery.
Include ONLY if ALL hold:
1. It is about irrigated agriculture / agricultural water management (REJECT
   medical, urban water, industrial, or hydrological-only records — the search
   strings catch off-topic "telemetry", "probe", "flow meter", "monitoring" hits).
2. It concerns the performance, effects, benefits, or trade-offs of one of the
   two technologies above.
3. It reports empirical field, farm, or network-scale outcomes (not purely
   conceptual or hardware-design papers).
4. Published 2000-2026, English or Spanish.

OUTPUT
Return ONLY a JSON object:
{"include": true|false,
 "technology": "on_demand"|"flow_metering"|"neither",
 "confidence": <0-1>, "reason": "<max 15 words>"}'

landmark_schema <- list(
  type = "object",
  properties = list(
    include = list(type = "boolean"),
    technology = list(type = "string",
                      enum = list("on_demand", "flow_metering", "neither")),
    confidence = list(type = "number"),
    reason = list(type = "string")),
  required = list("include", "technology", "confidence", "reason"),
  additionalProperties = FALSE)

# SCREEN ######################################################################

if (!run_enabled) {
  cat("\nGATED: LANDMARK_RUN_ENABLED != true - no billable API contacted.\n")
} else {
  cl <- run_claude_direct_fun(cand[, .(custom_id = rec_id, user_text)],
                              system_prompt = landmark_prompt,
                              schema = landmark_schema, max_tokens = 200L)
  ds <- run_deepseek_batch_fun(cand[, .(custom_id = rec_id, user_text)],
                               system_prompt = landmark_prompt, max_tokens = 200L)
  setnames(cl, c("custom_id", "text"), c("rec_id", "cl_text"), skip_absent = TRUE)
  setnames(ds, c("custom_id", "text"), c("rec_id", "ds_text"), skip_absent = TRUE)

  parse_inc <- function(txt) {
    v <- tryCatch(jsonlite::parse_json(txt), error = function(e) NULL)
    if (is.null(v)) return(NA)
    isTRUE(v$include)
  }
  votes <- merge(cand[, .(rec_id, technology, title, year, doi, cited_by)],
                 cl[, .(rec_id, cl_text)], by = "rec_id")
  votes <- merge(votes, ds[, .(rec_id, ds_text)], by = "rec_id")
  votes[, `:=`(cl_inc = vapply(cl_text, parse_inc, logical(1)),
               ds_inc = vapply(ds_text, parse_inc, logical(1)))]
  votes[, both_inc := cl_inc & ds_inc]
  fwrite(votes[, !c("cl_text", "ds_text")],
         file.path(out_dir, "landmark_screening_votes.csv"))

  # Keep the top n_landmark by citations among concordant includes -----------

  selected <- votes[both_inc == TRUE][order(technology, -cited_by),
                                      .SD[1:min(.N, n_landmark)], by = technology]
  fwrite(selected, file.path(out_dir, "landmark_selected.csv"))
  cat(sprintf("\nConcordant includes: %d ; landmarks selected: %d\n",
              votes[both_inc == TRUE, .N], nrow(selected)))
  print(selected[, .(technology, cited_by, year, title = substr(title, 1, 60))])

  # Append the selected landmarks to the manual retrieval residue -------------

  selected[, save_as := paste0(gsub("[^a-z0-9]+", "_", tolower(trimws(doi))), ".pdf")]
  selected[, doi_link := paste0("https://doi.org/", doi)]
  add <- selected[, .(rec_id, technology, year, title, doi, doi_link, save_as,
                      stratum = "landmark_primary", retrieved = "", note = "")]
  fwrite(add, file.path(out_dir, "landmark_retrieval_worklist.csv"))
  write_xlsx(add, file.path(out_dir, "landmark_retrieval_worklist.xlsx"))
  cat(sprintf("\nLandmark retrieval worklist (%d PDFs) -> landmark_retrieval_worklist.xlsx\n",
              nrow(add)))
}
