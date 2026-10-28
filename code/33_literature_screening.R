# LITERATURE SCREENING ########################################################

#      adjudication queue. GATED: set LITSCREEN_RUN_ENABLED=true to call the

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "httr2", "jsonlite"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# CONFIGURATION ###############################################################

run_enabled <- tolower(Sys.getenv("LITSCREEN_RUN_ENABLED", "false")) == "true"
out_dir <- here("datasets", "output")
queue_file <- file.path(out_dir, "literature_screening_queue.csv")
queue <- fread(queue_file, encoding = "UTF-8")

# STEP 1: ABSTRACT ENRICHMENT #################################################

if (!"abstract" %in% names(queue)) {
  cat("Enriching queue with OpenAlex abstracts ...\n")
  abs_dt <- fetch_openalex_abstracts_fun(queue[!is.na(doi_norm) & doi_norm != "", doi_norm])
  queue <- merge(queue, abs_dt, by = "doi_norm", all.x = TRUE)
  fwrite(queue, queue_file)
}
cat(sprintf("Queue: %d records, %d with abstract (%.0f%%)\n",
            nrow(queue), sum(!is.na(queue$abstract) & queue$abstract != ""),
            100 * mean(!is.na(queue$abstract) & queue$abstract != "")))

# STEP 2: DUAL-MODEL SCREENING ################################################

screening_prompt <- 'SYSTEM ROLE
You screen bibliographic records for a systematic, review-level corpus on the
effects of irrigation modernization technologies.

TASK
Given a record (title, source, year, and abstract when available), decide
whether it enters the corpus. Include ONLY if ALL criteria hold:
1. The record is a review or meta-analysis (narrative reviews qualify), not a
   primary study, conference abstract, correction, editorial, or book review.
2. It concerns the field performance, effects, impacts, benefits, or
   trade-offs of one or more of these technologies in irrigated agriculture:
   drip/micro-irrigation; soil moisture sensors/probes; remote sensing for
   irrigation management; on-demand or pressurized collective networks;
   irrigation scheduling applications or decision-support systems; flow
   metering/telemetry.
3. It reports outcomes attributable to the technology (reviews of purely
   hypothetical modelling without field evidence are excluded).
4. Published 2000-2026, in English or Spanish.
If the abstract is missing, judge from the title and source; mark confidence
accordingly.

OUTPUT
Return ONLY a JSON object:
{"include": true|false, "technologies": ["<zero or more of: drip, soil_sensor,
remote_sensing, on_demand, app_scheduling, flow_metering>"],
"confidence": <0-1>, "reason": "<max 15 words>"}'

screening_schema <- list(
  type = "object",
  properties = list(
    include = list(type = "boolean"),
    technologies = list(type = "array",
                        items = list(type = "string",
                                     enum = list("drip", "soil_sensor",
                                                 "remote_sensing", "on_demand",
                                                 "app_scheduling", "flow_metering"))),
    confidence = list(type = "number"),
    reason = list(type = "string")),
  required = list("include", "technologies", "confidence", "reason"),
  additionalProperties = FALSE)

# Record text presented to the models -----------------------------------------

queue[, rec_id := sprintf("LIT_%04d", .I)]
queue[, user_text := sprintf(
  "TITLE: %s\nSOURCE: %s (%s)\nDATABASE TYPE TAG: %s\nABSTRACT: %s",
  title, source_title, year, subtype,
  fifelse(is.na(abstract) | abstract == "", "(not available)", abstract))]
fwrite(queue, queue_file)

if (!run_enabled) {
  cat("\nGATED: LITSCREEN_RUN_ENABLED != true - no billable API contacted.\n")
} else {

  cl_raw_file <- file.path(out_dir, "literature_screening_raw_claude.csv")
  ds_raw_file <- file.path(out_dir, "literature_screening_raw_deepseek.csv")

  claude_mode <- Sys.getenv("LITSCREEN_CLAUDE_MODE", "direct")
  if (file.exists(cl_raw_file)) {
    cat("\nClaude raw responses found on disk - reusing (no API call).\n")
    cl_raw <- fread(cl_raw_file, encoding = "UTF-8")
  } else if (claude_mode == "batch") {
    cat("\nSubmitting Claude screening batch ...\n")
    reqs <- lapply(seq_len(nrow(queue)), function(i)
      build_coding_request_fun(custom_id = queue$rec_id[i],
                               system_prompt = screening_prompt,
                               user_text = queue$user_text[i],
                               schema = screening_schema,
                               max_tokens = 300L, effort = "medium"))
    batch <- submit_batch_fun(reqs)
    batch <- poll_batch_fun(batch$id)
    cl_raw <- fetch_batch_results_fun(batch$id)
    fwrite(cl_raw, cl_raw_file)
  } else {
    cat("\nRunning Claude screening (direct, concurrent) ...\n")
    cl_raw <- run_claude_direct_fun(
      items = queue[, .(custom_id = rec_id, user_text)],
      system_prompt = screening_prompt, schema = screening_schema,
      max_tokens = 300L, effort = "medium")
    fwrite(cl_raw, cl_raw_file)
  }
  setnames(cl_raw, c("custom_id", "status", "text"),
           c("rec_id", "cl_status", "cl_text"), skip_absent = TRUE)

  # DeepSeek concurrently -------------------------------------------------------

  if (file.exists(ds_raw_file)) {
    cat("DeepSeek raw responses found on disk - reusing (no API call).\n")
    ds_raw <- fread(ds_raw_file, encoding = "UTF-8")
  } else {
    cat("Running DeepSeek screening ...\n")
    ds_raw <- run_deepseek_batch_fun(
      items = queue[, .(custom_id = rec_id, user_text)],
      system_prompt = screening_prompt, temperature = 0, max_tokens = 300L)
    fwrite(ds_raw, ds_raw_file)
  }
  setnames(ds_raw, c("custom_id", "status", "text"),
           c("rec_id", "ds_status", "ds_text"), skip_absent = TRUE)

  # Parse votes ----------------------------------------------------------------

  parse_vote <- function(txt, prefix) {
    v <- tryCatch(jsonlite::parse_json(txt), error = function(e) NULL)
    out <- data.table(
      include = if (is.null(v)) NA else isTRUE(v$include),
      tech = if (is.null(v)) NA_character_ else
        paste(unlist(v$technologies), collapse = ";"),
      conf = if (is.null(v)) NA_real_ else as.numeric(v$confidence %||% NA),
      reason = if (is.null(v)) NA_character_ else as.character(v$reason %||% NA))
    setnames(out, paste0(prefix, names(out)))
    out
  }
  votes <- merge(queue[, .(rec_id, technology, title, year, doi, database)],
                 cl_raw[, .(rec_id, cl_status, cl_text)], by = "rec_id", all.x = TRUE)
  votes <- merge(votes, ds_raw[, .(rec_id, ds_status, ds_text)], by = "rec_id", all.x = TRUE)
  votes <- cbind(votes,
                 rbindlist(lapply(votes$cl_text, parse_vote, prefix = "cl_")),
                 rbindlist(lapply(votes$ds_text, parse_vote, prefix = "ds_")))
  votes[, agreement := fifelse(!is.na(cl_include) & !is.na(ds_include) &
                               cl_include == ds_include, "agree", "adjudicate")]
  fwrite(votes[, !c("cl_text", "ds_text")],
         file.path(out_dir, "literature_screening_votes.csv"))

  # Adjudication queue: disagreements + parse failures --------------------------

  adj <- votes[agreement == "adjudicate",
               .(rec_id, technology, title, year, doi, cl_include, cl_reason,
                 ds_include, ds_reason,
                 human_include = NA_character_, adjudicator = NA_character_)]
  fwrite(adj, file.path(out_dir, "literature_screening_adjudication.csv"))
  cat(sprintf("\n%d records screened: %d concordant, %d to adjudicate\n",
              nrow(votes), sum(votes$agreement == "agree"), nrow(adj)))
  cat(sprintf("Concordant includes: %d\n",
              votes[agreement == "agree" & cl_include == TRUE, .N]))
}
