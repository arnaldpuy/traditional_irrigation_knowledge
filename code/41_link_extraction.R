# TECHNOLOGY-FUNCTION LINK EXTRACTION FROM THE REVIEW PAPERS ##################

# GATED behind LINK_RUN_ENABLED. LINK_DOC_IDS (comma-separated rec_ids) selects

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "httr2", "jsonlite", "openxlsx",
                          "digest", "stringi"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# CONFIGURATION ###############################################################

run_enabled <- tolower(Sys.getenv("LINK_RUN_ENABLED", "false")) == "true"
model <- Sys.getenv("LINK_MODEL", "claude-opus-5-5")
effort <- Sys.getenv("LINK_EFFORT", "high")
out_dir <- here("datasets", "output", Sys.getenv("LINK_OUT_DIR", "link_extraction"))
timeout_s <- as.integer(Sys.getenv("LINK_TIMEOUT", "900"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
ft_dir <- here("datasets", "inputs", "literature_corpus", "fulltexts")

manifest <- fread(here("datasets", "output", "literature_fulltext_manifest.csv"))
trial <- fread(here("datasets", "output", "trial_irrigation_sample.csv"))
doc_ids <- Sys.getenv("LINK_DOC_IDS", "")
doc_ids <- if (nzchar(doc_ids)) trimws(strsplit(doc_ids, ",")[[1]]) else trial$rec_id
docs <- manifest[rec_id %in% doc_ids]
stopifnot(nrow(docs) == length(doc_ids))

# INSTRUMENT ##################################################################

prompt_file <- here("prompts", "link_extraction_prompt.txt")
if (file.exists(prompt_file)) {
  system_prompt <- paste(readLines(prompt_file, encoding = "UTF-8"), collapse = "\n")
} else {
  prompt_lines <- readLines(here("manuscript", "prompt_draft.txt"), encoding = "UTF-8")
  rules <- grep("^=====", prompt_lines)
  system_prompt <- paste(prompt_lines[(rules[1] + 1):(rules[2] - 1)], collapse = "\n")
}
prompt_hash <- digest(system_prompt, algo = "sha256")

tech_vocab <- c("drip", "soil_sensor", "remote_sensing", "app_scheduling",
                "on_demand", "flow_metering")
fun_vocab <- c("crop_production", "water_conservation", "soil_health",
               "risk_management", "groundwater_recharge",
               "microclimate_regulation", "biodiversity", "pest_control",
               "livestock", "medicinal_food", "social_cohesion",
               "recreation_wellbeing")
link_schema <- list(
  type = "object",
  properties = list(
    technology = list(type = "string", enum = as.list(tech_vocab)),
    `function` = list(type = "string", enum = as.list(fun_vocab)),
    direction = list(type = "string", enum = list("delivers", "harms", "mixed")),
    link_type = list(type = "string", enum = list("explicit", "inferred")),
    passage = list(type = "string"),
    supporting_passages = list(type = "array", items = list(type = "string")),
    inference_note = list(type = "string"),
    citation_in_text = list(type = "string"),
    reference_entries = list(type = "array", items = list(type = "string")),
    section = list(type = "string"),
    claim_type = list(type = "string", enum = list("finding", "recommendation"))),
  required = list("technology", "function", "direction", "link_type", "passage",
                  "supporting_passages", "inference_note", "citation_in_text",
                  "reference_entries", "section", "claim_type"),
  additionalProperties = FALSE)
doc_schema <- list(
  type = "object",
  properties = list(
    doc_id = list(type = "string"),
    links = list(type = "array", items = link_schema),
    notes = list(type = "string")),
  required = list("doc_id", "links", "notes"),
  additionalProperties = FALSE)

# FULL TEXTS ##################################################################

texts <- rbindlist(lapply(seq_len(nrow(docs)), function(i)
  extract_doc_text_fun(file.path(ft_dir, docs$file[i]), max_chars = 5000000L)))
texts[, rec_id := docs$rec_id]
cat("Characters per paper:\n"); print(texts[, .(rec_id, source, n_chars)])
items <- texts[, .(custom_id = rec_id,
                   user_text = sprintf("DOCUMENT: %s\n\nTEXT:\n%s", rec_id, body))]

# EXTRACTION ##################################################################

if (!run_enabled) {
  cat("\nGATED: LINK_RUN_ENABLED != true - no billable API contacted.\n")
} else {
  todo <- items[!file.exists(file.path(out_dir, paste0(custom_id, "_raw.json")))]
  if (nrow(todo)) {
    cat(sprintf("Firing %d paper(s) at %s, effort %s\n", nrow(todo), model, effort))
    res <- run_claude_direct_fun(items = todo, system_prompt = system_prompt,
                                 schema = doc_schema, model = model,
                                 max_tokens = 32000L, effort = effort,
                                 concurrency = 2L, timeout = timeout_s,
                                 thinking = "adaptive")
    for (i in seq_len(nrow(res))) {
      if (res$status[i] == "succeeded")
        writeLines(res$text[i], file.path(out_dir, paste0(res$custom_id[i], "_raw.json")))
      else cat(sprintf("  %s: %s\n", res$custom_id[i], res$status[i]))
    }
    meta <- data.table(rec_id = res$custom_id, status = res$status, model = model,
                       effort = effort, date = as.character(Sys.Date()),
                       prompt_sha256 = prompt_hash)
    meta_file <- file.path(out_dir, "link_extraction_meta.csv")
    if (file.exists(meta_file)) meta <- rbind(fread(meta_file), meta, fill = TRUE)
    fwrite(meta, meta_file)
  } else cat("All requested papers already have a raw reply; parsing only.\n")
}

# PARSE AND VERIFY ############################################################

norm <- function(x) {
  x <- gsub("[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f]", "", x, perl = TRUE)   # pdftotext leaves C0 bytes for some accents
  x <- stringi::stri_trans_nfkc(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("[\u2018\u2019\u02bc]", "'", x)
  x <- gsub("[\u201c\u201d]", "\"", x)
  x <- gsub("\\s*[\u00ad\u2010\u2011\u2012\u2013\u2014-]\\s*", "", x)
  x <- gsub("\\s+", " ", x)
  tolower(trimws(x))
}
tok <- function(x) unique(strsplit(gsub("[^[:alnum:] ]", " ", norm(x)), " +")[[1]])
overlap <- function(q, body_tokens) {
  qt <- setdiff(tok(q), "")
  if (!length(qt)) return(NA_real_)
  mean(qt %in% body_tokens)
}

raw_files <- file.path(out_dir, paste0(docs$rec_id, "_raw.json"))
have <- file.exists(raw_files)
links <- rbindlist(lapply(which(have), function(i) {
  v <- tryCatch(parse_json(paste(readLines(raw_files[i], warn = FALSE, encoding = "UTF-8"),
                                 collapse = "\n")), error = function(e) NULL)
  if (is.null(v) || !length(v$links)) return(NULL)
  body <- texts[rec_id == docs$rec_id[i], body]
  body_n <- norm(body)
  body_tok <- tok(body)
  rbindlist(lapply(seq_along(v$links), function(k) {
    l <- v$links[[k]]
    refs <- unlist(l$reference_entries)
    if (!length(refs)) refs <- "not found"
    data.table(
      rec_id = docs$rec_id[i],
      technology = l$technology, `function` = l[["function"]],
      direction = l$direction, link_type = l$link_type,
      passage = l$passage,
      supporting_passages = paste(unlist(l$supporting_passages), collapse = " || "),
      inference_note = l$inference_note,
      citation_in_text = l$citation_in_text,
      citation_group = sprintf("%s_%02d", docs$rec_id[i], k),
      n_cited = length(refs),
      reference_entry = refs,
      section = l$section, claim_type = l$claim_type,
      passage_verified = grepl(norm(l$passage), body_n, fixed = TRUE),
      passage_overlap = overlap(l$passage, body_tok),
      reference_overlap = vapply(refs, function(r)
        if (r %in% c("none", "not found", "")) NA_real_ else overlap(r, body_tok),
        numeric(1), USE.NAMES = FALSE),
      notes = v$notes)
  }), fill = TRUE)
}), fill = TRUE)

if (nrow(links)) {
  links <- merge(links, manifest[, .(rec_id, title, doi, year, technology_reviewed = technology)],
                 by = "rec_id", all.x = TRUE)
  setcolorder(links, c("rec_id", "title", "year", "doi", "technology_reviewed",
                       "technology", "function", "direction", "link_type",
                       "passage", "passage_verified", "passage_overlap",
                       "supporting_passages", "inference_note", "citation_in_text",
                       "citation_group", "n_cited", "reference_entry",
                       "reference_overlap", "section", "claim_type", "notes"))
  fwrite(links, file.path(out_dir, "link_extraction.csv"))
  write.xlsx(links, file.path(out_dir, "link_extraction.xlsx"), overwrite = TRUE)

  # Readout -------------------------------------------------------------------

  cat(sprintf("\n%d rows (%d passage x function links, %d cited studies) from %d paper(s)\n",
              nrow(links), uniqueN(links$citation_group), nrow(links), uniqueN(links$rec_id)))
  cat(sprintf("passages verified verbatim: %d / %d (overlap >= 0.9: %d)\n",
              sum(links$passage_verified), nrow(links),
              sum(links$passage_overlap >= 0.9, na.rm = TRUE)))
  cat(sprintf("reference entries with overlap >= 0.9: %d of %d given\n",
              sum(links$reference_overlap >= 0.9, na.rm = TRUE),
              sum(!is.na(links$reference_overlap))))
  print(links[, .N, by = .(technology, `function`, direction)][order(-N)])
  print(links[, .N, by = .(link_type, claim_type)])
} else cat("\nNo links parsed.\n")
