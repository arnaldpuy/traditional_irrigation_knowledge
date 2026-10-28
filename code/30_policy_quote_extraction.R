# POLICY VERBATIM-QUOTE EXTRACTION ###########################################

# to promise. GATED behind QUOTE_RUN_ENABLED.

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "httr2", "jsonlite", "pdftools"))

r_functions <- list.files(path = here("functions"), pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# CONFIGURATION ###############################################################

run_enabled <- tolower(Sys.getenv("QUOTE_RUN_ENABLED", "false")) == "true"
out_dir <- here("datasets", "output")
pol_dir <- here("datasets", "inputs", "policy_corpus")
max_chars <- 200000L

target_fns <- c("social_cohesion", "biodiversity", "risk_management",
                "groundwater_recharge", "water_conservation", "crop_production")

# TARGET CELLS ################################################################

pm <- fread(file.path(out_dir, "policy_promise_map.csv")); setnames(pm, "function", "fn")
targets <- pm[delivered_consensus == TRUE & fn %in% target_fns,
              .(fns = paste(sort(unique(fn)), collapse = ", ")), by = rec_id]
cat(sprintf("Documents with target promises: %d\n", nrow(targets)))

lin_dir <- file.path(pol_dir, "linearized")
cap_txt <- function(b) {
  b <- trimws(b)
  if (nchar(b) > max_chars) {
    h <- as.integer(max_chars * 0.6); tl <- max_chars - h
    b <- paste0(substr(b, 1, h), "\n\n[... middle omitted ...]\n\n",
                substr(b, nchar(b) - tl + 1, nchar(b)))
  }
  b
}
if (dir.exists(lin_dir) && length(list.files(lin_dir, "\\.txt$")) > 0) {
  txt <- rbindlist(lapply(list.files(lin_dir, "\\.txt$", full.names = TRUE),
    function(f) data.table(doc_id = tools::file_path_sans_ext(basename(f)),
      body = cap_txt(paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
                           collapse = "\n")))))
} else {
  txt <- rbindlist(lapply(
    list.files(pol_dir, pattern = "\\.pdf$", full.names = TRUE),
    extract_doc_text_fun, max_chars = max_chars))
  txt[, doc_id := tools::file_path_sans_ext(file)]
}
targets <- merge(targets, txt[, .(doc_id, body)], by.x = "rec_id",
                 by.y = "doc_id", all.x = TRUE)
targets <- targets[!is.na(body) & nchar(body) > 500]

# INSTRUMENT ##################################################################

quote_prompt <- '
SYSTEM ROLE
You extract VERBATIM promotional sentences from an irrigation-modernisation policy
document, for use as direct quotations in an academic paper.

TASK
You are given the document text and a list of functions the document promises.
For each listed function, copy 1-2 sentences, WORD FOR WORD, exactly as they
appear in the text (same language, same characters, same accents, no paraphrase,
no ellipsis unless it is in the original), in which the document claims irrigation
modernisation provides or protects that function. Provide an English translation.

HARD RULES
- Copy the exact substring from the text. Do NOT fix, shorten, translate, or
  rephrase the verbatim_quote field. If you cannot find an exact sentence for a
  function, OMIT that function rather than invent or approximate one.
- Prefer objective/justification statements over passing mentions.
- verbatim_quote must be a contiguous exact span of the provided text.

OUTPUT
Return ONLY this JSON object:
{"quotes": [
  {"function": "<one of the listed functions>",
   "verbatim_quote": "<exact text from the document>",
   "language": "es|ca|en",
   "english_translation": "<faithful English translation>",
   "context": "<section or heading hint, <=15 words>"}]}'

quote_schema <- list(
  type = "object",
  properties = list(quotes = list(type = "array", items = list(
    type = "object",
    properties = list(
      `function` = list(type = "string", enum = as.list(target_fns)),
      verbatim_quote = list(type = "string"),
      language = list(type = "string", enum = list("es", "ca", "en")),
      english_translation = list(type = "string"),
      context = list(type = "string")),
    required = list("function", "verbatim_quote", "language",
                    "english_translation", "context"),
    additionalProperties = FALSE))),
  required = list("quotes"), additionalProperties = FALSE)

targets[, user_text := sprintf(
  "DOCUMENT: %s\nFUNCTIONS THIS DOCUMENT PROMISES (extract a verbatim quote for each): %s\n\nTEXT:\n%s",
  rec_id, fns, body)]

# VERIFICATION HELPERS ########################################################

norm <- function(s) gsub("\\s+", " ", tolower(trimws(s)))
verify_quote <- function(q, body) {
  nq <- norm(q); nb <- norm(body)
  if (nchar(nq) < 8) return(list(exact = FALSE, overlap = 0))
  exact <- grepl(nq, nb, fixed = TRUE)
  qt <- strsplit(nq, " ")[[1]]
  ov <- if (length(qt) < 4) NA_real_ else {
    sh <- vapply(seq_len(length(qt) - 3), function(i)
      paste(qt[i:(i + 3)], collapse = " "), character(1))
    mean(vapply(sh, function(x) grepl(x, nb, fixed = TRUE), logical(1)))
  }
  list(exact = exact, overlap = ov)
}

# RUN #########################################################################

if (!run_enabled) {
  cat("\nGATED: QUOTE_RUN_ENABLED != true - no billable API contacted.\n")
} else {
  raw_file <- file.path(out_dir, "policy_quotes_raw.rds")
  if (file.exists(raw_file)) {
    cat("Quote raw responses on disk - reusing.\n")
    raw <- as.data.table(readRDS(raw_file))
  } else {
    raw <- run_claude_direct_fun(
      items = targets[, .(custom_id = rec_id, user_text)],
      system_prompt = quote_prompt, schema = quote_schema,
      max_tokens = 2000L, effort = "medium", concurrency = 4L, timeout = 300L)
    saveRDS(raw, raw_file)
  }

  parse_quotes <- function(txt, cid) {
    v <- tryCatch(jsonlite::parse_json(txt), error = function(e) NULL)
    if (is.null(v) || length(v$quotes) == 0) return(NULL)
    rbindlist(lapply(v$quotes, function(q) data.table(
      rec_id = cid, `function` = q[["function"]] %||% NA_character_,
      verbatim_quote = q$verbatim_quote %||% NA_character_,
      language = q$language %||% NA_character_,
      english_translation = q$english_translation %||% NA_character_,
      context = q$context %||% NA_character_)), fill = TRUE)
  }
  quotes <- rbindlist(Map(parse_quotes,
                          raw[status == "succeeded"]$text,
                          raw[status == "succeeded"]$custom_id), fill = TRUE)

  # Verify each quote against the exact text the model saw --------------------

  bodies <- setNames(targets$body, targets$rec_id)
  ver <- rbindlist(lapply(seq_len(nrow(quotes)), function(i) {
    v <- verify_quote(quotes$verbatim_quote[i], bodies[[quotes$rec_id[i]]])
    data.table(exact_match = v$exact, token_overlap = v$overlap)
  }))
  quotes <- cbind(quotes, ver)
  quotes[, verified := exact_match | (!is.na(token_overlap) & token_overlap >= 0.9)]

  quotes[, verbatim_clean := gsub("­\\s*", "", verbatim_quote)]
  quotes[, verbatim_clean := gsub("([[:alpha:]])-\\s*\n\\s*([[:alpha:]])", "\\1\\2",
                                  verbatim_clean)]
  quotes[, verbatim_clean := trimws(gsub("\\s+", " ", verbatim_clean))]
  setcolorder(quotes, c("rec_id", "function", "verified", "exact_match",
                        "token_overlap", "language", "verbatim_clean",
                        "verbatim_quote", "english_translation", "context"))
  fwrite(quotes, file.path(out_dir, "policy_quotes.csv"))

  # Manuscript-ready table: verified quotes with a readable document label -----

  man <- fread(file.path(pol_dir, "policy_corpus_manifest.csv"), encoding = "UTF-8")
  lab_col <- intersect(c("short_title", "title", "document"), names(man))[1]
  key_col <- intersect(c("doc_id", "file", "id"), names(man))[1]
  mtab <- quotes[verified == TRUE, .(rec_id, `function`, language,
                                     verbatim_clean, english_translation, context)]
  fwrite(mtab, file.path(out_dir, "policy_quotes_manuscript.csv"))

  # READOUT ###################################################################

  cat(sprintf("\nQuotes returned: %d ; verified (exact or >=0.9 overlap): %d\n",
              nrow(quotes), quotes[verified == TRUE, .N]))
  cat("Verified quotes per function:\n")
  print(quotes[verified == TRUE, .N, by = `function`][order(-N)])
  cat("\nSample verified over-promise quotes (social_cohesion / biodiversity):\n")
  ex <- quotes[verified == TRUE & `function` %in% c("social_cohesion", "biodiversity")]
  for (i in seq_len(min(6, nrow(ex)))) cat(sprintf(
    "\n[%s | %s] \"%s\"\n  -> %s\n", ex$rec_id[i], ex$`function`[i],
    substr(ex$verbatim_quote[i], 1, 180), substr(ex$english_translation[i], 1, 160)))
}
