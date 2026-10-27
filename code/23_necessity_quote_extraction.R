# NECESSITY-PREMISE QUOTE EXTRACTION, VERIFICATION AND VALIDATION #############

# present). GATED behind NECQUOTE_RUN_ENABLED.

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "httr2", "jsonlite", "pdftools"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# CONFIGURATION ###############################################################

run_enabled <- tolower(Sys.getenv("NECQUOTE_RUN_ENABLED", "false")) == "true"
out_dir <- here("datasets", "output")
nec_dir <- here("datasets", "inputs", "necessity_corpus")
pol_dir <- here("datasets", "inputs", "policy_corpus")

cap_academic <- 80000L
cap_full <- 200000L

# DOCUMENT ROSTER #############################################################

# Helper: cap text with head+tail preserved -----------------------------------

cap_txt <- function(b, cap) {
  b <- trimws(b)
  if (nchar(b) > cap) {
    h <- as.integer(cap * 0.6); tl <- cap - h
    b <- paste0(substr(b, 1, h), "\n\n[... middle omitted ...]\n\n",
                substr(b, nchar(b) - tl + 1, nchar(b)))
  }
  b
}

# Helper: read a folder of documents, preferring a linearized/ subdir ---------

read_docs <- function(dir, source_label, cap) {
  lin_dir <- file.path(dir, "linearized")
  if (dir.exists(lin_dir) && length(list.files(lin_dir, "\\.txt$")) > 0) {
    dt <- rbindlist(lapply(list.files(lin_dir, "\\.txt$", full.names = TRUE),
      function(f) data.table(doc_id = tools::file_path_sans_ext(basename(f)),
        body = cap_txt(paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
                             collapse = "\n"), cap))))
  } else {
    files <- list.files(dir, pattern = "\\.(pdf|xml)$", full.names = TRUE)
    if (length(files) == 0) return(data.table())
    dt <- rbindlist(lapply(files, extract_doc_text_fun, max_chars = cap),
                    fill = TRUE)
    dt[, doc_id := tools::file_path_sans_ext(file)]
    dt <- dt[, .(doc_id, body)]
  }
  dt[, doc_source := source_label]
  dt
}

# Academic stratum: full texts of screened includes (code/25) -----------------

docs <- read_docs(file.path(nec_dir, "fulltexts"), "academic", cap_academic)

# Institutional stratum: hand-retrieved flagship documents --------------------

docs <- rbind(docs, read_docs(file.path(nec_dir, "institutional"),
                              "institutional", cap_full), fill = TRUE)

# Policy stratum: the eleven policy documents, column-aware linearised --------

docs <- rbind(docs, read_docs(pol_dir, "policy", cap_full), fill = TRUE)

docs <- docs[!is.na(body) & nchar(body) > 500]
roster <- docs[, .N, by = doc_source]
cat(sprintf("Document roster: %d (%s)\n", nrow(docs),
            paste(sprintf("%s %d", roster$doc_source, roster$N), collapse = ", ")))

# EXTRACTION INSTRUMENT #######################################################

quote_prompt <- '
SYSTEM ROLE
You extract VERBATIM passages from documents on irrigation and its
modernization, for use as direct quotations in an academic paper.

THE TARGET CLAIM
The passage must assert, in the DOCUMENT\'S OWN VOICE, that the future
sustainability, viability, survival, competitiveness, or adaptation (to climate
change, water scarcity, or economic pressure) of irrigated agriculture — or of
traditional / existing irrigation systems — DEPENDS ON adopting technological
modernization or technologies that raise water or crop efficiency (drip,
pressurized networks, sensors, remote sensing, precision / smart / digital
irrigation, metering, or modernization in general). The dependence must be
stated (must, requires, depends on, essential, imperative, indispensable,
precondition, cannot ... without, only way, key to; Spanish: imprescindible,
condicion necesaria, pasa por, no sera viable sin, and equivalents).

TASK
Copy up to THREE passages (1-3 sentences each), WORD FOR WORD, exactly as they
appear in the text (same language, same characters, same accents, no
paraphrase, no ellipsis unless in the original), that assert the target claim.
Statements that merely praise a technology or report its benefits are NOT the
target claim; omit them. If the document contains no such passage, return an
empty list rather than approximate.

For each passage also code:
- link_strength: "explicit_necessity" (necessity or strict-conditional
  formulation) or "strong_normative" ("should", "needs to", recommendation).
- target: "traditional_specific" (traditional, historic, communal, surface /
  flood, existing "outdated" systems) or "irrigation_general".
- voice: "own_claim" (document asserts it) or "attributed" (document reports
  someone else asserting it).

HARD RULES
- verbatim_quote must be a contiguous exact span of the provided text. Do NOT
  fix, shorten, translate, or rephrase it. Omit rather than invent.
- Prefer motivation / justification statements (introductions, objectives,
  preambles, conclusions) over passing mentions.

OUTPUT
Return ONLY this JSON object:
{"quotes": [
  {"verbatim_quote": "<exact text from the document>",
   "language": "es|ca|en",
   "english_translation": "<faithful English translation>",
   "link_strength": "explicit_necessity|strong_normative",
   "target": "traditional_specific|irrigation_general",
   "voice": "own_claim|attributed",
   "context": "<section or heading hint, <=15 words>"}]}'

quote_schema <- list(
  type = "object",
  properties = list(quotes = list(type = "array", items = list(
    type = "object",
    properties = list(
      verbatim_quote = list(type = "string"),
      language = list(type = "string", enum = list("es", "ca", "en")),
      english_translation = list(type = "string"),
      link_strength = list(type = "string",
                           enum = list("explicit_necessity", "strong_normative")),
      target = list(type = "string",
                    enum = list("traditional_specific", "irrigation_general")),
      voice = list(type = "string", enum = list("own_claim", "attributed")),
      context = list(type = "string")),
    required = list("verbatim_quote", "language", "english_translation",
                    "link_strength", "target", "voice", "context"),
    additionalProperties = FALSE))),
  required = list("quotes"), additionalProperties = FALSE)

docs[, user_text := sprintf("DOCUMENT: %s\n\nTEXT:\n%s", doc_id, body)]

# VALIDATION INSTRUMENT #######################################################

validation_prompt <- 'SYSTEM ROLE
You judge whether a quoted passage asserts a specific claim. You see ONLY the
passage (with an English translation when the original is not English).

THE CLAIM
The future sustainability, viability, survival, competitiveness, or adaptation
(to climate change, water scarcity, or economic pressure) of irrigated
agriculture — or of traditional / existing irrigation systems — DEPENDS ON
adopting technological modernization or technologies that raise water or crop
efficiency.

TASK
Decide whether the passage asserts the claim: it must (1) name the stake, (2)
name the technological means, and (3) bind them with a dependence or necessity
operator (must, requires, depends on, essential, imperative, indispensable,
precondition, cannot ... without, only way, key to, or equivalents in Spanish
or Catalan). A passage that merely praises a technology, reports benefits, or
recommends it without a dependence link does NOT assert the claim.

OUTPUT
Return ONLY a JSON object:
{"asserts_premise": true|false,
 "link_strength": "explicit_necessity|strong_normative|promotional_only",
 "confidence": <0-1>, "reason": "<max 20 words>"}'

validation_schema <- list(
  type = "object",
  properties = list(
    asserts_premise = list(type = "boolean"),
    link_strength = list(type = "string",
                         enum = list("explicit_necessity", "strong_normative",
                                     "promotional_only")),
    confidence = list(type = "number"),
    reason = list(type = "string")),
  required = list("asserts_premise", "link_strength", "confidence", "reason"),
  additionalProperties = FALSE)

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
  cat("\nGATED: NECQUOTE_RUN_ENABLED != true - no billable API contacted.\n")
} else {

  # GATE 1: extraction (cached) -----------------------------------------------

  raw_file <- file.path(out_dir, "necessity_quotes_raw.rds")
  raw_prev <- if (file.exists(raw_file)) {
    as.data.table(readRDS(raw_file))[status == "succeeded"]
  } else data.table(custom_id = character(), status = character(),
                    text = character())
  todo <- docs[!doc_id %in% raw_prev$custom_id]
  cat(sprintf("Extraction: %d cached, %d to fire\n", nrow(raw_prev), nrow(todo)))
  if (nrow(todo) > 0) {
    raw_new <- run_claude_direct_fun(
      items = todo[, .(custom_id = doc_id, user_text)],
      system_prompt = quote_prompt, schema = quote_schema,
      max_tokens = 2500L, effort = "medium", concurrency = 4L, timeout = 300L)
    raw <- rbind(raw_prev, raw_new, fill = TRUE)
    saveRDS(raw, raw_file)
  } else {
    raw <- raw_prev
  }

  parse_quotes <- function(txt, cid) {
    v <- tryCatch(jsonlite::parse_json(txt), error = function(e) NULL)
    if (is.null(v) || length(v$quotes) == 0) return(NULL)
    rbindlist(lapply(v$quotes, function(q) data.table(
      doc_id = cid,
      verbatim_quote = q$verbatim_quote %||% NA_character_,
      language = q$language %||% NA_character_,
      english_translation = q$english_translation %||% NA_character_,
      link_strength = q$link_strength %||% NA_character_,
      target = q$target %||% NA_character_,
      voice = q$voice %||% NA_character_,
      context = q$context %||% NA_character_)), fill = TRUE)
  }
  quotes <- rbindlist(Map(parse_quotes,
                          raw[status == "succeeded"]$text,
                          raw[status == "succeeded"]$custom_id), fill = TRUE)
  quotes <- merge(quotes, docs[, .(doc_id, doc_source)], by = "doc_id",
                  all.x = TRUE)
  quotes[, quote_id := sprintf("NQ_%04d", .I)]

  # GATE 2: verification against the exact text the model saw ------------------

  bodies <- setNames(docs$body, docs$doc_id)
  ver <- rbindlist(lapply(seq_len(nrow(quotes)), function(i) {
    v <- verify_quote(quotes$verbatim_quote[i], bodies[[quotes$doc_id[i]]])
    data.table(exact_match = v$exact, token_overlap = v$overlap)
  }))
  quotes <- cbind(quotes, ver)
  quotes[, verified := exact_match | (!is.na(token_overlap) & token_overlap >= 0.9)]

  # GATE 3: blind dual-model validation of the verified quotes (cached) --------

  vq <- quotes[verified == TRUE]
  vq[, val_text := fifelse(
    !is.na(language) & language == "en", sprintf("PASSAGE:\n%s", verbatim_quote),
    sprintf("PASSAGE (original):\n%s\n\nENGLISH TRANSLATION:\n%s",
            verbatim_quote, english_translation))]

  cl_val_file <- file.path(out_dir, "necessity_validation_raw_claude.csv")
  ds_val_file <- file.path(out_dir, "necessity_validation_raw_deepseek.csv")

  read_val_cache <- function(f) {
    if (file.exists(f)) {
      as.data.table(utils::read.csv(f, stringsAsFactors = FALSE,
                                    encoding = "UTF-8"))
    } else {
      data.table(custom_id = character(), status = character(),
                 text = character())
    }
  }
  cl_prev <- read_val_cache(cl_val_file)
  todo_cl <- vq[!quote_id %in% cl_prev$custom_id]
  cat(sprintf("Validation (Claude): %d cached, %d to fire\n",
              nrow(cl_prev), nrow(todo_cl)))
  cl_val <- if (nrow(todo_cl) > 0) {
    new <- run_claude_direct_fun(
      items = todo_cl[, .(custom_id = quote_id, user_text = val_text)],
      system_prompt = validation_prompt, schema = validation_schema,
      max_tokens = 300L, effort = "medium")
    out <- rbind(cl_prev, new, fill = TRUE)
    fwrite(out, cl_val_file)
    out
  } else cl_prev
  ds_prev <- read_val_cache(ds_val_file)
  todo_ds <- vq[!quote_id %in% ds_prev$custom_id]
  cat(sprintf("Validation (DeepSeek): %d cached, %d to fire\n",
              nrow(ds_prev), nrow(todo_ds)))
  ds_val <- if (nrow(todo_ds) > 0) {
    new <- run_deepseek_batch_fun(
      items = todo_ds[, .(custom_id = quote_id, user_text = val_text)],
      system_prompt = validation_prompt, temperature = 0, max_tokens = 300L)
    out <- rbind(ds_prev, new, fill = TRUE)
    fwrite(out, ds_val_file)
    out
  } else ds_prev

  parse_val <- function(txt, prefix) {
    v <- tryCatch(jsonlite::parse_json(txt), error = function(e) NULL)
    out <- data.table(
      asserts = if (is.null(v)) NA else isTRUE(v$asserts_premise),
      strength = if (is.null(v)) NA_character_ else
        as.character(v$link_strength %||% NA),
      conf = if (is.null(v)) NA_real_ else as.numeric(v$confidence %||% NA))
    setnames(out, paste0(prefix, names(out)))
    out
  }
  setnames(cl_val, c("custom_id", "text"), c("quote_id", "cl_text"),
           skip_absent = TRUE)
  setnames(ds_val, c("custom_id", "text"), c("quote_id", "ds_text"),
           skip_absent = TRUE)
  vq <- merge(vq, cl_val[, .(quote_id, cl_text)], by = "quote_id", all.x = TRUE)
  vq <- merge(vq, ds_val[, .(quote_id, ds_text)], by = "quote_id", all.x = TRUE)
  vq <- cbind(vq, rbindlist(lapply(vq$cl_text, parse_val, prefix = "cl_")),
              rbindlist(lapply(vq$ds_text, parse_val, prefix = "ds_")))
  vq[, validation := fcase(
    !is.na(cl_asserts) & !is.na(ds_asserts) & cl_asserts & ds_asserts, "confirmed",
    !is.na(cl_asserts) & !is.na(ds_asserts) & !cl_asserts & !ds_asserts, "rejected",
    default = "adjudicate")]

  vq[, headline := validation == "confirmed" & voice == "own_claim" &
       cl_strength != "promotional_only" & ds_strength != "promotional_only"]

  # OUTPUTS #####################################################################

  quotes <- merge(quotes,
                  vq[, .(quote_id, cl_asserts, cl_strength, ds_asserts,
                         ds_strength, validation, headline)],
                  by = "quote_id", all.x = TRUE)

  quotes[doc_id == "10_5753_compbr_2019_40_467", language := "pt"]

  adj_file <- file.path(out_dir, "necessity_validation_adjudication.csv")
  if (file.exists(adj_file)) {
    prev_adj <- as.data.table(utils::read.csv(adj_file, stringsAsFactors = FALSE,
                                              encoding = "UTF-8"))
    ruled <- prev_adj[toupper(as.character(human_asserts)) %in% c("TRUE", "FALSE"),
                      .(quote_id, human_asserts = toupper(as.character(human_asserts)),
                        adjudicator)]
    if (nrow(ruled) > 0) {
      quotes <- merge(quotes, ruled, by = "quote_id", all.x = TRUE)
      quotes[human_asserts == "TRUE",
             `:=`(validation = "confirmed_human",
                  headline = voice == "own_claim")]
      quotes[human_asserts == "FALSE",
             `:=`(validation = "rejected_human", headline = FALSE)]
    }
  }

  quotes[, verbatim_clean := gsub("\\x{00AD}\\s*", "", verbatim_quote, perl = TRUE)]
  quotes[, verbatim_clean := gsub("([[:alpha:]])-\\s*\n\\s*([[:alpha:]])", "\\1\\2",
                                  verbatim_clean)]
  quotes[, verbatim_clean := trimws(gsub("\\s+", " ", verbatim_clean))]
  fwrite(quotes, file.path(out_dir, "necessity_quotes.csv"))

  fwrite(quotes[headline == TRUE,
                .(quote_id, doc_id, doc_source, language, link_strength, target,
                  verbatim_clean, english_translation, context)],
         file.path(out_dir, "necessity_quotes_manuscript.csv"))

  # Adjudication file: append new splits, never overwrite existing rulings ----

  adj <- quotes[validation %in% c("adjudicate", "confirmed_human",
                                  "rejected_human"),
                .(quote_id, doc_id, verbatim_clean, english_translation,
                  cl_asserts, cl_strength, ds_asserts, ds_strength)]
  if (exists("ruled") && nrow(ruled) > 0) {
    adj <- merge(adj, ruled, by = "quote_id", all.x = TRUE)
  } else {
    adj[, `:=`(human_asserts = NA_character_, adjudicator = NA_character_)]
  }
  fwrite(adj, adj_file)

  # PRISMA-style flow log -------------------------------------------------------

  flow <- data.table(
    gate = c("documents_rostered", "documents_quoted", "quotes_returned",
             "quotes_verified", "quotes_confirmed_both_models",
             "quotes_confirmed_human_adjudication", "quotes_headline",
             "documents_headline"),
    n = c(nrow(docs), uniqueN(quotes$doc_id), nrow(quotes),
          quotes[verified == TRUE, .N],
          quotes[validation == "confirmed", .N],
          quotes[validation == "confirmed_human", .N],
          quotes[headline == TRUE, .N],
          quotes[headline == TRUE, uniqueN(doc_id)]))
  fwrite(flow, file.path(out_dir, "necessity_flow_log.csv"))

  # READOUT #####################################################################

  cat("\nFlow:\n"); print(flow)
  cat(sprintf("\nTo adjudicate: %d quotes -> necessity_validation_adjudication.csv\n",
              nrow(adj)))
  cat("\nSample headline quotes:\n")
  ex <- quotes[headline == TRUE]
  for (i in seq_len(min(6, nrow(ex)))) cat(sprintf(
    "\n[%s | %s | %s] \"%s\"\n  -> %s\n", ex$doc_id[i], ex$doc_source[i],
    ex$link_strength[i], substr(ex$verbatim_clean[i], 1, 180),
    substr(ex$english_translation[i], 1, 160)))
}
