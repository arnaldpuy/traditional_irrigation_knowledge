# STAGE-2 LLM READING OF THE CITED STUDIES ####################################

# GATED behind STAGE2_RUN_ENABLED. STAGE2_CITED_NOS (comma-separated cited_no)

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "httr2", "jsonlite", "digest",
                          "pdftools"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# CONFIGURATION ###############################################################

run_enabled <- tolower(Sys.getenv("STAGE2_RUN_ENABLED", "false")) == "true"
model <- Sys.getenv("STAGE2_MODEL", "claude-fable-5-1")
effort <- Sys.getenv("STAGE2_EFFORT", "high")
n_draws <- as.integer(Sys.getenv("STAGE2_DRAWS", "3"))
timeout_s <- as.integer(Sys.getenv("STAGE2_TIMEOUT", "900"))
out_dir <- here("datasets", "output", Sys.getenv("STAGE2_OUT_DIR", "stage2_llm"))
with_context <- tolower(Sys.getenv("STAGE2_CONTEXT", "true")) == "true"
pdf_dir <- Sys.getenv("STAGE2_PDF_DIR",
                      "/Volumes/LaCieData/irrigators_assumptions/cited_papers")
reader_dir <- Sys.getenv("STAGE2_READER_DIR", path.expand("~/Desktop/stage2_reader_files"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir_in <- here("datasets", "output", "link_extraction", "fable_full")
questions <- paste0("q", 1:9)

# INSTRUMENT ##################################################################

system_prompt <- paste(readLines(here("prompts", "stage2_reading_prompt_v1.txt"),
                                 encoding = "UTF-8"), collapse = "\n")
prompt_hash <- digest(system_prompt, algo = "sha256")

yes_no <- list(type = "string", enum = list("yes", "no"))
row_schema <- list(
  type = "object",
  properties = c(
    list(claim_id = list(type = "string")),
    setNames(rep(list(yes_no), 9), questions),
    list(evidence = list(type = "string"),
         notes = list(type = "string"),
         upstream = list(type = "boolean"),
         technology_mismatch = list(type = "boolean"))),
  required = as.list(c("claim_id", questions, "evidence", "notes", "upstream",
                       "technology_mismatch")),
  additionalProperties = FALSE)
study_schema <- list(
  type = "object",
  properties = list(
    cited_no = list(type = "string"),
    rows = list(type = "array", items = row_schema),
    notes = list(type = "string")),
  required = list("cited_no", "rows", "notes"),
  additionalProperties = FALSE)

# STUDIES AND CLAIM ROWS ######################################################

sample_dt <- fread(file.path(dir_in, "stage2_sample.csv"))
manifest <- fread(file.path(dir_in, "stage2_retrieval_manifest.csv"))
supp_sample <- file.path(dir_in, "stage2_supplementary_sample.csv")
supp_manifest <- file.path(dir_in, "stage2_supplementary_manifest.csv")
include_supplement <- tolower(Sys.getenv("STAGE2_INCLUDE_SUPPLEMENT", "true")) == "true"
if (include_supplement && file.exists(supp_sample) && file.exists(supp_manifest)) {
  supp <- fread(supp_sample)
  stopifnot(!any(supp$cited_no %in% sample_dt$cited_no))
  sample_dt <- rbind(sample_dt, supp[, intersect(names(sample_dt), names(supp)), with = FALSE], fill = TRUE)
  manifest <- rbind(manifest, fread(supp_manifest, colClasses = list(character = c("date", "sha256", "note", "file", "route"))),
                    fill = TRUE)
  cat(sprintf("supplementary sample appended: %d studies, %d retrieved\n", nrow(supp),
              fread(supp_manifest)[status == "retrieved", .N]))
}
assignment <- fread(file.path(dir_in, "stage2_reader_assignment.csv"))
links <- fread(file.path(dir_in, "link_rows_cited.csv"))
passages <- fread(file.path(dir_in, "link_extraction.csv"))
str(manifest); manifest[1:3]

studies <- merge(sample_dt[, .(cited_no, cited_id, title, year, first_author)],
                 manifest[status == "retrieved", .(cited_id, file)], by = "cited_id")
cited_nos <- Sys.getenv("STAGE2_CITED_NOS", "")
if (nzchar(cited_nos))
  studies <- studies[cited_no %in% as.integer(trimws(strsplit(cited_nos, ",")[[1]]))]
setorder(studies, cited_no)
cat(sprintf("%d studies with a retrieved file (%d in the sample)\n",
            nrow(studies), nrow(sample_dt)))

setkey(passages, rec_id, citation_group)
claims <- unique(links[cited_id %in% studies$cited_id,
                       .(cited_id, rec_id, citation_group, technology,
                         `function`, direction)])
claims <- merge(claims, unique(passages[, .(rec_id, citation_group, passage, link_type,
                                            supporting_passages, inference_note)]),
                by = c("rec_id", "citation_group"), all.x = TRUE)
if (!with_context) claims[, c("supporting_passages", "inference_note") := ""]
claims <- merge(claims, studies[, .(cited_id, cited_no)], by = "cited_id")
claims[, claim_id := sprintf("%s_%s_%s", citation_group, technology, `function`)]
stopifnot(!anyNA(claims$passage), !anyDuplicated(claims[, .(cited_no, claim_id)]))
setorder(claims, cited_no, claim_id)
cat(sprintf("%d claim rows for %d studies\n", nrow(claims), uniqueN(claims$cited_no)))

# FULL TEXTS ##################################################################

locate_file <- function(f) {
  p <- file.path(pdf_dir, f)
  if (file.exists(p)) return(p)
  copy <- assignment[source_file == f][1]
  if (nrow(copy) && !is.na(copy$file))
    return(file.path(reader_dir, copy$reader, "papers", copy$file))
  NA_character_
}
studies[, path := vapply(file, locate_file, character(1))]
missing <- studies[is.na(path) | !file.exists(path)]
if (nrow(missing)) {
  cat("No file on disk for:\n"); print(missing[, .(cited_no, file)])
  studies <- studies[!cited_no %in% missing$cited_no]
}
texts <- rbindlist(lapply(studies$path, extract_doc_text_fun, max_chars = 5000000L))
texts[, cited_no := studies$cited_no]
cat("Characters per study (quantiles):\n"); print(quantile(texts$n_chars))

unusable <- texts[n_chars < 1000, .(cited_no, file, source, n_chars)]
if (nrow(unusable)) {
  cat("No usable text, left out:\n"); print(unusable)
  texts <- texts[!cited_no %in% unusable$cited_no]
  studies <- studies[!cited_no %in% unusable$cited_no]
  claims <- claims[cited_no %in% studies$cited_no]
}
fwrite(claims, file.path(out_dir, "stage2_llm_claims.csv"))
cat(sprintf("%d claim rows for %d studies will be sent\n", nrow(claims), uniqueN(claims$cited_no)))

context_line <- function(sp, note) {
  out <- character(length(sp))
  has_sp <- !is.na(sp) & nzchar(sp)
  has_note <- !is.na(note) & nzchar(note)
  out[has_sp] <- paste0("\n  context (neighbouring passages the extractor used to attribute this ",
                        "claim; the claim remains the passage above): ", sp[has_sp])
  out[has_note] <- paste0(out[has_note], "\n  extractor note: ", note[has_note])
  out
}
claim_block <- claims[, .(block = paste(sprintf(
  "[%s] technology: %s | function: %s | direction: %s\n  passage: %s%s",
  claim_id, technology, `function`, direction, passage,
  context_line(supporting_passages, inference_note)), collapse = "\n\n")),
  by = cited_no]
items <- merge(studies[, .(cited_no, title, year, first_author)], claim_block,
               by = "cited_no")
items <- merge(items, texts[, .(cited_no, doc_text = body)], by = "cited_no")
items[, user_text := sprintf(
  "STUDY cited_no: %d\n%s (%s), first author %s\n\nCLAIM ROWS:\n%s\n\nThe full text of the study is in the DOCUMENT TEXT block above.",
  cited_no, title, year, first_author, block)]
items[, custom_id := as.character(cited_no)]
setorder(items, cited_no)
items[, user_text_sha256 := vapply(user_text, function(x) digest::digest(x, algo = "sha256", serialize = FALSE),
                                   character(1))]
fwrite(items[, .(cited_no, with_context = with_context, n_chars_doc = nchar(doc_text), user_text_sha256)],
       file.path(out_dir, "stage2_llm_items.csv"))

# READING #####################################################################

raw_path <- function(custom_id, draw)
  file.path(out_dir, sprintf("draw_%d", draw), paste0(custom_id, "_raw.json"))
for (k in seq_len(n_draws)) dir.create(file.path(out_dir, sprintf("draw_%d", k)),
                                       showWarnings = FALSE)
meta_file <- file.path(out_dir, "stage2_llm_meta.csv")
read_meta <- function() fread(meta_file, colClasses = list(character = c("date", "prompt_sha256")))

if (!run_enabled) {
  cat("\nGATED: STAGE2_RUN_ENABLED != true - no billable API contacted.\n")
} else {
  n_todo <- sum(!file.exists(unlist(lapply(seq_len(n_draws), function(k)
    raw_path(items$custom_id, k)))))
  cat(sprintf("Firing %d study-draw(s) (%d studies x %d draws, less those stored) at %s, effort %s\n",
              n_todo, nrow(items), n_draws, model, effort))

  store <- function(res) {
    if (res$status == "succeeded")
      writeLines(res$text, raw_path(res$custom_id, res$draw))
    else cat(sprintf("  %s draw %d: %s\n", res$custom_id, res$draw, res$status))
    meta <- data.table(cited_no = as.integer(res$custom_id), draw = res$draw,
                       status = res$status, model = model, effort = effort,
                       date = as.character(Sys.Date()), prompt_sha256 = prompt_hash,
                       input_tokens = res$input_tokens,
                       cache_creation_input_tokens = res$cache_creation_input_tokens,
                       cache_read_input_tokens = res$cache_read_input_tokens,
                       output_tokens = res$output_tokens, stop_reason = res$stop_reason)
    if (file.exists(meta_file)) meta <- rbind(read_meta(), meta, fill = TRUE)
    fwrite(meta, meta_file)
  }
  res <- run_claude_cached_draws_fun(
    items = items[, .(custom_id, doc_text, user_text)],
    system_prompt = system_prompt, schema = study_schema, n_draws = n_draws,
    done = function(custom_id, draw) file.exists(raw_path(custom_id, draw)),
    on_result = store, model = model, max_tokens = 16000L, effort = effort,
    concurrency = 2L, timeout = timeout_s)
  if (nrow(res)) {
    cat(sprintf("\n%d replies: %s\n", nrow(res),
                paste(sprintf("%s %d", names(table(res$status)), table(res$status)),
                      collapse = ", ")))
    print(res[status == "succeeded",
              .(replies = .N, input = sum(input_tokens, na.rm = TRUE),
                cache_write = sum(cache_creation_input_tokens, na.rm = TRUE),
                cache_read = sum(cache_read_input_tokens, na.rm = TRUE),
                output = sum(output_tokens, na.rm = TRUE)), by = draw])
  }
}

# PARSE #######################################################################

read_draw <- function(k) {
  draw_dir <- file.path(out_dir, sprintf("draw_%d", k))
  files <- file.path(draw_dir, paste0(items$custom_id, "_raw.json"))
  rbindlist(lapply(files[file.exists(files)], function(f) {
    v <- tryCatch(fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(v) || !length(v$rows)) return(NULL)
    rbindlist(lapply(v$rows, function(r)
      data.table(cited_no = as.integer(v$cited_no), draw = k, claim_id = r$claim_id,
                 as.data.table(setNames(lapply(questions, function(q) r[[q]]), questions)),
                 upstream = isTRUE(r$upstream),
                 technology_mismatch = isTRUE(r$technology_mismatch),
                 evidence = r$evidence, notes = r$notes, study_notes = v$notes)))
  }), fill = TRUE)
}
wide <- rbindlist(lapply(seq_len(n_draws), read_draw), fill = TRUE)

if (nrow(wide)) {
  sent <- claims[, paste(cited_no, claim_id)]
  unknown <- wide[!paste(cited_no, claim_id) %in% sent]
  if (nrow(unknown)) cat(sprintf("%d reply row(s) carry a claim_id not sent; dropped.\n",
                                 nrow(unknown)))
  wide <- wide[paste(cited_no, claim_id) %in% sent]
  n_fix <- wide[, sum((q1 == "no" & Reduce(`|`, lapply(.SD, `==`, "yes"))) |
                      (q2 == "no" & Reduce(`|`, lapply(.SD[, -1], `==`, "yes")))),
                .SDcols = questions[-1]]
  wide[q1 == "no", (questions[-1]) := "no"]
  wide[q2 == "no", (questions[-(1:2)]) := "no"]
  cat(sprintf("gate rules re-applied to %d reply row(s)\n", n_fix))
  answers <- melt(wide, id.vars = c("cited_no", "draw", "claim_id", "upstream",
                                    "technology_mismatch", "evidence", "notes",
                                    "study_notes"),
                  measure.vars = questions, variable.name = "question",
                  value.name = "answer")
  answers <- merge(answers, claims[, .(cited_no, claim_id, technology, `function`, direction)],
                   by = c("cited_no", "claim_id"))
  setcolorder(answers, c("cited_no", "claim_id", "technology", "function",
                         "direction", "draw", "question", "answer"))
  setorder(answers, cited_no, claim_id, question, draw)
  fwrite(answers, file.path(out_dir, "stage2_llm_answers.csv"))

  # Vote rules -----------------------------------------------------------------

  votes <- answers[, .(n_draws = .N, n_yes = sum(answer == "yes")),
                   by = .(cited_no, claim_id, technology, `function`, direction, question)]
  votes[, `:=`(any_draw = n_yes >= 1L,
               majority = n_yes > n_draws / 2,
               unanimous = n_yes == n_draws)]
  fwrite(votes, file.path(out_dir, "stage2_llm_votes.csv"))

  # Readout -------------------------------------------------------------------

  cat(sprintf("\n%d studies, %d citations (study x claim), %d draws parsed\n",
              uniqueN(answers$cited_no), uniqueN(answers[, .(cited_no, claim_id)]), max(answers$draw)))
  cat(sprintf("citations with all %d draws: %d of %d\n", n_draws,
              uniqueN(votes[n_draws == max(n_draws), .(cited_no, claim_id)]),
              uniqueN(votes[, .(cited_no, claim_id)])))
  print(dcast(votes[, .(share_yes = round(mean(majority), 2)), by = question],
              . ~ question, value.var = "share_yes"))
  print(votes[, .(draw_to_draw_agreement = round(mean(n_yes %in% c(0L, n_draws)), 2)),
              by = question])
  flags <- wide[, .(upstream = mean(upstream), technology_mismatch = mean(technology_mismatch)),
                by = draw]
  print(flags)
} else cat("\nNo raw replies parsed.\n")
