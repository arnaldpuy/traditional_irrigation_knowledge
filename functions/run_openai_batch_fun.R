run_openai_batch_fun <- function(items, system_prompt, model = "gpt-5.4",
                                 reasoning_effort = "low",
                                 max_completion_tokens = 1200L,
                                 response_format = list(type = "json_object"),
                                 api_key = Sys.getenv("OPENAI_API_KEY"),
                                 poll_seconds = 20L, timeout_minutes = 180L,
                                 tmpdir = tempdir(), verbose = TRUE) {

  if (!nzchar(api_key)) stop("OPENAI_API_KEY is not set.")
  auth <- paste("Bearer", api_key)
  t0 <- Sys.time()

  # 0. Auto-chunk under the enqueued-token ceiling ---------------------------
  ceiling_tokens <- 1.1e6
  est_tokens <- (nchar(system_prompt) + nchar(items$user_text) + 200) / 3.5
  if (sum(est_tokens) > ceiling_tokens) {
    n_chunks <- ceiling(sum(est_tokens) / ceiling_tokens)
    chunk_id <- ceiling(seq_len(nrow(items)) / (nrow(items) / n_chunks))
    if (verbose) message(sprintf(
      "openai: est. %.2fM input tokens > ceiling; splitting into %d sequential sub-batches",
      sum(est_tokens) / 1e6, n_chunks))
    res <- data.table::rbindlist(lapply(unique(chunk_id), function(ch)
      run_openai_batch_fun(items[chunk_id == ch], system_prompt, model,
                           reasoning_effort, max_completion_tokens,
                           response_format, api_key, poll_seconds,
                           timeout_minutes, tmpdir, verbose)))
    return(res)
  }

  # 1. Assemble the JSONL request file --------------------------------------
  lines <- vapply(seq_len(nrow(items)), function(i) {
    body <- list(model = model, reasoning_effort = reasoning_effort,
                 max_completion_tokens = max_completion_tokens,
                 response_format = response_format,
                 messages = list(
                   list(role = "system", content = system_prompt),
                   list(role = "user", content = items$user_text[i])))
    jsonlite::toJSON(list(custom_id = items$custom_id[i], method = "POST",
                          url = "/v1/chat/completions", body = body),
                     auto_unbox = TRUE)
  }, character(1))
  jsonl <- file.path(tmpdir, sprintf("openai_batch_%d.jsonl",
                                     as.integer(Sys.time())))
  writeLines(lines, jsonl)

  # 2. Upload the file (purpose = batch) ------------------------------------
  up <- httr2::request("https://api.openai.com/v1/files") |>
    httr2::req_headers("Authorization" = auth) |>
    httr2::req_body_multipart(purpose = "batch",
                              file = curl::form_file(jsonl)) |>
    httr2::req_retry(max_tries = 4) |>
    httr2::req_perform()
  file_id <- httr2::resp_body_json(up)$id

  # 3. Create the batch job -------------------------------------------------
  cr <- httr2::request("https://api.openai.com/v1/batches") |>
    httr2::req_headers("Authorization" = auth, "content-type" = "application/json") |>
    httr2::req_body_json(list(input_file_id = file_id,
                              endpoint = "/v1/chat/completions",
                              completion_window = "24h"), auto_unbox = TRUE) |>
    httr2::req_retry(max_tries = 4) |>
    httr2::req_perform()
  batch_id <- httr2::resp_body_json(cr)$id
  if (verbose) message(sprintf("openai batch %s submitted (%d requests)",
                               batch_id, nrow(items)))

  # 4. Poll until the batch resolves ----------------------------------------
  status_obj <- NULL
  repeat {
    status_obj <- httr2::request(paste0("https://api.openai.com/v1/batches/", batch_id)) |>
      httr2::req_headers("Authorization" = auth) |>
      httr2::req_retry(max_tries = 4) |>
      httr2::req_perform() |>
      httr2::resp_body_json()
    st <- status_obj$status
    done <- status_obj$request_counts$completed
    tot <- status_obj$request_counts$total
    if (verbose) message(sprintf("  [%s] %s: %s/%s",
                                 format(Sys.time(), "%H:%M:%S"), st,
                                 ifelse(is.null(done), "?", done),
                                 ifelse(is.null(tot), "?", tot)))
    if (st %in% c("completed", "failed", "expired", "cancelled")) break
    if (as.numeric(difftime(Sys.time(), t0, units = "mins")) > timeout_minutes)
      stop("openai batch ", batch_id, " timed out after ", timeout_minutes, " min")
    Sys.sleep(poll_seconds)
  }
  if (status_obj$status != "completed")
    warning("openai batch ", batch_id, " finished with status ", status_obj$status)

  # 5. Fetch and parse the output JSONL -------------------------------------
  out_id <- status_obj$output_file_id
  if (is.null(out_id))
    return(data.table::data.table(custom_id = items$custom_id,
                                  status = "errored", text = NA_character_))
  content <- httr2::request(paste0("https://api.openai.com/v1/files/", out_id, "/content")) |>
    httr2::req_headers("Authorization" = auth) |>
    httr2::req_retry(max_tries = 4) |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  rows <- Filter(nzchar, strsplit(content, "\n", fixed = TRUE)[[1]])
  parsed <- data.table::rbindlist(lapply(rows, function(ln) {
    o <- jsonlite::fromJSON(ln, simplifyVector = FALSE)
    code <- tryCatch(o$response$status_code, error = function(e) NULL)
    txt <- tryCatch(o$response$body$choices[[1]]$message$content,
                    error = function(e) NULL)
    data.table::data.table(
      custom_id = o$custom_id,
      status = if (!is.null(code) && code < 300 && !is.null(txt))
                 "succeeded" else "errored",
      text = if (is.null(txt)) NA_character_ else txt)
  }))

  # Return in the input order --------------------------------------------------
  out <- merge(data.table::data.table(custom_id = items$custom_id),
               parsed, by = "custom_id", all.x = TRUE, sort = FALSE)
  out[is.na(status), status := "errored"]
  out[]
}
