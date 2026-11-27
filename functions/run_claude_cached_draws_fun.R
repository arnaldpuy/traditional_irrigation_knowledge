# Read each document several times (independent draws) against the Claude

run_claude_cached_draws_fun <- function(items, system_prompt, schema, n_draws = 3L,
                                        done = function(custom_id, draw) FALSE,
                                        on_result = function(res) NULL,
                                        model = "claude-fable-5-1",
                                        api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                                        max_tokens = 16000L, effort = "high",
                                        concurrency = 2L, timeout = 900L,
                                        max_tries = 4L) {
  if (!nzchar(api_key)) stop("ANTHROPIC_API_KEY is not set.")
  stopifnot(all(c("custom_id", "doc_text", "user_text") %in% names(items)))

  make_req <- function(doc_text, user_text) {
    httr2::request("https://api.anthropic.com/v1/messages") |>
      httr2::req_headers("x-api-key" = api_key,
                         "anthropic-version" = "2023-06-01",
                         "content-type" = "application/json") |>
      httr2::req_body_json(list(
        model = model,
        max_tokens = max_tokens,
        output_config = list(
          effort = effort,
          format = list(type = "json_schema", schema = schema)),
        system = list(
          list(type = "text", text = system_prompt,
               cache_control = list(type = "ephemeral")),
          list(type = "text", text = paste0("DOCUMENT TEXT:\n", doc_text),
               cache_control = list(type = "ephemeral"))),
        messages = list(list(role = "user", content = user_text))),
        auto_unbox = TRUE) |>
      httr2::req_timeout(timeout) |>
      httr2::req_retry(max_tries = max_tries) |>
      httr2::req_error(is_error = function(resp) FALSE)
  }

  extract <- function(resp) {
    empty <- list(status = "error", text = NA_character_, input_tokens = NA_integer_,
                  cache_creation_input_tokens = NA_integer_,
                  cache_read_input_tokens = NA_integer_, output_tokens = NA_integer_,
                  stop_reason = NA_character_)
    if (inherits(resp, "error") || is.null(resp)) return(empty)
    if (httr2::resp_status(resp) >= 300) {
      empty$status <- sprintf("errored_%d", httr2::resp_status(resp))
      return(empty)
    }
    body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
    txt <- tryCatch(
      paste(vapply(Filter(function(b) b$type == "text", body$content),
                   function(b) b$text, character(1)), collapse = ""),
      error = function(e) NULL)
    if (is.null(txt) || !nzchar(txt)) {
      empty$status <- "errored"
      return(empty)
    }
    u <- body$usage
    num <- function(x) if (is.null(x)) NA_integer_ else as.integer(x)
    list(status = "succeeded", text = txt,
         input_tokens = num(u$input_tokens),
         cache_creation_input_tokens = num(u$cache_creation_input_tokens),
         cache_read_input_tokens = num(u$cache_read_input_tokens),
         output_tokens = num(u$output_tokens),
         stop_reason = if (is.null(body$stop_reason)) NA_character_ else body$stop_reason)
  }

  # One draw over a subset of items, replies handed to on_result at once -------

  run_draw <- function(subset, draw) {
    if (!nrow(subset)) return(NULL)
    reqs <- Map(make_req, subset$doc_text, subset$user_text)
    resps <- httr2::req_perform_parallel(reqs, max_active = concurrency,
                                         on_error = "continue", progress = FALSE)
    out <- data.table::rbindlist(lapply(seq_along(resps), function(i)
      c(list(custom_id = subset$custom_id[i], draw = draw), extract(resps[[i]]))))
    for (i in seq_len(nrow(out))) on_result(out[i])
    out
  }

  results <- list()
  batches <- split(seq_len(nrow(items)), ceiling(seq_len(nrow(items)) / concurrency))
  for (b in seq_along(batches)) {
    batch <- items[batches[[b]]]
    for (k in seq_len(n_draws)) {
      todo <- batch[!mapply(done, custom_id, rep(k, nrow(batch)))]
      results[[length(results) + 1L]] <- run_draw(todo, k)
    }
    message(sprintf("run_claude_cached_draws_fun: batch %d of %d done", b, length(batches)))
  }
  out <- data.table::rbindlist(results, fill = TRUE)
  if (!nrow(out)) return(out)

  # One more pass for whatever failed, document by document ---------------------

  fails <- out[status != "succeeded"]
  if (nrow(fails)) {
    message(sprintf("run_claude_cached_draws_fun: re-firing %d failed reply(ies)", nrow(fails)))
    redo <- data.table::rbindlist(lapply(seq_len(nrow(fails)), function(i)
      run_draw(items[custom_id == fails$custom_id[i]], fails$draw[i])), fill = TRUE)
    out <- rbind(out[status == "succeeded"], redo, fill = TRUE)
  }
  data.table::setorder(out, custom_id, draw)
  out[]
}
