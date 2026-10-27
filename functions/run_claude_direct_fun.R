# Run a set of classification requests against the Claude Messages API

run_claude_direct_fun <- function(items, system_prompt, schema,
                                  model = "claude-opus-4-8",
                                  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                                  max_tokens = 300L, effort = "medium",
                                  concurrency = 8L, timeout = 120L,
                                  max_passes = 3L, thinking = "disabled") {
  if (!nzchar(api_key)) stop("ANTHROPIC_API_KEY is not set.")
  thinking_field <- if (identical(thinking, "disabled"))
    list(thinking = list(type = "disabled")) else list()

  make_req <- function(user_text) {
    httr2::request("https://api.anthropic.com/v1/messages") |>
      httr2::req_headers("x-api-key" = api_key,
                         "anthropic-version" = "2023-06-01",
                         "content-type" = "application/json") |>
      httr2::req_body_json(c(list(
        model = model,
        max_tokens = max_tokens),
        thinking_field,
        list(output_config = list(
          effort = effort,
          format = list(type = "json_schema", schema = schema)),
        system = list(list(type = "text", text = system_prompt,
                           cache_control = list(type = "ephemeral"))),
        messages = list(list(role = "user", content = user_text)))),
        auto_unbox = TRUE) |>
      httr2::req_timeout(timeout) |>
      httr2::req_retry(max_tries = 4) |>
      httr2::req_error(is_error = function(resp) FALSE)
  }

  extract <- function(resp) {
    if (inherits(resp, "error") || is.null(resp)) return(list("error", NA_character_))
    if (httr2::resp_status(resp) >= 300) return(list("errored", NA_character_))
    body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
    txt <- tryCatch(
      paste(vapply(Filter(function(b) b$type == "text", body$content),
                   function(b) b$text, character(1)), collapse = ""),
      error = function(e) NULL)
    if (is.null(txt) || !nzchar(txt)) return(list("errored", NA_character_))
    list("succeeded", txt)
  }

  # One concurrent sweep over a subset of items -------------------------------

  run_pass <- function(subset) {
    reqs <- lapply(subset$user_text, make_req)
    resps <- httr2::req_perform_parallel(reqs, max_active = concurrency,
                                         on_error = "continue")
    ex <- lapply(resps, extract)
    data.table::data.table(
      custom_id = subset$custom_id,
      status = vapply(ex, `[[`, character(1), 1),
      text = vapply(ex, `[[`, character(1), 2))
  }

  out <- run_pass(items)
  pass <- 1L
  while (pass < max_passes && out[status != "succeeded", .N] > 0) {
    fails <- out[status != "succeeded", custom_id]
    message(sprintf("run_claude_direct_fun: retry pass %d for %d failed item(s)",
                    pass + 1L, length(fails)))
    redo <- run_pass(items[custom_id %in% fails])
    out <- rbind(out[!custom_id %in% fails], redo)
    pass <- pass + 1L
  }

  out[match(items$custom_id, out$custom_id)]
}
