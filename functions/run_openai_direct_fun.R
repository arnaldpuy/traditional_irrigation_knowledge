# Run a set of classification requests against the OpenAI Chat Completions API

run_openai_direct_fun <- function(items, system_prompt,
                                  model = "gpt-5.4",
                                  reasoning_effort = "low",
                                  max_completion_tokens = 2500L,
                                  response_format = list(type = "json_object"),
                                  api_key = Sys.getenv("OPENAI_API_KEY"),
                                  concurrency = 8L, timeout = 180L,
                                  max_passes = 3L) {
  if (!nzchar(api_key)) stop("OPENAI_API_KEY is not set.")
  auth <- paste("Bearer", api_key)

  make_req <- function(user_text) {
    httr2::request("https://api.openai.com/v1/chat/completions") |>
      httr2::req_headers("Authorization" = auth,
                         "content-type" = "application/json") |>
      httr2::req_body_json(list(
        model = model,
        reasoning_effort = reasoning_effort,
        max_completion_tokens = max_completion_tokens,
        response_format = response_format,
        messages = list(
          list(role = "system", content = system_prompt),
          list(role = "user", content = user_text))),
        auto_unbox = TRUE) |>
      httr2::req_timeout(timeout) |>
      httr2::req_retry(max_tries = 4) |>
      httr2::req_error(is_error = function(resp) FALSE)
  }

  extract <- function(resp) {
    if (inherits(resp, "error") || is.null(resp)) return(list("error", NA_character_))
    if (httr2::resp_status(resp) >= 300) return(list("errored", NA_character_))
    body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
    txt <- tryCatch(body$choices[[1]]$message$content, error = function(e) NULL)
    if (is.null(txt) || !nzchar(txt)) return(list("errored", NA_character_))
    list("succeeded", txt)
  }

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
    message(sprintf("run_openai_direct_fun: retry pass %d for %d failed item(s)",
                    pass + 1L, length(fails)))
    redo <- run_pass(items[custom_id %in% fails])
    out <- rbind(out[!custom_id %in% fails], redo)
    pass <- pass + 1L
  }

  out[match(items$custom_id, out$custom_id)]
}
