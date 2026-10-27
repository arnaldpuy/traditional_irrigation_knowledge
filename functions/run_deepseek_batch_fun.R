run_deepseek_batch_fun <- function(items, system_prompt,
                                   model = "deepseek-chat",
                                   api_key = Sys.getenv("DEEPSEEK_API_KEY"),
                                   temperature = 0, max_tokens = 800L,
                                   concurrency = 8L) {

  if (!nzchar(api_key)) stop("DEEPSEEK_API_KEY is not set.")

  make_req <- function(user_text) {
    httr2::request("https://api.deepseek.com/chat/completions") |>
      httr2::req_headers("Authorization" = paste("Bearer", api_key),
                         "content-type" = "application/json") |>
      httr2::req_body_json(list(
        model = model, temperature = temperature, max_tokens = max_tokens,
        response_format = list(type = "json_object"),
        messages = list(
          list(role = "system", content = system_prompt),
          list(role = "user", content = user_text))),
        auto_unbox = TRUE) |>
      httr2::req_retry(max_tries = 4) |>
      httr2::req_error(is_error = function(resp) FALSE)   # inspect per response
  }

  reqs <- lapply(items$user_text, make_req)
  resps <- httr2::req_perform_parallel(reqs, max_active = concurrency,
                                       on_error = "continue")

  extract <- function(resp) {
    if (inherits(resp, "error") || is.null(resp)) return(list("error", NA_character_))
    if (httr2::resp_status(resp) >= 300) return(list("errored", NA_character_))
    body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
    txt <- tryCatch(body$choices[[1]]$message$content, error = function(e) NULL)
    if (is.null(txt)) return(list("errored", NA_character_))
    list("succeeded", txt)
  }

  parsed <- lapply(resps, extract)
  data.table::data.table(
    custom_id = items$custom_id,
    status = vapply(parsed, `[[`, character(1), 1L),
    text = vapply(parsed, `[[`, character(1), 2L))
}
