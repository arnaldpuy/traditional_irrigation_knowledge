submit_batch_fun <- function(requests, api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                             anthropic_version = "2023-06-01", max_tries = 6L,
                             beta = "extended-cache-ttl-2025-04-11") {

  if (!nzchar(api_key)) stop("ANTHROPIC_API_KEY is not set.")

  perform <- function() {
    httr2::request("https://api.anthropic.com/v1/messages/batches") |>
      httr2::req_headers("x-api-key" = api_key,
                         "anthropic-version" = anthropic_version,
                         "anthropic-beta" = beta,
                         "content-type" = "application/json") |>
      httr2::req_body_json(list(requests = requests), auto_unbox = TRUE) |>
      httr2::req_timeout(600) |>
      httr2::req_retry(max_tries = 4) |>
      httr2::req_perform()
  }

  for (attempt in seq_len(max_tries)) {
    resp <- tryCatch(perform(), error = function(e) e)
    if (!inherits(resp, "error")) return(httr2::resp_body_json(resp))
    message(sprintf("submit attempt %d/%d failed: %s", attempt, max_tries,
                    conditionMessage(resp)))
    if (attempt < max_tries) Sys.sleep(min(60, 2^attempt))
  }
  stop("Batch submission failed after ", max_tries, " attempts.")
}
