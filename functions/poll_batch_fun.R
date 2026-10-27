poll_batch_fun <- function(batch_id, api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                           interval = 60, timeout = 24 * 3600,
                           anthropic_version = "2023-06-01") {

  if (!nzchar(api_key)) stop("ANTHROPIC_API_KEY is not set.")

  url <- paste0("https://api.anthropic.com/v1/messages/batches/", batch_id)
  waited <- 0
  fails <- 0
  max_fails <- 10L
  repeat {
    batch <- tryCatch(
      httr2::request(url) |>
        httr2::req_headers("x-api-key" = api_key,
                           "anthropic-version" = anthropic_version) |>
        httr2::req_retry(max_tries = 4) |>
        httr2::req_timeout(120) |>
        httr2::req_perform() |>
        httr2::resp_body_json(),
      error = function(e) e)

    if (inherits(batch, "error")) {
      fails <- fails + 1
      message(sprintf("poll %d/%d failed (%s); batch %s still running, retrying",
                      fails, max_fails, conditionMessage(batch), batch_id))
      if (fails >= max_fails)
        stop("Polling failed ", max_fails, " times in a row. The batch is NOT ",
             "lost: resume with code/41_resume_batch_arm.R and batch id ",
             batch_id, " -- do NOT re-run the arm, it would submit a second batch.")
      Sys.sleep(interval)
      waited <- waited + interval
      next
    }
    fails <- 0

    if (identical(batch$processing_status, "ended")) return(batch)
    if (waited >= timeout) stop("Batch polling timed out after ", timeout, "s.")

    counts <- batch$request_counts
    message(sprintf("status=%s processing=%s succeeded=%s errored=%s",
                    batch$processing_status, counts$processing,
                    counts$succeeded, counts$errored))
    Sys.sleep(interval)
    waited <- waited + interval
  }
}
