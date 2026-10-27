fetch_batch_results_fun <- function(batch,
                                    api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                                    anthropic_version = "2023-06-01") {

  if (!nzchar(api_key)) stop("ANTHROPIC_API_KEY is not set.")
  if (is.null(batch$results_url)) stop("Batch has no results_url yet.")

  raw <- httr2::request(batch$results_url) |>
    httr2::req_headers("x-api-key" = api_key,
                       "anthropic-version" = anthropic_version) |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  lines <- raw[nzchar(raw)]
  lines <- unlist(strsplit(raw, "\n", fixed = TRUE))
  lines <- lines[nzchar(trimws(lines))]

  parsed <- lapply(lines, function(ln) {
    rec <- jsonlite::fromJSON(ln, simplifyVector = FALSE)
    status <- rec$result$type
    text <- NA_character_
    if (identical(status, "succeeded")) {
      blocks <- rec$result$message$content
      txt <- Filter(function(b) identical(b$type, "text"), blocks)
      if (length(txt)) text <- txt[[1]]$text
    }
    data.table::data.table(custom_id = rec$custom_id, status = status,
                           text = text)
  })

  data.table::rbindlist(parsed)
}
