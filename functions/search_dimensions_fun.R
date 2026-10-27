# Query the Dimensions DSL API and return the hits as a data.table. Logs in

search_dimensions_fun <- function(query_terms, year_min = 2000, year_max = 2026,
                                  max_records = 20000) {
  api_key <- Sys.getenv("DIMENSIONS_API_KEY")
  if (!nzchar(api_key)) stop("DIMENSIONS_API_KEY not set in the environment")
  token <- httr2::request("https://app.dimensions.ai/api/auth.json") |>
    httr2::req_body_json(list(key = api_key)) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    (\(x) x$token)()
  out <- list()
  skip <- 0L
  step <- 500L
  repeat {
    dsl <- sprintf(paste0(
      'search publications in title_abstract_only for "%s" ',
      'where year in [%d:%d] and type = "article" ',
      'return publications[title + authors + year + doi + journal + ',
      'times_cited + document_type] limit %d skip %d'),
      query_terms, year_min, year_max, step, skip)
    body <- httr2::request("https://app.dimensions.ai/api/dsl/v2") |>
      httr2::req_headers(Authorization = paste("JWT", token)) |>
      httr2::req_body_raw(dsl, type = "application/json") |>
      httr2::req_timeout(120) |>
      httr2::req_retry(max_tries = 4) |>
      httr2::req_perform() |>
      httr2::resp_body_json()
    pubs <- body$publications
    if (is.null(pubs) || length(pubs) == 0) break
    out[[length(out) + 1L]] <- data.table::rbindlist(lapply(pubs, function(p) {
      first_author <- if (!is.null(p$authors) && length(p$authors) > 0) {
        paste(p$authors[[1]]$last_name, p$authors[[1]]$first_name, sep = ", ")
      } else NA_character_
      data.table::data.table(
        title = p$title %||% NA_character_,
        first_author = first_author,
        year = as.character(p$year %||% NA),
        doi = p$doi %||% NA_character_,
        source_title = if (!is.null(p$journal)) p$journal$title else NA_character_,
        subtype = p$document_type %||% NA_character_,
        cited_by = as.integer(p$times_cited %||% NA),
        eid = NA_character_)
    }), fill = TRUE)
    skip <- skip + step
    if (skip >= max_records || length(pubs) < step) break
  }
  dt <- data.table::rbindlist(out, fill = TRUE)
  if (nrow(dt) > 0) dt[, database := "dimensions"]
  dt
}
