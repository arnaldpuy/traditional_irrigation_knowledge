# Query the Scopus Search API and return the hits as a data.table. Sends a

search_scopus_fun <- function(query, date_range = "2000-2026", max_records = 2000) {
  api_key <- Sys.getenv("SCOPUS_API_KEY")
  if (!nzchar(api_key)) stop("SCOPUS_API_KEY not set in the environment")
  base_req <- httr2::request("https://api.elsevier.com/content/search/scopus") |>
    httr2::req_headers(`X-ELS-APIKey` = api_key, Accept = "application/json") |>
    httr2::req_user_agent(paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
                                 "AppleWebKit/537.36 (KHTML, like Gecko) ",
                                 "Chrome/126.0 Safari/537.36")) |>
    httr2::req_timeout(120) |>
    httr2::req_retry(max_tries = 4)
  out <- list()
  start <- 0L
  fetched <- 0L
  repeat {
    resp <- base_req |>
      httr2::req_url_query(query = query, date = date_range, start = start,
                           count = 25, view = "STANDARD") |>
      httr2::req_perform()
    body <- httr2::resp_body_json(resp)
    sr <- body[["search-results"]]
    total <- as.integer(sr[["opensearch:totalResults"]])
    entries <- sr[["entry"]]
    if (is.null(entries) || length(entries) == 0 ||
        !is.null(entries[[1]][["error"]])) break
    out[[length(out) + 1L]] <- data.table::rbindlist(lapply(entries, function(e) {
      data.table::data.table(
        title = e[["dc:title"]] %||% NA_character_,
        first_author = e[["dc:creator"]] %||% NA_character_,
        year = substr(e[["prism:coverDate"]] %||% "", 1, 4),
        doi = e[["prism:doi"]] %||% NA_character_,
        source_title = e[["prism:publicationName"]] %||% NA_character_,
        subtype = e[["subtypeDescription"]] %||% NA_character_,
        cited_by = as.integer(e[["citedby-count"]] %||% NA),
        eid = e[["eid"]] %||% NA_character_)
    }), fill = TRUE)
    fetched <- fetched + length(entries)
    if (fetched >= min(total, max_records, 5000L)) break
    start <- start + length(entries)
  }
  dt <- data.table::rbindlist(out, fill = TRUE)
  if (nrow(dt) > 0) dt[, database := "scopus"]
  dt
}

`%||%` <- function(a, b) if (is.null(a)) b else a
