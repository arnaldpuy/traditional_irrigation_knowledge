# Queries the Crossref works API once for a free-text bibliographic string and

crossref_lookup_fun <- function(entry, rows = 3L, mailto = "a.puy@bham.ac.uk",
                                pause = 0.15) {
  Sys.sleep(pause)
  req <- httr2::request("https://api.crossref.org/works") |>
    httr2::req_url_query(query.bibliographic = substr(entry, 1, 600), rows = rows,
                         select = "DOI,title,author,issued,score,container-title") |>
    httr2::req_user_agent(sprintf("irrigators_assumptions/0.1 (mailto:%s)", mailto)) |>
    httr2::req_retry(max_tries = 4, backoff = function(i) 2^i) |>
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  empty <- data.table::data.table(doi = character(), title = character(),
                                  year = integer(), authors = character(),
                                  journal = character(), score = numeric())
  if (is.null(resp) || httr2::resp_status(resp) != 200) return(empty)
  items <- tryCatch(httr2::resp_body_json(resp)$message$items, error = function(e) NULL)
  if (!length(items)) return(empty)
  data.table::rbindlist(lapply(items, function(it) data.table::data.table(
    doi = tolower(it$DOI %||% NA_character_),
    title = if (length(it$title)) it$title[[1]] else NA_character_,
    year = tryCatch(as.integer(it$issued$`date-parts`[[1]][[1]]), error = function(e) NA_integer_),
    authors = paste(vapply(it$author %||% list(), function(a) a$family %||% "", character(1)),
                    collapse = "|"),
    journal = if (length(it$`container-title`)) it$`container-title`[[1]] else NA_character_,
    score = as.numeric(it$score %||% NA_real_))), fill = TRUE)
}

crossref_doi_fun <- function(doi, mailto = "a.puy@bham.ac.uk", pause = 0.15) {
  Sys.sleep(pause)
  req <- httr2::request(paste0("https://api.crossref.org/works/", utils::URLencode(doi, reserved = TRUE))) |>
    httr2::req_user_agent(sprintf("irrigators_assumptions/0.1 (mailto:%s)", mailto)) |>
    httr2::req_retry(max_tries = 4, backoff = function(i) 2^i) |>
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) != 200) return(NULL)
  it <- tryCatch(httr2::resp_body_json(resp)$message, error = function(e) NULL)
  if (is.null(it)) return(NULL)
  data.table::data.table(
    doi = tolower(it$DOI), title = if (length(it$title)) it$title[[1]] else NA_character_,
    year = tryCatch(as.integer(it$issued$`date-parts`[[1]][[1]]), error = function(e) NA_integer_),
    authors = paste(vapply(it$author %||% list(), function(a) a$family %||% "", character(1)), collapse = "|"),
    journal = if (length(it$`container-title`)) it$`container-title`[[1]] else NA_character_)
}
