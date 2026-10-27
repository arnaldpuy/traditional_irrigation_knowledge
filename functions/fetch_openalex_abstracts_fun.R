# Fetch abstracts for a vector of DOIs from OpenAlex (keyless; polite pool via

fetch_openalex_abstracts_fun <- function(dois, mailto = "a.puy@bham.ac.uk") {
  dois <- unique(tolower(trimws(dois)))
  dois <- dois[nzchar(dois) & !is.na(dois)]
  if (length(dois) == 0) return(data.table::data.table())
  rebuild <- function(inv) {
    if (is.null(inv) || length(inv) == 0) return(NA_character_)
    pos <- unlist(inv, use.names = FALSE)
    words <- rep(names(inv), lengths(inv))
    paste(words[order(pos)], collapse = " ")
  }
  query <- function(doi_set) {
    resp <- httr2::request("https://api.openalex.org/works") |>
      httr2::req_url_query(
        filter = paste0("doi:", paste(doi_set, collapse = "|")),
        select = "doi,abstract_inverted_index",
        `per-page` = 50, mailto = mailto) |>
      httr2::req_timeout(60) |>
      httr2::req_retry(max_tries = 4) |>
      httr2::req_perform()
    res <- httr2::resp_body_json(resp)$results
    if (length(res) == 0) return(data.table::data.table())
    data.table::rbindlist(lapply(res, function(w) data.table::data.table(
      doi_norm = sub("^https://doi.org/", "", tolower(w$doi %||% "")),
      abstract = rebuild(w$abstract_inverted_index))))
  }

  # DOIs with a comma can never ride in an OR filter; they go per-DOI ---------

  batchable <- dois[!grepl(",", dois, fixed = TRUE)]
  loners <- setdiff(dois, batchable)
  batches <- if (length(batchable) > 0)
    split(batchable, ceiling(seq_along(batchable) / 50)) else list()
  n_failed <- 0L
  out <- lapply(batches, function(b) {
    r <- tryCatch(query(b), error = function(e) NULL)
    if (!is.null(r)) return(r)
    data.table::rbindlist(lapply(b, function(d) {
      r1 <- tryCatch(query(d), error = function(e) NULL)
      if (is.null(r1)) n_failed <<- n_failed + 1L
      r1
    }), fill = TRUE)
  })
  out <- c(out, lapply(loners, function(d) {
    r1 <- tryCatch(query(d), error = function(e) NULL)
    if (is.null(r1)) n_failed <<- n_failed + 1L
    r1
  }))
  if (n_failed > 0) message(sprintf(
    "fetch_openalex_abstracts_fun: %d DOI(s) skipped after failed requests",
    n_failed))
  data.table::rbindlist(out, fill = TRUE)
}
