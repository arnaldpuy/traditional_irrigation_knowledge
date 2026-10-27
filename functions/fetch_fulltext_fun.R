# Retrieve the full text of one DOI through a waterfall of legal routes:

fetch_fulltext_fun <- function(doi, dest_dir, mailto = "a.puy@bham.ac.uk") {
  doi <- tolower(trimws(doi))
  slug <- gsub("[^a-z0-9]+", "_", doi)
  ua <- paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
               "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0 Safari/537.36")
  save_pdf <- function(url) {
    dest <- file.path(dest_dir, paste0(slug, ".pdf"))
    resp <- tryCatch(
      httr2::request(url) |> httr2::req_user_agent(ua) |>
        httr2::req_timeout(120) |> httr2::req_retry(max_tries = 3) |>
        httr2::req_perform(path = dest),
      error = function(e) NULL)
    ok <- !is.null(resp) && file.exists(dest) && file.size(dest) > 10000 &&
      identical(readChar(dest, 4L, useBytes = TRUE), "%PDF")
    if (!ok && file.exists(dest)) file.remove(dest)
    if (ok) dest else NULL
  }

  # Route 1: Unpaywall -----------------------------------------------------------

  up <- tryCatch(
    httr2::request(paste0("https://api.unpaywall.org/v2/", doi)) |>
      httr2::req_url_query(email = mailto) |>
      httr2::req_timeout(60) |> httr2::req_perform() |> httr2::resp_body_json(),
    error = function(e) NULL)
  loc <- up$best_oa_location
  if (!is.null(loc) && !is.null(loc$url_for_pdf)) {
    f <- save_pdf(loc$url_for_pdf)
    if (!is.null(f)) return(data.table::data.table(
      doi = doi, route = "unpaywall", file = basename(f), status = "retrieved"))
  }

  # Route 2: CORE ------------------------------------------------------------------

  core_key <- Sys.getenv("CORE_API_KEY")
  if (nzchar(core_key)) {
    core <- tryCatch(
      httr2::request("https://api.core.ac.uk/v3/discover") |>
        httr2::req_headers(Authorization = paste("Bearer", core_key)) |>
        httr2::req_body_json(list(doi = doi)) |>
        httr2::req_timeout(60) |> httr2::req_perform() |> httr2::resp_body_json(),
      error = function(e) NULL)
    if (!is.null(core$fullTextLink)) {
      f <- save_pdf(core$fullTextLink)
      if (!is.null(f)) return(data.table::data.table(
        doi = doi, route = "core", file = basename(f), status = "retrieved"))
    }
  }

  # Route 3: Elsevier full-text API (XML; entitlement-dependent) -------------------

  els_key <- Sys.getenv("SCOPUS_API_KEY")
  if (nzchar(els_key)) {
    dest <- file.path(dest_dir, paste0(slug, ".xml"))
    resp <- tryCatch(
      httr2::request(paste0("https://api.elsevier.com/content/article/doi/", doi)) |>
        httr2::req_headers(`X-ELS-APIKey` = els_key, Accept = "text/xml") |>
        httr2::req_user_agent(ua) |>
        httr2::req_timeout(120) |> httr2::req_perform(path = dest),
      error = function(e) NULL)
    if (!is.null(resp) && file.exists(dest) && file.size(dest) > 20000 &&
        !grepl("ERROR", readChar(dest, 2000L, useBytes = TRUE), fixed = TRUE)) {
      return(data.table::data.table(
        doi = doi, route = "elsevier_tdm", file = basename(dest), status = "retrieved"))
    }
    if (file.exists(dest) && file.size(dest) <= 20000) file.remove(dest)
  }

  data.table::data.table(doi = doi, route = NA_character_, file = NA_character_,
                         status = "manual")
}
