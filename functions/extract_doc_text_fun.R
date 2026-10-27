# Extract plain text from a retrieved full text for LLM review-coding. Handles

extract_doc_text_fun <- function(path, max_chars = 90000L) {
  ext <- tolower(tools::file_ext(path))
  out <- data.table::data.table(
    file = basename(path), format = ext, source = NA_character_,
    title = NA_character_, abstract = NA_character_, body = NA_character_,
    n_chars = 0L, truncated = FALSE)

  if (!file.exists(path)) {
    out[, source := "missing"]
    return(out)
  }

  if (ext == "xml") {
    x <- tryCatch(xml2::read_xml(path), error = function(e) NULL)
    if (is.null(x)) {
      out[, source := "xml_unreadable"]
      return(out)
    }
    ln <- function(node, tag)
      xml2::xml_find_all(node, sprintf(".//*[local-name()='%s']", tag))
    core <- xml2::xml_find_first(x, "//*[local-name()='coredata']")
    title_v <- NA_character_
    abstract_v <- NA_character_
    if (!is.na(core)) {
      title_v <- paste(xml2::xml_text(ln(core, "title")), collapse = " ")
      abstract_v <- paste(xml2::xml_text(ln(core, "description")), collapse = " ")
    }
    body_nodes <- xml2::xml_find_all(
      x, "//*[local-name()='section-title' or local-name()='para']")
    body_v <- paste(xml2::xml_text(body_nodes), collapse = "\n")
    out$title <- trimws(title_v)
    out$abstract <- trimws(abstract_v)
    out$body <- trimws(body_v)
    out$source <- fifelse(nchar(out$body) > 500, "xml_body", "xml_abstract_only")

  } else if (ext == "pdf") {
    txt <- tryCatch(paste(pdftools::pdf_text(path), collapse = "\n"),
                    error = function(e) NA_character_)
    out[, `:=`(body = trimws(txt),
               source = fifelse(is.na(txt) || nchar(txt) < 500,
                                "pdf_unreadable", "pdf"))]
  } else {
    out[, source := "unsupported"]
  }

  # Cap the body, preserving head + tail --------------------------------------

  b <- out$body
  if (!is.na(b) && nchar(b) > max_chars) {
    head_n <- as.integer(max_chars * 0.6)
    tail_n <- max_chars - head_n
    b <- paste0(substr(b, 1, head_n), "\n\n[... middle omitted ...]\n\n",
                substr(b, nchar(b) - tail_n + 1, nchar(b)))
    out[, `:=`(body = b, truncated = TRUE)]
  }
  out[, n_chars := fifelse(is.na(body), 0L, nchar(body))]
  out[]
}
