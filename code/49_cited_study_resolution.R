# CITED-STUDY RESOLUTION #######################################################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "xml2", "httr2", "stringi", "openxlsx"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"), pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

`%||%` <- function(a, b) if (is.null(a)) b else a
dir_in <- here("datasets", "output", "link_extraction", "fable_full")
ft_dir <- here("datasets", "inputs", "literature_corpus", "fulltexts")
cache_file <- file.path(dir_in, "crossref_cache.rds")
links <- fread(file.path(dir_in, "link_extraction.csv"),
               colClasses = list(character = c("passage_verified", "passage_overlap",
                                               "reference_overlap")))
links[, row_id := .I]
manifest <- fread(here("datasets", "output", "literature_fulltext_manifest.csv"))
links[, src := tools::file_ext(manifest$file[match(rec_id, manifest$rec_id)])]
cat(sprintf("%d rows; %d with a reference entry, %d citation-only (%d from XML reviews)\n",
            nrow(links), links[reference_entry != "not found", .N],
            links[reference_entry == "not found", .N],
            links[reference_entry == "not found" & src == "xml", .N]))

norm_key <- function(x) {
  x <- stri_trans_general(stri_trans_nfkc(x), "Latin-ASCII")
  tolower(gsub("[^A-Za-z0-9]", "", x))
}
title_tokens <- function(x) {
  t <- strsplit(gsub("[^a-z0-9 ]", " ", tolower(stri_trans_general(x, "Latin-ASCII"))), " +")[[1]]
  t[nchar(t) > 2]
}

# XML BIBLIOGRAPHIES ###########################################################

xml_ids <- unique(links[src == "xml", rec_id])
bib <- rbindlist(lapply(xml_ids, function(id) {
  x <- read_xml(file.path(ft_dir, manifest[rec_id == id, file]))
  refs <- xml_find_all(x, "//*[local-name()='bib-reference']")
  first <- function(r, xp) { v <- xml_text(xml_find_first(r, xp)); if (is.na(v)) NA_character_ else trimws(v) }
  data.table(
    rec_id = id,
    label = vapply(refs, first, character(1), ".//*[local-name()='label']"),
    doi = tolower(vapply(refs, first, character(1), ".//*[local-name()='doi']")),
    year = suppressWarnings(as.integer(substr(vapply(refs, first, character(1), ".//*[local-name()='date']"), 1, 4))),
    title = vapply(refs, first, character(1), ".//*[local-name()='contribution']//*[local-name()='maintitle']"),
    surname1 = vapply(refs, first, character(1), ".//*[local-name()='author']/*[local-name()='surname']"),
    journal = vapply(refs, first, character(1), ".//*[local-name()='host']//*[local-name()='maintitle']"),
    entry = vapply(refs, function(r) gsub("\\s+", " ", xml_text(r)), character(1)))
}))
bib[, label_key := norm_key(gsub("et al\\.?|\\band\\b|&", "", label, ignore.case = TRUE))]
bib[, label_num := suppressWarnings(as.integer(gsub("[^0-9]", "", label)))]
bib[!grepl("^\\[?[0-9]+\\]?$", label), label_num := NA_integer_]
cat(sprintf("XML bibliographies: %d reviews, %d references, %d with DOI\n",
            uniqueN(bib$rec_id), nrow(bib), sum(!is.na(bib$doi))))

# Parse a citation string into label keys -------------------------------------

parse_citation <- function(cit) {
  cit <- gsub("–|—", "-", cit)
  if (grepl("^\\[?[0-9]", cit)) {                      # numeric: [4,11,81-88]
    parts <- strsplit(gsub("[^0-9,-]", "", cit), ",")[[1]]
    nums <- unlist(lapply(parts, function(p) {
      if (grepl("-", p)) { r <- as.integer(strsplit(p, "-")[[1]]); if (length(r) == 2 && r[2] >= r[1] && r[2] - r[1] < 40) seq(r[1], r[2]) else r[1] }
      else as.integer(p) }))
    return(data.table(kind = "num", num = nums[!is.na(nums)], ckey = NA_character_))
  }
  parts <- trimws(strsplit(cit, ";")[[1]])
  rbindlist(lapply(parts, function(p) {
    yrs <- regmatches(p, gregexpr("(19|20)[0-9]{2}[a-z]?", p))[[1]]
    if (!length(yrs)) return(NULL)
    auth <- trimws(sub("\\(?\\s*(19|20)[0-9]{2}.*$", "", p))
    auth <- gsub("\\bin\\s*$", "", auth)
    extra <- regmatches(p, gregexpr("(?<=[0-9][a-z], )[a-z](?=\\b)", p, perl = TRUE))[[1]]
    if (length(extra)) yrs <- c(yrs, paste0(substr(yrs[length(yrs)], 1, 4), extra))
    data.table(kind = "ay", num = NA_integer_,
               ckey = norm_key(paste0(gsub("et al\\.?|\\band\\b|&", "", auth, ignore.case = TRUE), yrs)))
  }))
}

co <- links[reference_entry == "not found" & src == "xml"]
matched <- rbindlist(lapply(seq_len(nrow(co)), function(i) {
  p <- parse_citation(co$citation_in_text[i])
  if (is.null(p) || !nrow(p)) return(NULL)
  b <- bib[rec_id == co$rec_id[i]]
  hits <- rbindlist(lapply(seq_len(nrow(p)), function(k) {
    h <- if (p$kind[k] == "num") b[label_num == p$num[k]] else b[label_key == p$ckey[k]]
    if (!nrow(h) && p$kind[k] == "ay")              # year without letter vs label with letter, or vice versa
      h <- b[sub("[a-z]$", "", label_key) == sub("[a-z]$", "", p$ckey[k])]
    if (nrow(h)) data.table(row_id = co$row_id[i], cite_key = if (p$kind[k] == "num") as.character(p$num[k]) else p$ckey[k], h[1])
  }))
  hits
}), fill = TRUE)
cat(sprintf("XML citation matching: %d of %d citation-only rows matched to %d label hits\n",
            uniqueN(matched$row_id), nrow(co), nrow(matched)))
unmatched_xml <- co[!row_id %in% matched$row_id, .(row_id, rec_id, citation_in_text)]

# PDF ENTRIES: DOI IN THE STRING ###############################################

pe <- links[reference_entry != "not found", .(row_id, rec_id, entry = reference_entry)]
pe[, entry_key := norm_key(substr(entry, 1, 120))]
extract_doi <- function(x) {
  m <- regmatches(x, regexpr("10\\.[0-9]{4,9}/[^[:space:]\"<>]+", x))
  if (!length(m)) return(NA_character_)
  tolower(sub("[[:punct:]]+$", "", m))
}
pe[, doi := vapply(entry, extract_doi, character(1), USE.NAMES = FALSE)]
cat(sprintf("PDF entries: %d rows, %d distinct, %d carry a DOI\n", nrow(pe), uniqueN(pe$entry_key), sum(!is.na(pe$doi))))

# CROSSREF ####################################################################

cache <- if (file.exists(cache_file)) readRDS(cache_file) else list(bib = list(), doi = list())
save_cache <- function() saveRDS(cache, cache_file)

year_of <- function(x) suppressWarnings(as.integer(regmatches(x, regexpr("(19|20)[0-9]{2}", x))))
surname_of <- function(e) {                      # first alphabetic token after any "[12]" / "12." label or "Name et al., 2009" prefix
  e <- sub("^\\s*\\[?[0-9]+[].]?\\s*", "", e)
  e <- stringi::stri_trans_general(e, "Latin-ASCII")
  m <- regmatches(e, regexpr("[A-Za-z][A-Za-z'-]{2,}", e))
  if (length(m)) m else ""
}
bib_pdf <- unique(pe[is.na(doi), .(key = entry_key, query = entry,
                                   year_hint = vapply(entry, function(e) { y <- year_of(e); if (length(y)) y else NA_integer_ }, integer(1), USE.NAMES = FALSE),
                                   surname_hint = vapply(entry, surname_of, character(1), USE.NAMES = FALSE), title_hint = NA_character_)], by = "key")
bib_xml <- unique(matched[is.na(doi), .(key = paste0("xml_", rec_id, "_", label_key),
                                        query = paste(surname1, year, title, journal),
                                        year_hint = year, surname_hint = surname1, title_hint = title)], by = "key")
need_bib <- rbind(bib_pdf, bib_xml)[!key %in% names(cache$bib)]
cat(sprintf("Crossref bibliographic queries to run: %d (cached %d)\n", nrow(need_bib), length(cache$bib)))
for (i in seq_len(nrow(need_bib))) {
  cache$bib[[need_bib$key[i]]] <- crossref_lookup_fun(need_bib$query[i])
  if (i %% 100 == 0) { save_cache(); cat(sprintf("  %d queries done\n", i)) }
}
save_cache()

accept_hit <- function(cands, year_hint, surname_hint, title_hint, entry_text) {
  if (is.null(cands) || !nrow(cands)) return(NULL)
  cands <- copy(cands)
  cands[, year_ok := !is.na(year) & !is.na(year_hint) & abs(year - year_hint) <= 1]
  sn <- norm_key(surname_hint)
  cands[, surname_ok := nzchar(sn) & grepl(sn, norm_key(authors), fixed = TRUE)]
  ref_text <- if (!is.na(title_hint)) title_hint else entry_text
  cands[, title_ok := vapply(title, function(t) {
    tt <- title_tokens(t); if (length(tt) < 2) return(FALSE)
    mean(tt %in% title_tokens(ref_text)) >= 0.6 }, logical(1))]
  ok <- cands[year_ok & surname_ok & title_ok]
  if (nrow(ok)) ok[1] else NULL
}
resolve_bib <- function(dt) rbindlist(lapply(seq_len(nrow(dt)), function(i) {
  hit <- accept_hit(cache$bib[[dt$key[i]]], dt$year_hint[i], dt$surname_hint[i], dt$title_hint[i], dt$query[i])
  if (is.null(hit)) data.table(qkey = dt$key[i], cr_doi = NA_character_, cr_title = NA_character_, cr_year = NA_integer_,
                               cr_authors = NA_character_, cr_journal = NA_character_)
  else data.table(qkey = dt$key[i], cr_doi = hit$doi, cr_title = hit$title, cr_year = hit$year,
                  cr_authors = hit$authors, cr_journal = hit$journal)
}))
res_pdf <- resolve_bib(bib_pdf)
res_xml <- resolve_bib(bib_xml)
cat(sprintf("Crossref accepted: PDF entries %d of %d; XML references %d of %d\n",
            sum(!is.na(res_pdf$cr_doi)), nrow(res_pdf), sum(!is.na(res_xml$cr_doi)), nrow(res_xml)))

# Metadata for DOIs found in PDF entries --------------------------------------

need_doi <- setdiff(unique(na.omit(pe$doi)), names(cache$doi))
cat(sprintf("Crossref DOI look-ups to run: %d (cached %d)\n", length(need_doi), length(cache$doi)))
for (i in seq_along(need_doi)) {
  cache$doi[[need_doi[i]]] <- crossref_doi_fun(need_doi[i]) %||% data.table()
  if (i %% 100 == 0) { save_cache(); cat(sprintf("  %d DOIs done\n", i)) }
}
save_cache()
doi_meta <- rbindlist(Filter(function(x) nrow(x) > 0, cache$doi), fill = TRUE)
bad_doi <- setdiff(unique(na.omit(pe$doi)), doi_meta$doi)
cat(sprintf("DOIs in PDF entries not resolving at Crossref: %d\n", length(bad_doi)))

# ASSEMBLE ####################################################################

first_of <- function(a) sub("\\|.*$", "", a)

pe[doi %in% bad_doi, doi := NA_character_]
pe[!is.na(doi), route := "doi_in_entry"]
pe <- merge(pe, res_pdf, by.x = "entry_key", by.y = "qkey", all.x = TRUE)
pe[is.na(route) & !is.na(cr_doi), `:=`(doi = cr_doi, route = "crossref")]
pe[is.na(route), route := "unresolved"]
pe <- merge(pe, doi_meta[, .(doi, m_title = title, m_year = year, m_authors = authors, m_journal = journal)],
            by = "doi", all.x = TRUE)
pe[, `:=`(title = fifelse(route == "doi_in_entry", m_title, cr_title),
          year = fifelse(route == "doi_in_entry", m_year, cr_year),
          first_author = fifelse(route == "doi_in_entry", first_of(m_authors), first_of(cr_authors)),
          journal = fifelse(route == "doi_in_entry", m_journal, cr_journal))]
pe[route == "unresolved", `:=`(first_author = vapply(entry, surname_of, character(1), USE.NAMES = FALSE),
                               year = vapply(entry, function(e) { y <- year_of(e); if (length(y)) y else NA_integer_ }, integer(1), USE.NAMES = FALSE))]
pdf_part <- pe[, .(row_id, rec_id, entry, doi, route, title, year, first_author, journal)]

xm <- copy(matched)[, qkey := paste0("xml_", rec_id, "_", label_key)]
xm <- merge(xm, res_xml, by = "qkey", all.x = TRUE)
xm[!is.na(doi), route := "xml_doi"]
xm[is.na(route) & !is.na(cr_doi), `:=`(doi = cr_doi, route = "crossref")]
xm[is.na(route), route := "unresolved"]
xml_part <- xm[, .(row_id, rec_id, entry, doi, route, title, year, first_author = surname1, journal)]

cited_rows <- rbind(pdf_part, xml_part)
cited_rows[, title_key := vapply(fcoalesce(title, entry), function(x)
  paste(head(title_tokens(x), 4), collapse = ""), character(1), USE.NAMES = FALSE)]
cited_rows[, cited_id := fifelse(!is.na(doi), paste0("doi:", doi),
                                 paste0("key:", norm_key(paste0(first_author, year, title_key))))]
cited_rows[is.na(first_author) & is.na(doi), cited_id := paste0("key:", norm_key(substr(entry, 1, 80)))]

# CITED STUDIES ###############################################################

cited <- cited_rows[, .(doi = first(na.omit(doi)) %||% NA_character_,
                        title = first(na.omit(title)), year = first(na.omit(year)),
                        first_author = first(na.omit(first_author)), journal = first(na.omit(journal)),
                        route = first(route), n_rows = .N, n_reviews = uniqueN(rec_id),
                        entry_example = first(entry)), by = cited_id]
cited[, route := fifelse(route == "unresolved", "unresolved", route)]
setorder(cited, -n_reviews, -n_rows)
cited[, cited_no := .I]

link_cols <- links[, .(row_id, rec_id, review_title = title, technology_reviewed, technology, `function`,
                       direction, link_type, claim_type, citation_group, citation_in_text)]
link_rows_cited <- merge(link_cols, cited_rows[, .(row_id, cited_id, doi, route)], by = "row_id")
link_rows_cited <- merge(link_rows_cited, cited[, .(cited_id, cited_no, cited_title = title, cited_year = year, cited_first_author = first_author)], by = "cited_id")
setorder(link_rows_cited, row_id)

cell_by_study <- link_rows_cited[, .(n_links = uniqueN(citation_group), n_reviews = uniqueN(rec_id),
                                     directions = paste(sort(unique(direction)), collapse = "/")),
                                 by = .(technology, `function`, cited_id, cited_no)]

unresolved <- rbind(
  cited_rows[route == "unresolved", .(row_id, rec_id, kind = "entry without accepted match", text = entry)],
  unmatched_xml[, .(row_id, rec_id, kind = "XML citation not matched to a label", text = citation_in_text)],
  links[reference_entry == "not found" & src == "pdf", .(row_id, rec_id, kind = "PDF citation without entry", text = citation_in_text)])

fwrite(cited, file.path(dir_in, "cited_studies.csv"))
fwrite(link_rows_cited, file.path(dir_in, "link_rows_cited.csv"))
fwrite(cell_by_study, file.path(dir_in, "cell_by_study.csv"))
fwrite(unresolved, file.path(dir_in, "unresolved.csv"))
write.xlsx(list(cited_studies = cited, link_rows = link_rows_cited, cell_by_study = cell_by_study,
                unresolved = unresolved), file.path(dir_in, "cited_studies.xlsx"), overwrite = TRUE)

# REPORT ######################################################################

cat("\n=== Cited-study resolution ===\n")
cat(sprintf("link rows resolved to at least one study: %d of %d\n", uniqueN(cited_rows$row_id), nrow(links)))
print(cited_rows[, .N, by = route])
cat(sprintf("distinct cited studies: %d (with DOI %d); cited by two or more reviews: %d\n",
            nrow(cited), sum(!is.na(cited$doi)), cited[n_reviews >= 2, .N]))
cat("unresolved rows by kind:\n"); print(unresolved[, .N, by = kind])
cat("\nstudies per cell (technology x function), distinct studies:\n")
print(dcast(cell_by_study[, uniqueN(cited_id), by = .(technology, `function`)], technology ~ `function`, value.var = "V1", fill = 0))
