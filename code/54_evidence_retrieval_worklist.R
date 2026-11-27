# STAGE-2 RETRIEVAL: REPOSITORY COPIES AND THE MANUAL WORKLIST ################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "httr2", "digest", "openxlsx"))

dir_in <- here("datasets", "output", "link_extraction", "fable_full")
dest <- Sys.getenv("STAGE2_PDF_DIR", "/Volumes/LaCieData/irrigators_assumptions/cited_papers")
mailto <- "a.puy@bham.ac.uk"
ua <- sprintf("irrigators_assumptions/0.1 (mailto:%s)", mailto)
manifest_file <- file.path(dir_in, Sys.getenv("STAGE2_MANIFEST_FILE", "stage2_retrieval_manifest.csv"))
worklist_stem <- Sys.getenv("STAGE2_WORKLIST_STEM", "stage2_manual_worklist")
m <- fread(manifest_file, colClasses = list(character = c("date", "sha256", "note", "file", "route")))
m[status == "retrieved" & (is.na(date) | !nzchar(date)), date := as.character(Sys.Date())]
sample_dt <- fread(file.path(dir_in, Sys.getenv("STAGE2_SAMPLE_FILE", "stage2_sample.csv")))

# HAND-SAVED FILES ############################################################

on_disk <- list.files(dest)
m[, save_as := fifelse(!is.na(doi) & nzchar(doi), gsub("[^a-z0-9]+", "_", tolower(doi)),
                       sprintf("cited_%04d", cited_no))]
hand <- m[status != "retrieved"]
hand[, file_found := vapply(save_as, function(s) {
  hit <- on_disk[sub("\\..*$", "", on_disk) == s]
  if (length(hit)) hit[1] else NA_character_
}, character(1))]
hand <- hand[!is.na(file_found)]
if (nrow(hand)) {
  m[hand, `:=`(route = "manual_download", file = i.file_found, status = "retrieved", note = "hand download",
               sha256 = vapply(file.path(dest, i.file_found), digest, character(1), algo = "sha256", file = TRUE),
               date = as.character(Sys.Date())), on = "cited_id"]
}
m[, save_as := NULL]
fwrite(m, manifest_file)
cat(sprintf("hand-saved files registered: %d\n", nrow(hand)))

# REPOSITORY COPIES ############################################################

miss <- m[status != "retrieved" & !is.na(doi) & nzchar(doi)]
oa_info <- rbindlist(lapply(miss$doi, function(d) {
  up <- tryCatch(request(paste0("https://api.unpaywall.org/v2/", d)) |>
                   req_url_query(email = mailto) |> req_timeout(30) |> req_perform() |> resp_body_json(),
                 error = function(e) NULL)
  locs <- up$oa_locations
  if (!length(locs)) return(data.table(doi = d, is_oa = isTRUE(up$is_oa), publisher_pdf = NA_character_,
                                       repo_pdfs = NA_character_, landing = NA_character_))
  pdfs <- vapply(locs, function(l) l$url_for_pdf %||% NA_character_, character(1))
  host <- vapply(locs, function(l) l$host_type %||% NA_character_, character(1))
  data.table(doi = d, is_oa = isTRUE(up$is_oa),
             publisher_pdf = na.omit(pdfs[host == "publisher"])[1] %||% NA_character_,
             repo_pdfs = paste(na.omit(pdfs[host == "repository"]), collapse = " "),
             landing = up$best_oa_location$url %||% NA_character_)
}), fill = TRUE)
oa_info[repo_pdfs == "", repo_pdfs := NA_character_]

save_pdf <- function(url, slug) {
  f <- file.path(dest, paste0(slug, ".pdf"))
  resp <- tryCatch(request(url) |> req_user_agent(ua) |> req_timeout(120) |>
                     req_error(is_error = function(r) FALSE) |> req_perform(path = f), error = function(e) NULL)
  ok <- !is.null(resp) && resp_status(resp) == 200 && file.exists(f) && file.size(f) > 10000 &&
    identical(readChar(f, 4L, useBytes = TRUE), "%PDF")
  if (!ok && file.exists(f)) file.remove(f)
  if (ok) basename(f) else NA_character_
}
got <- rbindlist(lapply(seq_len(nrow(oa_info)), function(i) {
  if (is.na(oa_info$repo_pdfs[i])) return(NULL)
  slug <- gsub("[^a-z0-9]+", "_", oa_info$doi[i])
  for (u in strsplit(oa_info$repo_pdfs[i], " ")[[1]]) {
    f <- save_pdf(u, slug)
    if (!is.na(f)) return(data.table(doi = oa_info$doi[i], route = "unpaywall_repository", file = f))
  }
  NULL
}))
cat(sprintf("repository copies retrieved: %d of %d open-access studies with a repository link\n",
            nrow(got), sum(!is.na(oa_info$repo_pdfs))))
if (nrow(got)) {
  m[got, `:=`(route = i.route, file = i.file, status = "retrieved",
              sha256 = vapply(file.path(dest, i.file), digest, character(1), algo = "sha256", file = TRUE),
              date = as.character(Sys.Date())), on = "doi"]
  fwrite(m, manifest_file)
}

# WORKLIST ####################################################################

w <- merge(m[status != "retrieved"], sample_dt[, .(cited_id, title, year, first_author, journal, entry_example)], by = "cited_id")
w <- merge(w, oa_info[, .(doi, is_oa, publisher_pdf, landing)], by = "doi", all.x = TRUE)
w[, group := fcase(is.na(doi) | !nzchar(doi), "3 no DOI: library search",
                   is_oa %in% TRUE & !is.na(publisher_pdf), "2 open access, publisher refused the script: download in a browser",
                   is_oa %in% TRUE, "2b open access, landing page only: download in a browser",
                   default = "1 closed, no API route: download through the university library proxy")]
setorder(w, group, block, cited_no)
w[, save_as := fifelse(!is.na(doi) & nzchar(doi), paste0(gsub("[^a-z0-9]+", "_", tolower(doi)), ".pdf"),
                       sprintf("cited_%04d.pdf", cited_no))]
worklist <- w[, .(group, block, cited_no, save_as, cited_id, doi, title, year, first_author, journal,
                  url = fcoalesce(publisher_pdf, landing, fifelse(!is.na(doi) & nzchar(doi), paste0("https://doi.org/", doi), NA_character_)),
                  entry_example)]
fwrite(worklist, file.path(dir_in, paste0(worklist_stem, ".csv")))
write.xlsx(list(worklist = worklist, retrieved = m[status == "retrieved"]),
           file.path(dir_in, paste0(worklist_stem, ".xlsx")), overwrite = TRUE)

cat("\n=== Stage-2 retrieval after the repository pass ===\n")
print(m[, .N, by = .(status, route)][order(-N)])
print(worklist[, .N, by = group][order(group)])
print(m[, .(retrieved = sum(status == "retrieved"), missing = sum(status != "retrieved")), by = block][order(block)])
