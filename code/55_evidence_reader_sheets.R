# STAGE-2 READER SHEETS #######################################################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "httr2", "digest", "openxlsx", "stringi"))

dir_in <- here("datasets", "output", "link_extraction", "fable_full")
pdf_dir <- Sys.getenv("STAGE2_PDF_DIR", "/Volumes/LaCieData/irrigators_assumptions/cited_papers")
out_dir <- Sys.getenv("STAGE2_READER_OUT", path.expand("~/Desktop/stage2_reader_files"))
seed <- 20261028L
kappa_threshold <- 0.60
n_readers <- 5L
manifest_file <- file.path(dir_in, "stage2_retrieval_manifest.csv")
m <- fread(manifest_file, colClasses = list(character = c("date", "sha256", "note", "file", "route")))
sample_dt <- fread(file.path(dir_in, "stage2_sample.csv"))
links <- fread(file.path(dir_in, "link_rows_cited.csv"))
le <- fread(file.path(dir_in, "link_extraction.csv"))
le[, row_id := .I]

# PUBLISHER PDFS FOR THE XML COPIES ###########################################

els_key <- Sys.getenv("SCOPUS_API_KEY")
xml_only <- m[status == "retrieved" & grepl("\\.xml$", file) & nzchar(doi)]
got <- rbindlist(lapply(xml_only$doi, function(d) {
  f <- file.path(pdf_dir, paste0(gsub("[^a-z0-9]+", "_", tolower(d)), ".pdf"))
  if (!file.exists(f)) {
    resp <- tryCatch(request(paste0("https://api.elsevier.com/content/article/doi/", d)) |>
                       req_url_query(httpAccept = "application/pdf") |>
                       req_headers(`X-ELS-APIKey` = els_key, Accept = "application/pdf") |>
                       req_timeout(120) |> req_error(is_error = function(r) FALSE) |>
                       req_perform(path = f), error = function(e) NULL)
    ok <- !is.null(resp) && resp_status(resp) == 200 && file.exists(f) && file.size(f) > 10000 &&
      identical(readChar(f, 4L, useBytes = TRUE), "%PDF")
    if (!ok) {
      if (file.exists(f)) file.remove(f)
      return(NULL)
    }
  }
  data.table(doi = d, file = basename(f))
}))
cat(sprintf("publisher PDFs for XML copies: %d of %d\n", nrow(got), nrow(xml_only)))
if (nrow(got)) {
  m[got, `:=`(file = i.file, note = "publisher PDF via Elsevier API; XML kept",
              sha256 = vapply(file.path(pdf_dir, i.file), digest, character(1), algo = "sha256", file = TRUE),
              date = as.character(Sys.Date())), on = "doi"]
  fwrite(m, manifest_file)
}

# CITING CONTEXT PER STUDY ####################################################

ctx <- le[links[cited_id %in% sample_dt$cited_id, .(cited_id, row_id)], on = "row_id"]
ctx[, cell := paste0(technology, " -> ", `function`, " (", direction, ")")]
pass <- ctx[, .(cells = paste(sort(unique(cell)), collapse = "; ")), by = .(cited_id, rec_id, year, passage)]
study_ctx <- pass[, .(cells = paste(sort(unique(unlist(strsplit(cells, "; ", fixed = TRUE)))), collapse = "; "),
                      citing_passages = paste(sprintf("[%s, %s | %s] %s", rec_id, year, cells, passage),
                                              collapse = "\n\n")), by = cited_id]

# SHEET ROWS ##################################################################

base <- merge(sample_dt[, .(cited_no, cited_id, block, doi, title, year, first_author, journal, entry_example)],
              m[, .(cited_id, file, status)], by = "cited_id")
base[is.na(title) | !nzchar(title), title := entry_example]
base <- merge(base, study_ctx, by = "cited_id", all.x = TRUE)
set.seed(seed)
base[, order_key := runif(.N)]
setorder(base, order_key)
q_cols <- c("primary_study", "empirical", "triangulation", "causality", "representativeness",
            "scalability", "support_factors", "limitations", "uncertainty")
base[, (q_cols) := ""]
base[, notes := ""]
sheet_cols <- c("file", "cited_no", "doi", "title", "year", "first_author", "journal", "cells",
                "citing_passages", q_cols, "notes")

name_copies <- function(d, tag) {
  author <- gsub("[^A-Za-z]", "", stringi::stri_trans_general(d$first_author, "Latin-ASCII"))
  author[is.na(author) | !nzchar(author)] <- sprintf("cited%04d", d$cited_no[is.na(author) | !nzchar(author)])
  year <- fifelse(is.na(d$year), "nd", as.character(d$year))
  sprintf("%s_%02d_%s_%s.%s", tag, seq_len(nrow(d)), author, year, tools::file_ext(d$file))
}

questions <- data.table(
  column = q_cols,
  group = c("Nature of the work", rep("Rigour", 3), rep("Applicability", 3), rep("Transparency", 2)),
  question = c(
    "Primary study: Is the cited work itself a primary study, reporting its own data, experiment or model runs, rather than a review, synthesis, guideline or book chapter?",
    "Nature of evidence: Does the study report empirical evidence (e.g. field, laboratory, observational)?",
    "Triangulation: Does the study report at least two independent types of evidence (empirical, computational, observational, model-based, survey)?",
    "Causality: Does the study report the identification of relevant causal mechanisms?",
    "Representativeness: Is the technology evaluated on at least three or more soil textural classes / crops / climate zones / regions?",
    "Scalability: Is the technology evaluated above field scale (farm, scheme, basin, regional)?",
    "Support factors: Does the study name at least one condition required for the replication of the effect (capacity, finance, governance, infrastructure; community cohesion)?",
    "Limitations: Does the study report limitations or restrictions in scope?",
    "Uncertainty: Does the study report uncertainty measures (e.g. range, interquartile, standard deviation, etc.)?"))

instructions <- data.table(item = c(
  "What you rate",
  "Order",
  "Files",
  "Answers",
  "Citing passages",
  "Independence",
  "Time"), text = c(
  "Each row is one study cited by one or more of the reviews on irrigation technologies. You rate the study itself, not the review's claim about it.",
  "Fill the 'calibration' tab first and send it back before starting 'main'. All readers rate the same calibration papers; the main tab is yours alone.",
  "The first column names the file to read, in the folder 'papers' next to this workbook. Files are numbered by tab and row: cal_03_... is row 3 of the calibration tab, main_12_... row 12 of the main tab. One file is a Word document; open it as such.",
  "Answer yes or no (dropdown) to all nine questions for every paper. Use 'notes' for doubts or for anything you want the analysis to know. Do not edit the other columns.",
  "The column 'citing_passages' shows how the reviews used the study and in which technology -> function cells. It orients the reading; the answers are about the study itself.",
  "Keep to this questionnaire throughout and do not discuss answers with the other readers until the calibration round has been scored.",
  "Reading enough of a paper to answer the nine questions takes about 15 to 20 minutes."))

# WRITE THE WORKBOOKS #########################################################

header_style <- createStyle(textDecoration = "bold", fgFill = "#E7E6E6", border = "bottom",
                            wrapText = TRUE, valign = "top")
body_style <- createStyle(wrapText = TRUE, valign = "top")
col_widths <- c(30, 9, 26, 44, 6, 14, 20, 34, 90, rep(13, length(q_cols)), 40)
q_idx <- which(sheet_cols %in% q_cols)
assignment <- list()
for (k in seq_len(n_readers)) {
  reader <- paste0("reader_", k)
  rows <- base[block %in% c("shared", reader)]
  calib <- rows[block == "shared" & status == "retrieved", ..sheet_cols]
  main <- rows[block == reader & status == "retrieved", ..sheet_cols]
  calib[, source_file := file][, file := name_copies(calib, "cal")]
  main[, source_file := file][, file := name_copies(main, "main")]
  not_retrieved <- rows[status != "retrieved", .(block, cited_no, doi, title, year, first_author, journal)]
  reader_dir <- file.path(out_dir, reader)
  papers_dir <- file.path(reader_dir, "papers")
  unlink(papers_dir, recursive = TRUE)
  dir.create(papers_dir, showWarnings = FALSE, recursive = TRUE)
  copies <- rbind(calib, main)[, .(source_file, file)]
  stopifnot(all(file.copy(file.path(pdf_dir, copies$source_file), file.path(papers_dir, copies$file))))
  wb <- createWorkbook()
  addWorksheet(wb, "instructions")
  writeData(wb, "instructions", instructions, headerStyle = header_style)
  writeData(wb, "instructions", questions, startRow = nrow(instructions) + 3L, headerStyle = header_style)
  addStyle(wb, "instructions", body_style, rows = 1:(nrow(instructions) + nrow(questions) + 4L),
           cols = 1:3, gridExpand = TRUE)
  setColWidths(wb, "instructions", cols = 1:3, widths = c(22, 24, 110))
  for (tab in c("calibration", "main")) {
    d <- if (tab == "calibration") calib[, ..sheet_cols] else main[, ..sheet_cols]
    addWorksheet(wb, tab)
    writeData(wb, tab, d, headerStyle = header_style)
    addStyle(wb, tab, body_style, rows = 2:(nrow(d) + 1L), cols = seq_along(sheet_cols), gridExpand = TRUE)
    setColWidths(wb, tab, cols = seq_along(sheet_cols), widths = col_widths)
    freezePane(wb, tab, firstRow = TRUE, firstCol = TRUE)
    dataValidation(wb, tab, cols = q_idx, rows = 2:(nrow(d) + 1L), type = "list", value = '"yes,no"')
  }
  addWorksheet(wb, "not_retrieved")
  writeData(wb, "not_retrieved", not_retrieved, headerStyle = header_style)
  setColWidths(wb, "not_retrieved", cols = 1:7, widths = c(10, 9, 26, 50, 6, 14, 20))
  saveWorkbook(wb, file.path(reader_dir, paste0("stage2_", reader, ".xlsx")), overwrite = TRUE)
  assignment[[k]] <- rbind(calib[, .(reader, tab = "calibration", position = .I, cited_no, file, source_file)],
                           main[, .(reader, tab = "main", position = .I, cited_no, file, source_file)])
}
assignment <- rbindlist(assignment)

n_pages <- function(f) {
  out <- suppressWarnings(system2("pdfinfo", shQuote(f), stdout = TRUE, stderr = FALSE))
  as.integer(sub("^Pages:\\s+", "", grep("^Pages:", out, value = TRUE)))[1]
}
short <- unique(assignment[grepl("\\.pdf$", source_file), .(source_file)])
short[, pages := vapply(file.path(pdf_dir, source_file), n_pages, integer(1))]
short <- short[is.na(pages) | pages <= 2]
if (nrow(short)) {
  cat(sprintf("\nWARNING: %d PDFs have at most two pages; replace them before distribution:\n", nrow(short)))
  print(short)
}
assignment <- merge(assignment, sample_dt[, .(cited_no, cited_id, block)], by = "cited_no")
setorder(assignment, reader, tab, position)
fwrite(assignment, file.path(dir_in, "stage2_reader_assignment.csv"))
fwrite(data.table(item = c("seed", "kappa_threshold", "n_readers", "date", "sheets_dir"),
                  value = c(seed, kappa_threshold, n_readers, as.character(Sys.Date()),
                            sub(path.expand("~"), "~", out_dir, fixed = TRUE))),
       file.path(dir_in, "stage2_reader_design.csv"))

cat("\n=== Papers per reader ===\n")
print(dcast(assignment[, .N, by = .(reader, tab)], reader ~ tab, value.var = "N")[, total := calibration + main][])
cat(sprintf("not retrieved: shared %d, unique %d\n",
            base[block == "shared" & status != "retrieved", .N],
            base[block != "shared" & status != "retrieved", .N]))
