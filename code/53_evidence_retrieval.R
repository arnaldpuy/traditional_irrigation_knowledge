# STAGE-2 RETRIEVAL ###########################################################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "httr2", "digest"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"), pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

dir_in <- here("datasets", "output", "link_extraction", "fable_full")
dest <- Sys.getenv("STAGE2_PDF_DIR", "/Volumes/LaCieData/irrigators_assumptions/cited_papers")
stopifnot(dir.exists(dest))
manifest_file <- file.path(dir_in, Sys.getenv("STAGE2_MANIFEST_FILE", "stage2_retrieval_manifest.csv"))
sample_dt <- fread(file.path(dir_in, Sys.getenv("STAGE2_SAMPLE_FILE", "stage2_sample.csv")))
corpus <- fread(here("datasets", "output", "literature_fulltext_manifest.csv"))
corpus[, doi := tolower(doi)]
ft_dir <- here("datasets", "inputs", "literature_corpus", "fulltexts")

done <- if (file.exists(manifest_file)) fread(manifest_file) else
  data.table(cited_no = integer(), cited_id = character(), block = character(), doi = character(),
             route = character(), file = character(), status = character(), note = character(),
             sha256 = character(), date = character())
todo <- sample_dt[!cited_id %in% done[status == "retrieved", cited_id]]
cat(sprintf("sample %d; already retrieved %d; to process %d (%d with DOI)\n",
            nrow(sample_dt), nrow(done[status == "retrieved"]), nrow(todo), sum(todo$has_doi)))

rows <- vector("list", nrow(todo))
for (i in seq_len(nrow(todo))) {
  s <- todo[i]
  if (!s$has_doi) {
    rows[[i]] <- data.table(cited_no = s$cited_no, cited_id = s$cited_id, block = s$block, doi = NA_character_,
                            route = NA_character_, file = NA_character_, status = "manual", note = "no DOI")
    next
  }
  d <- tolower(s$doi)
  local <- corpus[doi == d & status == "retrieved" & nzchar(file)]
  if (nrow(local)) {
    f <- local$file[1]
    file.copy(file.path(ft_dir, f), file.path(dest, f), overwrite = TRUE)
    r <- data.table(doi = d, route = "review_corpus", file = f, status = "retrieved")
  } else {
    r <- fetch_fulltext_fun(d, dest_dir = dest)
  }
  rows[[i]] <- data.table(cited_no = s$cited_no, cited_id = s$cited_id, block = s$block, doi = d,
                          route = r$route, file = r$file, status = r$status, note = "")
  cat(sprintf("%3d/%d %-45s %s\n", i, nrow(todo), d, fifelse(r$status == "retrieved", r$route, "manual")))
  if (i %% 20 == 0) fwrite(rbind(done, rbindlist(rows[seq_len(i)]), fill = TRUE), manifest_file)
}
new <- rbindlist(rows, fill = TRUE)
new[status == "retrieved", sha256 := vapply(file.path(dest, file), digest, character(1), algo = "sha256", file = TRUE)]
new[, date := as.character(Sys.Date())]
out <- rbind(done[status == "retrieved"], new, fill = TRUE)
fwrite(out, manifest_file)

cat("\n=== Stage-2 retrieval ===\n")
print(out[, .N, by = .(status, route)][order(-N)])
print(out[, .(retrieved = sum(status == "retrieved"), manual = sum(status != "retrieved")), by = block][order(block)])
cat(sprintf("files on the drive: %d\n", length(list.files(dest))))
