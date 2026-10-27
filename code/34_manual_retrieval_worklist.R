# MANUAL RETRIEVAL WORKLIST ###################################################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "writexl"))

# BUILD THE WORKLIST ##########################################################

out_dir <- here("datasets", "output")
manifest <- fread(file.path(out_dir, "literature_fulltext_manifest.csv"),
                  encoding = "UTF-8")
man <- manifest[status == "manual"]

man[, save_as := paste0(gsub("[^a-z0-9]+", "_", tolower(trimws(doi))), ".pdf")]
man[, doi_link := paste0("https://doi.org/", doi)]

setorder(man, technology, year, title)
worklist <- man[, .(n = .I, rec_id, technology, year, title, doi, doi_link,
                    save_as, retrieved = "", note = "")]

fwrite(worklist, file.path(out_dir, "literature_fulltext_worklist.csv"))

# xlsx with a leading instructions sheet --------------------------------------

instructions <- data.table(step = 1:6, instruction = c(
  "Open each DOI link in the doi_link column and download the publisher PDF (use the UoB VPN / institutional access for paywalled ones).",
  "Save every PDF into: datasets/inputs/literature_corpus/fulltexts/",
  "Name each file EXACTLY as the save_as column (DOI slug + .pdf). The pipeline matches on this name; any other name is ignored.",
  "If a PDF is genuinely inaccessible, leave its file out and write 'no access' in the note column so it is not chased again.",
  "Put an x in the retrieved column as you go, so the list doubles as a progress tracker.",
  "When done, rerun: Rscript code/14_fulltext_retrieval.R (reconciles the manifest), then code/15 re-extracts the enlarged corpus."))

write_xlsx(list(instructions = instructions, worklist = worklist),
           file.path(out_dir, "literature_fulltext_worklist.xlsx"))

cat(sprintf("Worklist: %d records to retrieve by hand.\n", nrow(worklist)))
print(worklist[, .N, by = technology][order(-N)])
cat(sprintf("\nSave PDFs to: %s\n",
            here("datasets", "inputs", "literature_corpus", "fulltexts")))
cat("Filename column: save_as (DOI slug + .pdf)\n")
