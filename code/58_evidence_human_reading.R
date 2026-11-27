# STAGE-2 HUMAN READING OF THE CITED STUDIES ##################################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "openxlsx"))

# Settings --------------------------------------------------------------------

in_dir <- here("datasets", "inputs", "stage2_human_reading")
out_dir <- here("datasets", "output", "stage2_evidence")
dir.create(out_dir, showWarnings = FALSE)
q_cols <- c("primary_study", "empirical", "triangulation", "causality", "representativeness",
            "scalability", "support_factors", "limitations", "uncertainty")
readers <- paste0("reader_", 1:5)
assignment <- fread(here("datasets", "output", "link_extraction", "fable_full",
                         "stage2_reader_assignment.csv"))

# Read the workbooks ----------------------------------------------------------

files <- file.path(in_dir, paste0("stage2_", readers, ".xlsx"))
present <- file.exists(files)
if (any(!present)) cat("not returned yet:", paste(readers[!present], collapse = ", "), "\n")
hum <- rbindlist(lapply(which(present), function(k) {
  rbindlist(lapply(c("calibration", "main"), function(s) {
    d <- as.data.table(read.xlsx(files[k], sheet = s))
    stopifnot(all(c("cited_no", q_cols) %in% names(d)))
    d <- d[, c("cited_no", q_cols), with = FALSE]
    d[, `:=`(reader = readers[k], tab = s, cited_no = as.integer(cited_no))]
    melt(d, id.vars = c("reader", "tab", "cited_no"), variable.name = "question",
         value.name = "raw", variable.factor = FALSE)
  }))
}))
hum[, answer := tolower(trimws(raw))]
hum[!answer %in% c("yes", "no"), answer := NA_character_]
hum[, q := paste0("q", match(question, q_cols))]
hum[, raw := NULL]

chk <- unique(hum[, .(reader, tab, cited_no)])[!assignment, on = .(reader, tab, cited_no)]
stopifnot(nrow(chk) == 0L)
fwrite(hum, file.path(out_dir, "stage2_human_answers.csv"))
cat(sprintf("%d readers, %d studies, %d answers (%d blank)\n", uniqueN(hum$reader),
            uniqueN(hum$cited_no), nrow(hum), hum[is.na(answer), .N]))
print(hum[, .(studies = uniqueN(cited_no), blank = sum(is.na(answer))), by = .(reader, tab)])

# One answer per study and question -------------------------------------------

study <- hum[!is.na(answer), .(yes = mean(answer == "yes") > 0.5, n_readers = .N),
             by = .(cited_no, q)]
study_w <- dcast(study, cited_no ~ q, value.var = "yes")
study_w <- merge(study_w, study[, .(n_readers = max(n_readers)), by = cited_no], by = "cited_no")
setcolorder(study_w, c("cited_no", paste0("q", 1:9), "n_readers"))
fwrite(study_w, file.path(out_dir, "stage2_human_study_answers.csv"))
cat(sprintf("study answers: %d studies (%d rated by more than one reader)\n",
            nrow(study_w), study_w[n_readers > 1, .N]))
