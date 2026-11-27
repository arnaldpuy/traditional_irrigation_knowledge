# STAGE-2 CALIBRATION ROUND: AGREEMENT PER QUESTION ###########################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "openxlsx"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"), pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

dir_in <- here("datasets", "output", "link_extraction", "fable_full")
sheets_dir <- Sys.getenv("STAGE2_READER_OUT", path.expand("~/Desktop/stage2_reader_files"))
returned_dir <- Sys.getenv("STAGE2_RETURNED_DIR", here("datasets", "inputs", "stage2_human_reading"))
design <- fread(file.path(dir_in, "stage2_reader_design.csv"))
kappa_threshold <- as.numeric(design[item == "kappa_threshold", value])
n_readers <- as.integer(design[item == "n_readers", value])
reader_ids <- as.integer(strsplit(Sys.getenv("STAGE2_READERS", "1,2,3,5"), ",")[[1]])
stopifnot(all(reader_ids %in% seq_len(n_readers)), length(reader_ids) >= 2L)
readers <- paste0("reader_", reader_ids)
assignment <- fread(file.path(dir_in, "stage2_reader_assignment.csv"))
q_cols <- c("primary_study", "empirical", "triangulation", "causality", "representativeness",
            "scalability", "support_factors", "limitations", "uncertainty")

# READ THE RETURNED CALIBRATION TABS ##########################################

answers <- rbindlist(lapply(reader_ids, function(k) {
  reader <- paste0("reader_", k)
  f <- file.path(returned_dir, paste0("stage2_", reader, ".xlsx"))
  if (!file.exists(f)) f <- file.path(sheets_dir, reader, paste0("stage2_", reader, ".xlsx"))
  d <- as.data.table(read.xlsx(f, sheet = "calibration"))
  d[, reader := reader]
  d[, cited_no := as.integer(cited_no)]
  melt(d[, c("reader", "cited_no", q_cols, "notes"), with = FALSE],
       id.vars = c("reader", "cited_no", "notes"), variable.name = "question",
       value.name = "answer", variable.factor = FALSE)
}))
answers[, answer := tolower(trimws(answer))]
answers[answer %in% c("y", "si", "sí"), answer := "yes"]
answers[answer %in% c("n"), answer := "no"]
answers[!answer %in% c("yes", "no"), answer := NA_character_]
expected <- assignment[tab == "calibration" & reader %in% readers, .(reader, cited_no)]
stopifnot(nrow(merge(expected, unique(answers[, .(reader, cited_no)]), by = c("reader", "cited_no"))) == nrow(expected))
missing <- answers[is.na(answer), .N, by = .(reader, question)]
if (nrow(missing)) {
  cat("blank or invalid answers (dropped paper-wise per question):\n")
  print(missing)
}

# KAPPA PER QUESTION ##########################################################

wide <- dcast(answers, question + cited_no ~ reader, value.var = "answer")
kappa <- wide[, {
  x <- .SD[, readers, with = FALSE]
  loo <- vapply(readers, function(r) fleiss_kappa_fun(x[, setdiff(readers, r), with = FALSE]), numeric(1))
  .(n_papers = sum(complete.cases(x)), kappa = fleiss_kappa_fun(x),
    kappa_loo_min = min(loo), kappa_loo_max = max(loo), kappa_loo_min_without = readers[which.min(loo)],
    yes_share = mean(unlist(x) == "yes", na.rm = TRUE))
}, by = question]
kappa[, verdict := fifelse(kappa >= kappa_threshold, "pass", "rewrite")]
kappa[, threshold := kappa_threshold]
kappa[, readers := paste(reader_ids, collapse = ",")]
setorder(kappa, kappa)

# READERS AGAINST THE MAJORITY ###############################################

answers[, n_yes := sum(answer == "yes", na.rm = TRUE), by = .(question, cited_no)]
answers[, n_valid := sum(!is.na(answer)), by = .(question, cited_no)]
answers[, majority := fcase(n_yes > n_valid / 2, "yes", n_yes < n_valid / 2, "no", default = NA_character_)]
reader_agreement <- answers[!is.na(answer) & !is.na(majority),
                            .(agreement_with_majority = mean(answer == majority), n = .N),
                            by = .(reader, question)]
reader_overall <- answers[!is.na(answer) & !is.na(majority),
                          .(agreement_with_majority = mean(answer == majority)), by = reader]

# SPLIT PAPERS ###############################################################

splits <- answers[n_valid >= 2 & n_yes > 0 & n_yes < n_valid,
                  .(n_yes = n_yes[1], n_valid = n_valid[1],
                    yes_readers = paste(reader[answer == "yes"], collapse = ", "),
                    notes = paste(unique(notes[!is.na(notes) & nzchar(notes)]), collapse = " | ")),
                  by = .(question, cited_no)]
splits <- merge(splits, unique(assignment[tab == "calibration", .(cited_no, file)]), by = "cited_no")
setorder(splits, question, -n_yes)

# WRITE #######################################################################

fwrite(answers[, .(reader, cited_no, question, answer, majority, notes)],
       file.path(dir_in, "stage2_calibration_answers.csv"))
fwrite(kappa, file.path(dir_in, "stage2_calibration_kappa.csv"))
fwrite(reader_agreement, file.path(dir_in, "stage2_calibration_reader_agreement.csv"))
fwrite(splits, file.path(dir_in, "stage2_calibration_splits.csv"))
write.xlsx(list(kappa = kappa, reader_agreement = dcast(reader_agreement, question ~ reader, value.var = "agreement_with_majority"),
                splits = splits, answers = answers[, .(reader, cited_no, question, answer, majority, notes)]),
           file.path(dir_in, "stage2_calibration_report.xlsx"), overwrite = TRUE)

cat(sprintf("\n=== Calibration round: %d papers, readers %s (%d of %d), threshold kappa >= %.2f ===\n",
            uniqueN(answers$cited_no), paste(reader_ids, collapse = ","), length(reader_ids), n_readers, kappa_threshold))
print(kappa[, .(question, n_papers, kappa = round(kappa, 2), loo_range = sprintf("%.2f-%.2f", kappa_loo_min, kappa_loo_max),
                yes_share = round(yes_share, 2), verdict)])
cat("\nreaders' agreement with the majority:\n")
print(reader_overall[order(-agreement_with_majority)][, agreement_with_majority := round(agreement_with_majority, 2)][])
cat(sprintf("\nsplit papers: %d question x paper pairs (stage2_calibration_splits.csv)\n", nrow(splits)))
