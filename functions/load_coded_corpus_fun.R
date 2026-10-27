# Load the canonical coded corpus exactly as the analysis notebook does:

load_coded_corpus_fun <- function() {
  out <- here::here("datasets", "output")
  claude <- data.table::fread(
    file.path(out, "coding_majority_analytical_anthropic.csv"))
  meta <- data.table::as.data.table(readxl::read_xlsx(
    here::here("datasets", "inputs", "assumption_list_updated.xlsx")))
  data.table::setnames(meta, tolower(names(meta)))
  meta <- meta[, .(id_cite, site, main_category,
                   assumption = underlying_assumption)]

  dt <- merge(claude, meta, by = "id_cite")
  dt[, efficiency_legible := as.character(efficiency_legible)]
  dt[, practice := normalize_practice_fun(practice)]
  dt[, domain := main_category]
  dt[]
}
