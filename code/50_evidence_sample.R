# STAGE-2 SAMPLE ###############################################################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "openxlsx"))

dir_in <- here("datasets", "output", "link_extraction", "fable_full")
cell <- fread(file.path(dir_in, "cell_by_study.csv"))
studies <- fread(file.path(dir_in, "cited_studies.csv"))
seed <- 20261028L
per_cell <- 30L
n_shared <- 40L
n_readers <- 5L
thick_functions <- c("water_conservation", "crop_production", "risk_management", "soil_health")
cell[, thick := `function` %in% thick_functions]
cell[, cell_id := paste(technology, `function`, sep = " x ")]

# CENSUS AND THICK-CELL DRAW ##################################################

set.seed(seed)
selected <- unique(cell[thick == FALSE, cited_id])
cat(sprintf("census of the thin cells: %d studies\n", length(selected)))
thick_cells <- cell[thick == TRUE, .(n_studies = uniqueN(cited_id)), by = cell_id][order(n_studies)]
for (cid in thick_cells$cell_id) {
  pool <- unique(cell[cell_id == cid, cited_id])
  already <- intersect(pool, selected)
  need <- max(0L, min(per_cell, length(pool)) - length(already))
  draw <- if (need > 0) sample(setdiff(pool, already), need) else character()
  selected <- c(selected, draw)
}
sample_dt <- unique(cell[cited_id %in% selected, .(cited_id, cited_no)])
sample_dt[, route := fifelse(cited_id %in% cell[thick == FALSE, cited_id], "census (thin function)", "sample (thick cell)")]
cat(sprintf("thick-cell draw adds %d; sample = %d studies\n",
            sample_dt[route == "sample (thick cell)", .N], nrow(sample_dt)))

# Cell summary -------------------------------------------------------------------

cell_summary <- cell[, .(n_studies = uniqueN(cited_id), n_sampled = uniqueN(cited_id[cited_id %in% selected])),
                     by = .(technology, `function`, thick)][order(`function`, technology)]
cell_summary[, rule := fifelse(!thick, "census", fifelse(n_studies <= per_cell, "all (cell below 30)", "30 at random"))]

# READER BLOCKS ################################################################

set.seed(seed + 1L)
tech_of <- cell[, .(technologies = paste(sort(unique(technology)), collapse = "; "),
                    functions = paste(sort(unique(`function`)), collapse = "; "),
                    n_cells = uniqueN(cell_id), n_links = sum(n_links), n_reviews = max(n_reviews)), by = cited_id]
sample_dt <- merge(sample_dt, tech_of, by = "cited_id")
sample_dt[, first_tech := sub(";.*$", "", technologies)]
pick_spread <- function(dt, n) {       # round-robin over technologies in random order
  dt <- dt[sample(.N)]
  setorder(dt[, r := seq_len(.N), by = first_tech], r)
  dt[seq_len(min(n, .N)), cited_id]
}
shared <- c(pick_spread(sample_dt[route == "census (thin function)"], n_shared / 2),
            pick_spread(sample_dt[route == "sample (thick cell)"], n_shared / 2))
sample_dt[, block := fifelse(cited_id %in% shared, "shared", NA_character_)]

set.seed(seed + 2L)
rest <- sample_dt[is.na(block)][sample(.N)]
rest[, reader := ((seq_len(.N) - 1L) %% n_readers) + 1L, by = .(first_tech, route)]
sample_dt[rest, block := paste0("reader_", i.reader), on = "cited_id"]
if ("r" %in% names(sample_dt)) sample_dt[, r := NULL]

# OUTPUT ######################################################################

out <- merge(sample_dt, studies[, .(cited_id, doi, title, year, first_author, journal, resolution = route, entry_example)],
             by = "cited_id")
out[, has_doi := !is.na(doi) & nzchar(doi)]
setcolorder(out, c("cited_no", "cited_id", "block", "route", "technologies", "functions", "n_cells", "n_links",
                   "n_reviews", "doi", "has_doi", "title", "year", "first_author", "journal", "resolution", "entry_example"))
setorder(out, block, cited_no)
fwrite(out, file.path(dir_in, "stage2_sample.csv"))
fwrite(cell_summary, file.path(dir_in, "stage2_cell_summary.csv"))
write.xlsx(list(sample = out, cells = cell_summary,
                design = data.table(item = c("seed", "per thick cell", "shared block", "readers", "thin functions"),
                                    value = c(seed, per_cell, n_shared, n_readers,
                                              paste(setdiff(unique(cell$`function`), thick_functions), collapse = "; ")))),
           file.path(dir_in, "stage2_sample.xlsx"), overwrite = TRUE)

cat("\n=== Stage-2 sample ===\n")
print(out[, .(studies = .N, with_doi = sum(has_doi)), by = .(block)][order(block)])
print(out[, .N, by = route])
cat("\nper cell:\n"); print(cell_summary)
cat(sprintf("\nreader load: %d shared + %d-%d unique\n", n_shared,
            out[block != "shared", .N, by = block][, min(N)], out[block != "shared", .N, by = block][, max(N)]))
