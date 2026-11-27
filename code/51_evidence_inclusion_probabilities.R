# INCLUSION PROBABILITIES OF THE STAGE-2 SAMPLE ###############################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table"))

dir_in <- here("datasets", "output", "link_extraction", "fable_full")
cell <- fread(file.path(dir_in, "cell_by_study.csv"))
sample_dt <- fread(file.path(dir_in, "stage2_sample.csv"))
n_replays <- 4000L
replay_seed <- 20261119L
per_cell <- 30L
thick_functions <- c("water_conservation", "crop_production", "risk_management", "soil_health")
cell[, thick := `function` %in% thick_functions]
cell[, cell_id := paste(technology, `function`, sep = " x ")]

# Replay of code/68 ------------------------------------------------------------

thick_cells <- cell[thick == TRUE, .(n_studies = uniqueN(cited_id)), by = cell_id][order(n_studies)]
pools <- setNames(lapply(thick_cells$cell_id, function(cid) unique(cell[cell_id == cid, cited_id])),
                  thick_cells$cell_id)
census <- unique(cell[thick == FALSE, cited_id])
frame <- unique(cell[, .(cited_id, cited_no)])
draw_once_fun <- function() {
  selected <- census
  for (pool in pools) {
    already <- intersect(pool, selected)
    need <- max(0L, min(per_cell, length(pool)) - length(already))
    if (need > 0) selected <- c(selected, sample(setdiff(pool, already), need))
  }
  selected
}
set.seed(replay_seed)
counts <- setNames(integer(nrow(frame)), frame$cited_id)
for (r in seq_len(n_replays)) {
  s <- draw_once_fun()
  counts[s] <- counts[s] + 1L
}
pi <- data.table(cited_id = names(counts), n_selected = as.integer(counts))
pi[, pi_study := n_selected / n_replays]
pi <- merge(frame, pi, by = "cited_id")
pi[, census := cited_id %in% census]
pi[, in_sample := cited_id %in% sample_dt$cited_id]
cells_of <- cell[, .(cells = paste(sort(unique(cell_id)), collapse = "; "), n_cells = uniqueN(cell_id)),
                 by = cited_id]
pi <- merge(pi, cells_of, by = "cited_id")
# Pool over exchangeable membership patterns -----------------------------------
pi[, `:=`(pattern_size = .N, pi_original = sum(n_selected) / (.N * n_replays)), by = cells]
pi[, pi_mc_se := sqrt(pi_original * (1 - pi_original) / (pattern_size * n_replays))]
near_zero <- 1e-3
supp_frame_file <- file.path(dir_in, "stage2_uncovered_studies.csv")
pi[, in_supplement_frame := if (file.exists(supp_frame_file)) cited_id %in% fread(supp_frame_file)$cited_id else pi_study == 0]
pi[, n_replays := n_replays][, replay_seed := replay_seed]
setorder(pi, cited_no)
fwrite(pi, file.path(dir_in, "stage2_inclusion_probabilities.csv"))
cat(sprintf("frame %d studies in %d membership patterns; census %d; expected sample size %.1f (actual %d)\n",
            nrow(pi), uniqueN(pi$cells), sum(pi$census), sum(pi$pi_original), nrow(sample_dt)))
cat(sprintf("studies never selected in %d replays: %d; studies in patterns with pooled probability below %.0e: %d\n",
            n_replays, pi[pi_study == 0, .N], near_zero, pi[pi_original < near_zero, .N]))
cat("near-zero patterns:\n")
print(pi[pi_original < near_zero, .(studies = .N, in_supplement_frame = sum(in_supplement_frame),
                                     pi_pooled = signif(pi_original[1], 2)), by = cells])
stopifnot(all(pi[in_sample == TRUE, pi_original > 0]))

# Uncovered studies and the cells they sit in --------------------------------

if (file.exists(supp_frame_file)) {
  cat("stage2_uncovered_studies.csv exists (frame of the fixed supplementary draw); not rewritten.\n")
  quit(save = "no")
}
uncovered <- merge(pi[pi_study == 0, .(cited_id, cited_no, cells, n_cells)],
                   cell[, .(cited_id, cell_id)], by = "cited_id")
unc_cells <- uncovered[, .(n_uncovered = uniqueN(cited_id)), by = cell_id][order(-n_uncovered)]
print(unc_cells)
stopifnot(nrow(unc_cells) == 3L)
unc_wide <- uncovered[, .(cells = cells[1], n_cells = n_cells[1],
                          in_drip_crop = any(cell_id == "drip x crop_production"),
                          in_drip_water = any(cell_id == "drip x water_conservation"),
                          in_app_water = any(cell_id == "app_scheduling x water_conservation")),
                      by = .(cited_id, cited_no)]
unc_wide[, pattern := fcase(in_drip_crop & !in_drip_water & !in_app_water, "drip_crop_only",
                            !in_drip_crop & in_drip_water & !in_app_water, "drip_water_only",
                            !in_drip_crop & !in_drip_water & in_app_water, "app_water_only",
                            default = "two_or_three_cells")]
setorder(unc_wide, cited_no)
fwrite(unc_wide, file.path(dir_in, "stage2_uncovered_studies.csv"))
cat("uncovered studies by membership pattern:\n"); print(unc_wide[, .N, by = pattern][order(-N)])
