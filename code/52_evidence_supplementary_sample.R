# SUPPLEMENTARY STAGE-2 SAMPLE FOR THE UNCOVERED CELLS ########################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "openxlsx"))

dir_in <- here("datasets", "output", "link_extraction", "fable_full")
uncovered <- fread(file.path(dir_in, "stage2_uncovered_studies.csv"))
cell <- fread(file.path(dir_in, "cell_by_study.csv"))
studies <- fread(file.path(dir_in, "cited_studies.csv"))
seed <- 20261119L
allocation <- c(drip_crop_only = 25L, drip_water_only = 25L, app_water_only = 25L,
                two_or_three_cells = 15L)
out_file <- file.path(dir_in, "stage2_supplementary_sample.csv")
if (file.exists(out_file)) stop("stage2_supplementary_sample.csv exists; the draw is fixed and is not redrawn.")

# Stratified draw --------------------------------------------------------------

set.seed(seed)
strata <- uncovered[, .(n_stratum = .N), by = pattern]
stopifnot(all(names(allocation) %in% strata$pattern))
drawn <- rbindlist(lapply(names(allocation), function(h) {
  pool <- uncovered[pattern == h]
  pool[sample(.N, min(allocation[[h]], .N))]
}))
drawn <- merge(drawn, strata, by = "pattern")
drawn[, n_drawn := .N, by = pattern]
drawn[, pi_supplement := n_drawn / n_stratum]
setnames(drawn, "pattern", "stratum")
cat(sprintf("seed %d; drawn %d of %d uncovered studies\n", seed, nrow(drawn), nrow(uncovered)))
print(drawn[, .(n_stratum = n_stratum[1], n_drawn = .N, pi = round(pi_supplement[1], 3)), by = stratum])
cat("studies per uncovered cell in the supplement:\n")
print(drawn[, .(drip_crop = sum(in_drip_crop), drip_water = sum(in_drip_water), app_water = sum(in_app_water))])

# Output in the shape of stage2_sample.csv -------------------------------------

tech_of <- cell[cited_id %in% drawn$cited_id,
                .(technologies = paste(sort(unique(technology)), collapse = "; "),
                  functions = paste(sort(unique(`function`)), collapse = "; "),
                  n_cells = uniqueN(paste(technology, `function`)), n_links = sum(n_links),
                  n_reviews = max(n_reviews)), by = cited_id]
out <- merge(drawn[, .(cited_id, cited_no, stratum, n_stratum, n_drawn, pi_supplement)], tech_of, by = "cited_id")
out <- merge(out, studies[, .(cited_id, doi, title, year, first_author, journal, resolution = route, entry_example)],
             by = "cited_id")
out[, `:=`(block = "supplement", route = "supplement (uncovered thick cell)",
           has_doi = !is.na(doi) & nzchar(doi), seed = seed)]
setcolorder(out, c("cited_no", "cited_id", "block", "route", "stratum", "n_stratum", "n_drawn", "pi_supplement",
                   "technologies", "functions", "n_cells", "n_links", "n_reviews", "doi", "has_doi", "title",
                   "year", "first_author", "journal", "resolution", "entry_example", "seed"))
setorder(out, stratum, cited_no)
fwrite(out, out_file)
write.xlsx(out, sub("csv$", "xlsx", out_file), overwrite = TRUE)
cat(sprintf("%d studies written; %d with a DOI\n", nrow(out), sum(out$has_doi)))
