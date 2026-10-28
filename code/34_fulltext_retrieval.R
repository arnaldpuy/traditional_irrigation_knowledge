# FULL-TEXT RETRIEVAL #########################################################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "httr2", "readxl", "digest"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# BUILD THE INCLUDE LIST ######################################################

out_dir <- here("datasets", "output")
ft_dir <- here("datasets", "inputs", "literature_corpus", "fulltexts")
if (!dir.exists(ft_dir)) dir.create(ft_dir, recursive = TRUE)

votes <- fread(file.path(out_dir, "literature_screening_votes.csv"),
               encoding = "UTF-8")
include <- votes[agreement == "agree" & cl_include == TRUE]

adj_xlsx <- file.path(out_dir, "literature_screening_adjudication.xlsx")
adj_file <- file.path(out_dir, "literature_screening_adjudication.csv")
if (file.exists(adj_xlsx) || file.exists(adj_file)) {
  adj <- if (file.exists(adj_xlsx)) {
    as.data.table(read_excel(adj_xlsx))
  } else {
    fread(adj_file, encoding = "UTF-8")
  }
  adj_in <- adj[toupper(as.character(human_include)) == "TRUE"]
  if (nrow(adj_in) > 0) {
    include <- rbind(include,
                     votes[rec_id %in% adj_in$rec_id], fill = TRUE)
    cat(sprintf("Adjudicated includes added: %d\n", nrow(adj_in)))
  }
  n_pending <- adj[is.na(human_include) | human_include == "", .N]
  if (n_pending > 0) cat(sprintf(
    "NOTE: %d adjudication rows still pending - rerun after they are filled.\n",
    n_pending))
}

spot_xlsx <- file.path(out_dir, "literature_screening_spotcheck.xlsx")
if (file.exists(spot_xlsx)) {
  spot <- as.data.table(read_excel(spot_xlsx))
  flips <- spot[human_agrees == FALSE]
  if (nrow(flips) > 0) {
    drop_ids <- flips[models_decision == "include", rec_id]
    add_ids <- flips[models_decision == "exclude", rec_id]
    include <- include[!rec_id %in% drop_ids]
    if (length(add_ids) > 0)
      include <- rbind(include, votes[rec_id %in% add_ids], fill = TRUE)
    cat(sprintf("Spot-check overrides: %d dropped, %d added\n",
                length(drop_ids), length(add_ids)))
  }
}

include <- unique(include, by = "rec_id")
include <- include[!is.na(doi) & doi != ""]
cat(sprintf("Include list: %d records with a DOI\n", nrow(include)))

# RETRIEVE ####################################################################

have <- tools::file_path_sans_ext(list.files(ft_dir))
res <- rbindlist(lapply(include$doi, function(d) {
  slug <- gsub("[^a-z0-9]+", "_", tolower(trimws(d)))
  if (slug %in% have) {
    f <- list.files(ft_dir, pattern = paste0("^", slug, "\\."))[1]
    return(data.table(doi = tolower(trimws(d)), route = "on_disk",
                      file = f, status = "retrieved"))
  }
  r <- fetch_fulltext_fun(d, dest_dir = ft_dir)
  cat(sprintf("  %-45s -> %s\n", substr(d, 1, 45),
              fifelse(r$status == "retrieved", r$route, "manual")))
  r
}))

# MANIFEST + MANUAL RESIDUE ###################################################

res[, doi_norm := tolower(trimws(doi))]
include[, doi_norm := tolower(trimws(doi))]
manifest <- merge(include[, .(rec_id, doi_norm, technology, title, year)],
                  res, by = "doi_norm", all.x = TRUE)
manifest[status == "retrieved",
         sha256 := vapply(file.path(ft_dir, file), function(f)
           if (file.exists(f)) digest::digest(f, algo = "sha256", file = TRUE)
           else NA_character_, character(1))]

lm_file <- file.path(out_dir, "landmark_retrieval_worklist.csv")
if (file.exists(lm_file)) {
  lm <- fread(lm_file, encoding = "UTF-8")
  lm[, doi_norm := tolower(trimws(doi))]
  have <- list.files(ft_dir)
  lm[, file := fifelse(save_as %in% have, save_as, NA_character_)]
  lm[, status := fifelse(!is.na(file), "retrieved", "manual")]
  lm[, route := fifelse(status == "retrieved", "landmark", NA_character_)]
  lm[status == "retrieved",
     sha256 := vapply(file.path(ft_dir, file), function(f)
       digest::digest(f, algo = "sha256", file = TRUE), character(1))]
  manifest <- rbind(manifest,
                    lm[, .(doi_norm, rec_id, technology, title, year, doi,
                           route, file, status, sha256)], fill = TRUE)
  cat(sprintf("Landmark stratum merged: %d (%d on disk)\n",
              nrow(lm), lm[status == "retrieved", .N]))
}
fwrite(manifest, file.path(out_dir, "literature_fulltext_manifest.csv"))
fwrite(manifest[status == "manual", .(rec_id, doi, title, year, technology)],
       file.path(out_dir, "literature_fulltext_manual.csv"))

cat(sprintf("\nRetrieved %d / %d (%s); %d for manual portal retrieval -> literature_fulltext_manual.csv\n",
            manifest[status == "retrieved", .N], nrow(manifest),
            paste(manifest[status == "retrieved", .N, by = route][
              , sprintf("%s %d", route, N)], collapse = ", "),
            manifest[status == "manual", .N]))
