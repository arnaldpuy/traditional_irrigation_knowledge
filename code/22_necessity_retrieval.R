# NECESSITY-PREMISE FULL-TEXT RETRIEVAL #######################################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "httr2", "readxl", "digest"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# CONFIGURATION ###############################################################

run_enabled <- tolower(Sys.getenv("NECFETCH_RUN_ENABLED", "false")) == "true"
out_dir <- here("datasets", "output")
ft_dir <- here("datasets", "inputs", "necessity_corpus", "fulltexts")
if (!dir.exists(ft_dir)) dir.create(ft_dir, recursive = TRUE)

# BUILD THE INCLUDE LIST ######################################################

votes <- fread(file.path(out_dir, "necessity_screening_votes.csv"),
               encoding = "UTF-8")
include <- votes[agreement == "agree" & cl_include == TRUE]

adj_xlsx <- file.path(out_dir, "necessity_screening_adjudication.xlsx")
adj_file <- file.path(out_dir, "necessity_screening_adjudication.csv")
if (file.exists(adj_xlsx) || file.exists(adj_file)) {
  adj <- if (file.exists(adj_xlsx)) {
    as.data.table(read_excel(adj_xlsx))
  } else {
    fread(adj_file, encoding = "UTF-8")
  }
  adj_in <- adj[toupper(as.character(human_include)) == "TRUE"]
  if (nrow(adj_in) > 0) {
    include <- rbind(include, votes[rec_id %in% adj_in$rec_id], fill = TRUE)
    cat(sprintf("Adjudicated includes added: %d\n", nrow(adj_in)))
  }
  n_pending <- adj[is.na(human_include) | human_include == "", .N]
  if (n_pending > 0) cat(sprintf(
    "NOTE: %d adjudication rows still pending - rerun after they are filled.\n",
    n_pending))
}

include <- unique(include, by = "rec_id")
include <- include[!is.na(doi) & doi != ""]
cat(sprintf("Include list: %d records with a DOI\n", nrow(include)))

# RETRIEVE ####################################################################

if (!run_enabled) {
  cat("\nGATED: NECFETCH_RUN_ENABLED != true - no retrieval attempted.\n")
} else {

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

  # MANIFEST + MANUAL RESIDUE #################################################

  res[, doi_norm := tolower(trimws(doi))]
  include[, doi_norm := tolower(trimws(doi))]
  manifest <- merge(include[, .(rec_id, doi_norm, stratum, title, year)],
                    res, by = "doi_norm", all.x = TRUE)
  manifest[status == "retrieved",
           sha256 := vapply(file.path(ft_dir, file), function(f)
             if (file.exists(f)) digest::digest(f, algo = "sha256", file = TRUE)
             else NA_character_, character(1))]
  fwrite(manifest, file.path(out_dir, "necessity_fulltext_manifest.csv"))

  manual <- manifest[status == "manual", .(rec_id, doi, title, year, stratum)]
  manual[, save_as := paste0(gsub("[^a-z0-9]+", "_", tolower(trimws(doi))), ".pdf")]
  fwrite(manual, file.path(out_dir, "necessity_manual_retrieval.csv"))

  cat(sprintf("\nRetrieved %d / %d; %d for manual retrieval -> necessity_manual_retrieval.csv\n",
              manifest[status == "retrieved", .N], nrow(manifest),
              nrow(manual)))
  cat(sprintf("Save manual PDFs to %s using the save_as names.\n", ft_dir))
}
