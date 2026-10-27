# NECESSITY-PREMISE ILLUSTRATIVE SUBSET — SELECTION AND RETRIEVAL #############

# GATED: set NECFETCH_RUN_ENABLED=true to run the retrieval waterfall.

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "httr2", "digest"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# CONFIGURATION ###############################################################

run_enabled <- tolower(Sys.getenv("NECFETCH_RUN_ENABLED", "false")) == "true"
out_dir <- here("datasets", "output")
ft_dir <- here("datasets", "inputs", "necessity_corpus", "fulltexts")
if (!dir.exists(ft_dir)) dir.create(ft_dir, recursive = TRUE)
n_subset <- 200L  # takes the whole explicit-explicit pool (129 unique DOIs)

# SELECT: top-cited explicit-explicit concordant includes ---------------------

votes <- fread(file.path(out_dir, "necessity_screening_votes.csv"),
               encoding = "UTF-8")
queue <- fread(file.path(out_dir, "necessity_screening_queue.csv"),
               encoding = "UTF-8")
sel <- votes[agreement == "agree" & cl_include == TRUE &
             cl_signal == "explicit" & ds_signal == "explicit" &
             !is.na(doi) & doi != ""]

# Citation count: Scopus rows carry cited_by; Dimensions rows times_cited -----

cite_cols <- intersect(c("cited_by", "times_cited"), names(queue))
cites <- queue[, c("doi_norm", cite_cols), with = FALSE]
cites[, citations := do.call(pmax, c(lapply(.SD, function(x)
  suppressWarnings(as.numeric(x))), na.rm = TRUE)), .SDcols = cite_cols]
cites <- unique(cites[, .(doi_norm, citations)], by = "doi_norm")
sel[, doi_norm := tolower(trimws(doi))]
sel <- merge(sel, cites, by = "doi_norm", all.x = TRUE)
sel <- unique(sel, by = "doi_norm")
setorder(sel, -citations, na.last = TRUE)
subset <- sel[1:min(n_subset, .N)]
cat(sprintf("Explicit-explicit pool: %d unique DOIs; subset taken: %d (citations %s-%s)\n",
            nrow(sel), nrow(subset),
            subset[.N, fifelse(is.na(citations), "NA", as.character(citations))],
            subset[1, fifelse(is.na(citations), "NA", as.character(citations))]))

# RETRIEVE ####################################################################

if (!run_enabled) {
  cat("\nGATED: NECFETCH_RUN_ENABLED != true - no retrieval attempted.\n")
} else {

  ua <- paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
               "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0 Safari/537.36")
  fetch_mdpi <- function(d, slug) {
    landing <- tryCatch(
      httr2::request(paste0("https://doi.org/", d)) |>
        httr2::req_user_agent(ua) |> httr2::req_timeout(60) |>
        httr2::req_perform() |> httr2::resp_url(),
      error = function(e) NULL)
    if (is.null(landing) || !grepl("mdpi\\.com", landing)) return(NULL)
    dest <- file.path(ft_dir, paste0(slug, ".pdf"))
    ok <- tryCatch({
      httr2::request(paste0(sub("/+$", "", landing), "/pdf")) |>
        httr2::req_user_agent(ua) |> httr2::req_timeout(120) |>
        httr2::req_retry(max_tries = 3) |> httr2::req_perform(path = dest)
      file.exists(dest) && file.size(dest) > 10000 &&
        identical(readChar(dest, 4L, useBytes = TRUE), "%PDF")
    }, error = function(e) FALSE)
    if (!ok && file.exists(dest)) file.remove(dest)
    if (ok) dest else NULL
  }

  have <- tools::file_path_sans_ext(list.files(ft_dir))
  res <- rbindlist(lapply(subset$doi, function(d) {
    slug <- gsub("[^a-z0-9]+", "_", tolower(trimws(d)))
    if (slug %in% have) {
      f <- list.files(ft_dir, pattern = paste0("^", slug, "\\."))[1]
      return(data.table(doi = tolower(trimws(d)), route = "on_disk",
                        file = f, status = "retrieved"))
    }
    r <- fetch_fulltext_fun(d, dest_dir = ft_dir)
    if (r$status != "retrieved" && grepl("^10\\.3390/", tolower(trimws(d)))) {
      f <- fetch_mdpi(tolower(trimws(d)), slug)
      if (!is.null(f)) r <- data.table(doi = tolower(trimws(d)), route = "mdpi",
                                       file = basename(f), status = "retrieved")
    }
    cat(sprintf("  %-45s -> %s\n", substr(d, 1, 45),
                fifelse(r$status == "retrieved", r$route, "manual")))
    r
  }))

  res[, doi_norm := tolower(trimws(doi))]
  manifest <- merge(subset[, .(rec_id, doi_norm, stratum, title, year, citations)],
                    res, by = "doi_norm", all.x = TRUE)
  manifest[status == "retrieved",
           sha256 := vapply(file.path(ft_dir, file), function(f)
             if (file.exists(f)) digest::digest(f, algo = "sha256", file = TRUE)
             else NA_character_, character(1))]
  fwrite(manifest, file.path(out_dir, "necessity_subset_manifest.csv"))

  cat(sprintf("\nRetrieved %d / %d; %d unavailable (acceptable: the subset is
illustrative and over-provisioned relative to the 30-40 quote target)\n",
              manifest[status == "retrieved", .N], nrow(manifest),
              manifest[status != "retrieved", .N]))
}
