# Blanks the author fields (creator, last modified by) in the file properties of an

strip_xlsx_authors_fun <- function(path) {
  path <- normalizePath(path)
  tmp <- tempfile("xlsx_core_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  utils::unzip(path, files = "docProps/core.xml", exdir = tmp)
  core <- file.path(tmp, "docProps", "core.xml")
  if (!file.exists(core)) return(invisible(FALSE))
  x <- readLines(core, warn = FALSE, encoding = "UTF-8")
  x <- gsub("<dc:creator>[^<]*</dc:creator>", "<dc:creator></dc:creator>", x)
  x <- gsub("<cp:lastModifiedBy>[^<]*</cp:lastModifiedBy>",
            "<cp:lastModifiedBy></cp:lastModifiedBy>", x)
  writeLines(x, core, useBytes = TRUE)
  old <- setwd(tmp)
  on.exit(setwd(old), add = TRUE, after = FALSE)
  status <- system2("zip", c("-q", "-X", shQuote(path), "docProps/core.xml"))
  if (status != 0) stop("zip failed on ", path)
  invisible(TRUE)
}
