# MD5 of a file; for an .xlsx workbook, of the contents of its sheets, so that a

file_md5_fun <- function(path) {
  if (!grepl("\\.xlsx$", path, ignore.case = TRUE))
    return(digest::digest(path, algo = "md5", file = TRUE))
  sheets <- openxlsx::getSheetNames(path)
  contents <- lapply(sheets, function(s)
    tryCatch(openxlsx::read.xlsx(path, sheet = s), error = function(e) NULL))
  digest::digest(list(sheets, contents), algo = "md5")
}
