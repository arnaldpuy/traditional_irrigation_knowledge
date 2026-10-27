tabulate_multi_fun <- function(dt, col, sep = ";") {

  toks <- trimws(unlist(strsplit(dt[[col]], sep, fixed = TRUE)))
  toks <- toks[nzchar(toks)]
  out <- data.table(level = toks)[, .(n = .N), by = level][order(-n)]
  out[, percent_of_rows := round(100 * n / nrow(dt), 1)]
  out[]
}
