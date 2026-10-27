tabulate_single_fun <- function(dt, col) {

  out <- dt[, .(n = .N), by = c(col)][order(-n)]
  setnames(out, col, "level")
  out[, percent := round(100 * n / sum(n), 1)]
  out[]
}
