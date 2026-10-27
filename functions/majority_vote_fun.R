majority_vote_fun <- function(dt_long, id_col = "id_cite", fields) {

  dt_long <- data.table::as.data.table(dt_long)
  fields <- intersect(fields, names(dt_long))

  vote <- function(x) {
    x <- x[!is.na(x)]
    if (!length(x)) return(list(val = NA_character_, agree = NA_real_))
    tab <- sort(table(x), decreasing = TRUE)
    top <- tab[tab == max(tab)]
    val <- sort(names(top))[1]
    list(val = val, agree = unname(max(tab)) / length(x))
  }

  out <- dt_long[, {
    res <- lapply(.SD, function(col) vote(as.character(col)))
    vals <- vapply(res, `[[`, character(1), "val")
    agr <- vapply(res, `[[`, numeric(1), "agree")
    c(as.list(vals),
      list(n_samples = .N,
           mean_agreement = mean(agr, na.rm = TRUE),
           min_agreement = min(agr, na.rm = TRUE)))
  }, by = id_col, .SDcols = fields]

  out
}
