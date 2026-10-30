# Fleiss' kappa for a papers x raters matrix of categorical answers (NA rows dropped).
fleiss_kappa_fun <- function(x) {
  x <- as.matrix(x)
  x <- x[stats::complete.cases(x), , drop = FALSE]
  n_raters <- ncol(x)
  if (nrow(x) < 2L || n_raters < 2L) return(NA_real_)
  cats <- sort(unique(as.vector(x)))
  counts <- vapply(cats, function(k) rowSums(x == k), numeric(nrow(x)))
  counts <- matrix(counts, nrow = nrow(x))
  p_i <- (rowSums(counts^2) - n_raters) / (n_raters * (n_raters - 1))
  p_j <- colSums(counts) / (nrow(x) * n_raters)
  p_bar <- mean(p_i)
  p_e <- sum(p_j^2)
  if (p_e == 1) return(1)
  (p_bar - p_e) / (1 - p_e)
}
