# Krippendorff's alpha via the coincidence-matrix method, with a pluggable

kripp_alpha_fun <- function(units, metric = c("nominal", "jaccard")) {
  metric <- match.arg(metric)

  norm_rating <- function(x) {
    x <- x[!is.na(x) & trimws(x) != ""]
    if (metric == "jaccard") {
      x <- vapply(x, function(s) {
        toks <- sort(unique(trimws(strsplit(s, ";")[[1]])))
        toks <- toks[toks != ""]
        paste(toks, collapse = ";")
      }, character(1))
    }
    unname(x)
  }

  units <- lapply(units, norm_rating)
  units <- units[vapply(units, length, integer(1)) >= 2L]
  if (length(units) == 0L) return(list(alpha = NA_real_, n_units = 0L, n_ratings = 0L))

  # Value inventory + pairwise difference matrix -------------------------------

  vals <- sort(unique(unlist(units)))
  diff_of <- function(a, b) {
    if (a == b) return(0)
    if (metric == "nominal") return(1)
    sa <- strsplit(a, ";")[[1]]; sb <- strsplit(b, ";")[[1]]
    inter <- length(intersect(sa, sb)); uni <- length(union(sa, sb))
    if (uni == 0) 0 else 1 - inter / uni
  }
  d2 <- outer(vals, vals, Vectorize(function(a, b) diff_of(a, b)^2))
  dimnames(d2) <- list(vals, vals)

  # Coincidence matrix ---------------------------------------------------------

  o <- matrix(0, length(vals), length(vals), dimnames = list(vals, vals))
  for (u in units) {
    m <- length(u)
    tab <- table(factor(u, levels = vals))
    for (a in vals) for (b in vals) {
      if (a == b) {
        o[a, b] <- o[a, b] + tab[a] * (tab[a] - 1) / (m - 1)
      } else {
        o[a, b] <- o[a, b] + tab[a] * tab[b] / (m - 1)
      }
    }
  }

  n_c <- rowSums(o)
  n <- sum(n_c)
  if (n < 2) return(list(alpha = NA_real_, n_units = length(units), n_ratings = n))

  do_num <- sum(o * d2)
  de_num <- sum(outer(n_c, n_c) * d2) / (n - 1)
  alpha <- 1 - do_num / de_num

  list(alpha = alpha, n_units = length(units), n_ratings = as.integer(n))
}
