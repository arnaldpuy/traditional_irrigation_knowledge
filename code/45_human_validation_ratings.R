# HUMAN VALIDATION OF THE REDUCIBLE / IRREDUCIBLE CLASSIFICATION ##############

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "openxlsx"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# LOAD DATA ###################################################################

in_dir <- here("datasets", "inputs", "human_validation")
out <- here("datasets", "output")
raters <- c("anthropologist", "modeller_1", "modeller_2")

ratings <- rbindlist(lapply(raters, function(k) {
  d <- as.data.table(read.xlsx(file.path(in_dir,
                                         sprintf("human_validation_rater_%s.xlsx", k))))
  stopifnot(nrow(d) == 475L, names(d)[1:2] == c("id_cite", "underlying_assumption"))
  data.table(rater = k, id_cite = d$id_cite, answer = tolower(trimws(d[[3]])))
}))
str(ratings)
ratings[1:3]

corpus <- load_coded_corpus_fun()
stopifnot(ratings[, setequal(id_cite, corpus$id_cite), by = rater]$V1,
          ratings[, !anyDuplicated(id_cite), by = rater]$V1,
          ratings[!is.na(answer), all(answer %in% c("yes", "no"))])

# RECODE ######################################################################

ratings[, irreducible := fifelse(answer == "no", 1L, fifelse(answer == "yes", 0L, NA_integer_))]
setnames(ratings, "answer", "reducible")
setkey(ratings, rater, id_cite)
print(ratings[, .(n_rated = sum(!is.na(irreducible)), n_blank = sum(is.na(irreducible))),
              by = rater])

# AGREEMENT ###################################################################

wide <- dcast(ratings, id_cite ~ rater, value.var = "irreducible")
pairs <- as.data.table(t(combn(raters, 2)))
setnames(pairs, c("rater_a", "rater_b"))
agreement <- pairs[, {
  a <- wide[[rater_a]]; b <- wide[[rater_b]]
  ok <- !is.na(a) & !is.na(b); a <- a[ok]; b <- b[ok]
  po <- mean(a == b)
  pe <- mean(a) * mean(b) + (1 - mean(a)) * (1 - mean(b))
  .(n = length(a), agreement = po, kappa = (po - pe) / (1 - pe))
}, by = .(rater_a, rater_b)]
print(agreement)

stopifnot(agreement$agreement > 0.6)

# SHARES ######################################################################

shares <- ratings[, .(n_rated = sum(!is.na(irreducible)),
                      irreducible_share = mean(irreducible, na.rm = TRUE)), by = rater]
print(shares)

fwrite(ratings, file.path(out, "human_validation_ratings.csv"))
fwrite(agreement, file.path(out, "human_validation_agreement.csv"))
fwrite(shares, file.path(out, "human_validation_shares.csv"))
