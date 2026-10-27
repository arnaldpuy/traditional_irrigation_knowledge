# Aggregate the N-draw dual-model literature extraction into a stabilised

aggregate_extraction_fun <- function(draws, present_thresh = 0.6,
                                     kripp_fun = kripp_alpha_fun) {
  draws <- data.table::as.data.table(draws)
  data.table::setnames(draws, "function", "fn", skip_absent = TRUE)

  draws <- unique(draws, by = c("rec_id", "model", "draw", "fn"))

  maj <- function(x) {
    x <- x[!is.na(x) & x != ""]
    if (!length(x)) return(NA_character_)
    tb <- sort(table(x), decreasing = TRUE)
    if (length(tb) > 1L && tb[1] == tb[2]) return(NA_character_)   # tie unresolved
    names(tb)[1]
  }

  # Draws per model (presence denominator) ------------------------------------

  nd <- draws[, .(n_draws = data.table::uniqueN(draw)), by = model]

  # Per (rec_id, model, function): presence fraction + majority attributes ----

  pres <- draws[, .(k = data.table::uniqueN(draw)), by = .(rec_id, model, fn)]
  pres <- merge(pres, nd, by = "model")
  pres[, presence_frac := k / n_draws]

  attrs <- draws[, .(direction = maj(direction), scale = maj(scale),
                     consistency = maj(consistency),
                     evidence_strength = maj(evidence_strength),
                     dir_agree = max(table(direction[!is.na(direction)])) /
                       sum(!is.na(direction))),
                 by = .(rec_id, model, fn)]

  cell <- merge(pres, attrs, by = c("rec_id", "model", "fn"))
  cell[is.na(direction), direction := "mixed"]                    # direction tie
  cell[, present := presence_frac >= present_thresh]

  # Cross-model consensus over present cells ----------------------------------

  keep <- cell[present == TRUE]
  dir_w <- data.table::dcast(keep, rec_id + fn ~ model, value.var = "direction")
  for (m in c("claude", "deepseek"))
    if (!m %in% names(dir_w)) dir_w[, (m) := NA_character_]
  dir_w[, present_both := !is.na(claude) & !is.na(deepseek)]
  dir_w[, present_either := !is.na(claude) | !is.na(deepseek)]
  dir_w[, direction_consensus := data.table::fifelse(
    present_both & claude == deepseek, claude,
    data.table::fifelse(present_both, "contested", NA_character_))]

  # Envelope: a function counts as DELIVERED under each variant ----------------

  is_del <- function(x) x %in% c("delivered", "mixed")
  dir_w[, delivered_pessimistic := present_both & claude == deepseek & is_del(claude)]
  dir_w[, delivered_consensus := present_both &
          (is_del(claude) & is_del(deepseek))]
  dir_w[, delivered_optimistic := (is_del(claude) %in% TRUE) |
          (is_del(deepseek) %in% TRUE)]

  # Krippendorff's alpha over the union grid, "absent" as a real category -----

  grid <- unique(draws[, .(rec_id, fn)])
  raters <- unique(draws[, .(model, draw)])
  # Cartesian product grid x raters (repeat grid rows, tile raters) -----------
  g <- nrow(grid); r <- nrow(raters)
  full <- cbind(grid[rep(seq_len(g), each = r)],
                raters[rep(seq_len(r), times = g)])
  full <- merge(full,
                draws[, .(rec_id, fn, model, draw, direction)],
                by = c("rec_id", "fn", "model", "draw"), all.x = TRUE)
  full[is.na(direction), direction := "absent"]

  units_incl <- split(full$direction, paste(full$rec_id, full$fn, sep = "||"))
  a_incl <- kripp_fun(units_incl, metric = "nominal")

  cond <- full[direction != "absent"]
  units_cond <- split(cond$direction, paste(cond$rec_id, cond$fn, sep = "||"))
  a_cond <- kripp_fun(units_cond, metric = "nominal")

  reliability <- data.table::data.table(
    measure = c("alpha_presence_inclusive", "alpha_direction_conditional"),
    alpha = c(a_incl$alpha, a_cond$alpha),
    n_units = c(a_incl$n_units, a_cond$n_units),
    n_ratings = c(a_incl$n_ratings, a_cond$n_ratings))

  data.table::setnames(cell, "fn", "function", skip_absent = TRUE)
  data.table::setnames(dir_w, "fn", "function", skip_absent = TRUE)
  list(cell = cell[], consensus = dir_w[], reliability = reliability[])
}
