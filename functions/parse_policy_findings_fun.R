# Parse one promise-map model reply (the JSON the code/18 instrument asks for)

parse_policy_findings_fun <- function(txt, rec_id, model, draw) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
  v <- tryCatch(jsonlite::parse_json(txt), error = function(e) NULL)
  if (is.null(v) || length(v$function_findings) == 0) return(NULL)
  data.table::rbindlist(lapply(v$function_findings, function(ff) data.table::data.table(
    rec_id = rec_id, model = model, draw = draw,
    `function` = ff[["function"]] %||% NA_character_,
    direction = ff$direction %||% NA_character_,
    scale = ff$scale %||% NA_character_,
    consistency = ff$consistency %||% NA_character_,
    evidence_strength = ff$evidence_strength %||% NA_character_,
    justification = ff$justification %||% NA_character_)), fill = TRUE)
}
