flatten_coding_record_fun <- function(txt, choice) {

  if (is.null(txt) || is.na(txt)) return(NULL)
  js <- tryCatch(jsonlite::parse_json(enc2utf8(txt)), error = function(e) NULL)
  if (is.null(js)) return(NULL)

  collapse <- function(v) if (is.null(v) || !length(v)) NA_character_ else
    paste(sort(as.character(unlist(v))), collapse = ";")
  scal <- function(v) if (is.null(v) || !length(v)) NA else v[[1]]

  if (choice == "analytical") {
    data.table::data.table(
      id_cite = scal(js$id_cite), epistemic_type = scal(js$epistemic_type),
      functions = collapse(js$functions),
      efficiency_legible = scal(js$efficiency_legible),
      sensory_channels = collapse(js$sensory_channels),
      n_env_variables = scal(js$n_env_variables),
      eroded_by = collapse(js$eroded_by),
      corroboration = scal(js$corroboration),
      measured_variable = collapse(js$measured_variable),
      time_horizon = scal(js$time_horizon),
      inferential_form = scal(js$inferential_form),
      temporal_framing = scal(js$temporal_framing),
      knowledge_mode = scal(js$knowledge_mode),
      is_saying = scal(js$is_saying),
      is_value_statement = scal(js$is_value_statement),
      cue = scal(js$cue), inference = scal(js$inference),
      action = scal(js$action), practice = scal(js$practice),
      confidence = scal(js$confidence), rationale = scal(js$rationale))
  } else {
    data.table::data.table(
      id_cite = scal(js$id_cite),
      primary_subcategory = scal(js$primary_subcategory),
      secondary_subcategory = scal(js$secondary_subcategory),
      cross_subcategory = scal(js$cross_category$subcategory),
      cross_main_category = scal(js$cross_category$main_category),
      confidence = scal(js$confidence), rationale = scal(js$rationale),
      main_category_flag = scal(js$main_category_flag))
  }
}
