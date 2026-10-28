# LITERATURE EXTRACTION — DOCUMENTED-BENEFIT SPACE ###########################

# reused on rerun; GATED behind LITEXTRACT_RUN_ENABLED so no billable call fires

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "httr2", "jsonlite",
                          "xml2", "pdftools"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# CONFIGURATION ###############################################################

run_enabled <- tolower(Sys.getenv("LITEXTRACT_RUN_ENABLED", "false")) == "true"
out_dir <- here("datasets", "output")
ft_dir <- here("datasets", "inputs", "literature_corpus", "fulltexts")

manifest <- fread(file.path(out_dir, "literature_fulltext_manifest.csv"),
                  encoding = "UTF-8")
docs <- manifest[status == "retrieved" & !is.na(file) & file != ""]
docs[, path := file.path(ft_dir, file)]
docs[, rec_id := as.character(rec_id)]

# TEXT EXTRACTION #############################################################

offline <- !any(file.exists(docs$path))
if (offline) {
  cat("Full texts not on disk: reading literature_extraction_text.csv (metadata only)\n")
  docs <- fread(file.path(out_dir, "literature_extraction_text.csv"), encoding = "UTF-8")
  docs[, rec_id := as.character(rec_id)]
  docs[, `:=`(body = NA_character_, abstract = NA_character_)]
} else {
  cat(sprintf("Extracting text from %d retrieved full texts ...\n", nrow(docs)))
  text_dt <- rbindlist(lapply(docs$path, extract_doc_text_fun))
  docs <- cbind(docs[, .(rec_id, technology, title_meta = title, year, doi)],
                text_dt[, .(file, format, source, title_xml = title, abstract,
                            body, n_chars, truncated)])
  fwrite(docs[, !c("body", "abstract")],
         file.path(out_dir, "literature_extraction_text.csv"))
}
cat("Extraction source breakdown:\n"); print(docs[, .N, by = source])
cat(sprintf("Bodies < 500 chars (unusable): %d\n",
            docs[n_chars < 500, .N]))

# INSTRUMENT ##################################################################

fun_vocab <- c("crop_production", "water_conservation", "soil_health",
               "risk_management", "social_cohesion", "biodiversity",
               "microclimate_regulation", "pest_control", "groundwater_recharge",
               "livestock", "medicinal_food", "recreation_wellbeing")
tech_vocab <- c("drip", "soil_sensor", "remote_sensing", "on_demand",
                "app_scheduling", "flow_metering", "other")

extraction_prompt <- '
SYSTEM ROLE
You extract the documented-benefit space of irrigation-modernisation technologies
from review-level agricultural literature, using a fixed function vocabulary.

TASK
Given the text of a review or meta-analysis, report:
(a) which modernisation technologies its synthesised evidence evaluates, and
(b) for each of a FIXED set of agroecosystem functions the review presents
    evidence on, whether the technology DELIVERS (provides, improves) or DEGRADES
    (harms, reduces) that function, with spatial scale, evidence consistency, and
    evidence strength.

FUNCTION VOCABULARY (use these exact keys; never invent one)
- crop_production: crop yield, growth, quality, productivity
- water_conservation: reduced/saved water use, application efficiency, water productivity
- soil_health: soil structure, fertility, organic matter, salinity/sodicity, erosion control
- risk_management: buffering drought/frost/shortage, reliability, resilience, yield stability
- social_cohesion: collective governance, shared labour, equity of distribution, institutions, conflict
- biodiversity: habitat, species/agro-biodiversity, pollinators, aquatic and bird life
- microclimate_regulation: local temperature/humidity moderation, cooling, frost protection
- pest_control: suppression of pests, weeds, disease
- groundwater_recharge: aquifer replenishment, return flows, water-table maintenance
- livestock: forage, animal watering, crop-livestock integration
- medicinal_food: wild/gathered food and medicinal plants the system supports
- recreation_wellbeing: landscape, cultural, amenity, recreational, wellbeing value

RULES
- Code a function ONLY if the review presents evidence about it; omit the rest.
- direction: "delivered" | "degraded" | "mixed" (both directions reported).
- scale: "field" | "farm" | "basin" | "regional" | "mixed" | "unclear".
- consistency: "consistent" | "mixed" | "contested".
- evidence_strength: "strong" | "moderate" | "weak" (how the review characterises
  the body of evidence: meta-analytic/many studies = strong; few/qualitative = weak).
- justification: <= 25 words grounded in the text.
- off_vocabulary_benefits: benefits the review emphasises that fall OUTSIDE the 12
  functions (e.g. labour saving, energy use, profitability, fertiliser saving), or "none".
- technologies from: drip, soil_sensor, remote_sensing, on_demand, app_scheduling,
  flow_metering, other.
- evidence_tier: "meta_analysis" | "systematic_review" | "narrative_review" | "primary_study".
- dominant_scale: the spatial scale most of the evidence sits at.

OUTPUT
Return ONLY this JSON object:
{"technologies": ["<one or more tech keys>"],
 "evidence_tier": "<tier>",
 "dominant_scale": "<scale>",
 "function_findings": [
   {"function": "<vocab key>", "direction": "delivered|degraded|mixed",
    "scale": "<scale>", "consistency": "<consistency>",
    "evidence_strength": "strong|moderate|weak", "justification": "<=25 words>"}],
 "off_vocabulary_benefits": "<short text or none>",
 "notes": "<=20 words"}'

finding_schema <- list(
  type = "object",
  properties = list(
    `function` = list(type = "string", enum = as.list(fun_vocab)),
    direction = list(type = "string",
                     enum = list("delivered", "degraded", "mixed")),
    scale = list(type = "string",
                 enum = list("field", "farm", "basin", "regional",
                             "mixed", "unclear")),
    consistency = list(type = "string",
                       enum = list("consistent", "mixed", "contested")),
    evidence_strength = list(type = "string",
                             enum = list("strong", "moderate", "weak")),
    justification = list(type = "string")),
  required = list("function", "direction", "scale", "consistency",
                  "evidence_strength", "justification"),
  additionalProperties = FALSE)

extraction_schema <- list(
  type = "object",
  properties = list(
    technologies = list(type = "array",
                        items = list(type = "string", enum = as.list(tech_vocab))),
    evidence_tier = list(type = "string",
                         enum = list("meta_analysis", "systematic_review",
                                     "narrative_review", "primary_study")),
    dominant_scale = list(type = "string",
                          enum = list("field", "farm", "basin", "regional",
                                      "mixed", "unclear")),
    function_findings = list(type = "array", items = finding_schema),
    off_vocabulary_benefits = list(type = "string"),
    notes = list(type = "string")),
  required = list("technologies", "evidence_tier", "dominant_scale",
                  "function_findings", "off_vocabulary_benefits", "notes"),
  additionalProperties = FALSE)

codeable <- docs[n_chars >= 500]
codeable[, user_text := sprintf(
  "RETRIEVAL-TAGGED TECHNOLOGY: %s\nTITLE: %s\nYEAR: %s\n\nTEXT:\n%s",
  technology, fifelse(is.na(title_xml) | title_xml == "", title_meta, title_xml),
  year, body)]
cat(sprintf("Codeable documents (>=500 chars): %d / %d\n",
            nrow(codeable), nrow(docs)))

# DUAL-MODEL, N-DRAW EXTRACTION ###############################################

n_draws <- as.integer(Sys.getenv("LITEXTRACT_N_DRAWS", "5"))

if (!run_enabled) cat("\nGATED: LITEXTRACT_RUN_ENABLED != true - no billable API contacted; parsing the archived replies.\n")
if (run_enabled || file.exists(file.path(out_dir, "literature_extraction_raw_claude.rds"))) {

  cl_raw_file <- file.path(out_dir, "literature_extraction_raw_claude.rds")
  ds_raw_file <- file.path(out_dir, "literature_extraction_raw_deepseek.rds")
  items <- codeable[, .(custom_id = rec_id, user_text)]

  # One firing sweep over n_draws, resumable per draw -------------------------

  fire_draws <- function(cache_file, fire_one, label) {
    cache <- if (file.exists(cache_file)) as.data.table(readRDS(cache_file)) else
      data.table(custom_id = character(), draw = integer(),
                 status = character(), text = character())
    if (!"draw" %in% names(cache)) cache[, draw := 1L]   # legacy single-draw cache
    if (!run_enabled) return(cache)
    for (dr in seq_len(n_draws)) {
      done <- cache[draw == dr & status == "succeeded" & !is.na(text), custom_id]
      todo <- setdiff(items$custom_id, done)
      if (length(todo) == 0L) {
        cat(sprintf("%s draw %d/%d: all %d cached.\n", label, dr, n_draws, nrow(items)))
        next
      }
      cat(sprintf("%s draw %d/%d: firing %d (%d cached).\n",
                  label, dr, n_draws, length(todo), length(done)))
      res <- fire_one(items[custom_id %in% todo])
      res[, draw := dr]
      cache <- rbind(cache[!(draw == dr & custom_id %in% todo)], res, fill = TRUE)
      saveRDS(cache, cache_file)
    }
    cache
  }

  fire_claude <- function(it) run_claude_direct_fun(
    items = it, system_prompt = extraction_prompt, schema = extraction_schema,
    max_tokens = 1500L, effort = "medium", concurrency = 6L, timeout = 240L)
  fire_deepseek <- function(it) run_deepseek_batch_fun(
    items = it, system_prompt = extraction_prompt, temperature = 0.4,
    max_tokens = 1500L)

  cl_raw <- fire_draws(cl_raw_file, fire_claude, "Claude")
  ds_raw <- fire_draws(ds_raw_file, fire_deepseek, "DeepSeek")

  # PARSE EVERY DRAW ############################################################

  parse_doc <- function(txt, rec_id, model, draw) {
    v <- tryCatch(jsonlite::parse_json(txt), error = function(e) NULL)
    if (is.null(v) || length(v$function_findings) == 0) return(NULL)
    rbindlist(lapply(v$function_findings, function(ff) data.table(
      rec_id = rec_id, model = model, draw = draw,
      `function` = ff[["function"]] %||% NA_character_,
      direction = ff$direction %||% NA_character_,
      scale = ff$scale %||% NA_character_,
      consistency = ff$consistency %||% NA_character_,
      evidence_strength = ff$evidence_strength %||% NA_character_,
      justification = ff$justification %||% NA_character_)), fill = TRUE)
  }
  parse_docmeta <- function(txt, rec_id, model, draw) {
    v <- tryCatch(jsonlite::parse_json(txt), error = function(e) NULL)
    if (is.null(v)) return(NULL)
    data.table(rec_id = rec_id, model = model, draw = draw,
      technologies = paste(unlist(v$technologies), collapse = ";"),
      evidence_tier = v$evidence_tier %||% NA_character_,
      dominant_scale = v$dominant_scale %||% NA_character_,
      off_vocabulary_benefits = as.character(v$off_vocabulary_benefits %||% NA),
      n_findings = length(v$function_findings))
  }
  parse_cache <- function(raw, model, f) {
    raw <- raw[status == "succeeded" & !is.na(text)]
    rbindlist(Map(f, raw$text, raw$custom_id, model, raw$draw), fill = TRUE)
  }

  draws_long <- rbind(parse_cache(cl_raw, "claude", parse_doc),
                      parse_cache(ds_raw, "deepseek", parse_doc), fill = TRUE)
  draws_long <- draws_long[!is.na(`function`)]
  draws_long <- merge(draws_long,
                      codeable[, .(rec_id, technology, title = title_meta, year)],
                      by = "rec_id", all.x = TRUE)
  fwrite(draws_long, file.path(out_dir, "literature_benefit_space_draws.csv"))

  # AGGREGATE ACROSS DRAWS ######################################################

  agg <- aggregate_extraction_fun(
    draws_long[, .(rec_id, model, draw, `function`, direction, scale,
                   consistency, evidence_strength)])
  cons <- merge(agg$consensus,
                codeable[, .(rec_id, technology, year)], by = "rec_id", all.x = TRUE)
  fwrite(cons, file.path(out_dir, "literature_benefit_consensus.csv"))
  fwrite(agg$cell, file.path(out_dir, "literature_extraction_cell_stability.csv"))
  fwrite(agg$reliability, file.path(out_dir, "literature_extraction_reliability.csv"))

  # Doc summary: majority over all draws, per (rec_id, model) ------------------

  meta_draws <- rbind(parse_cache(cl_raw, "claude", parse_docmeta),
                      parse_cache(ds_raw, "deepseek", parse_docmeta), fill = TRUE)
  vote1 <- function(x) { x <- x[!is.na(x) & x != ""]
    if (!length(x)) NA_character_ else names(sort(table(x), decreasing = TRUE))[1] }
  docmeta <- meta_draws[, .(
    evidence_tier = vote1(evidence_tier), dominant_scale = vote1(dominant_scale),
    technologies = vote1(technologies),
    off_vocabulary_benefits = off_vocabulary_benefits[which.max(nchar(off_vocabulary_benefits))],
    mean_findings = mean(n_findings, na.rm = TRUE)),
    by = .(rec_id, model)]
  docmeta <- merge(docmeta, codeable[, .(rec_id, technology, year, source, truncated)],
                   by = "rec_id", all.x = TRUE)
  fwrite(docmeta, file.path(out_dir, "literature_extraction_docsummary.csv"))

  # QUICK READOUT #############################################################

  cat(sprintf("\nDraws: %d per model; parsed findings %d over %d docs.\n",
              n_draws, nrow(draws_long), uniqueN(draws_long$rec_id)))
  cat("\nReliability (Krippendorff alpha):\n"); print(agg$reliability)
  del <- cons[delivered_consensus == TRUE]
  cat(sprintf("\nConsensus DELIVERED cells (present + agreed, both models): %d\n", nrow(del)))
  cat("Delivered functions (consensus):\n")
  print(del[, .N, by = `function`][order(-N)])
  core <- c("crop_production", "water_conservation", "soil_health", "risk_management")
  cat(sprintf("\nConsensus delivered in efficiency CORE: %.0f%% ; social-ecological TAIL: %.0f%%\n",
              100 * del[`function` %in% core, .N] / nrow(del),
              100 * del[!`function` %in% core, .N] / nrow(del)))
  cat(sprintf("Envelope (delivered cells): pessimistic %d / consensus %d / optimistic %d\n",
              cons[delivered_pessimistic == TRUE, .N], nrow(del),
              cons[delivered_optimistic == TRUE, .N]))
}
