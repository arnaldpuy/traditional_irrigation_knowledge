# POLICY PROMISE-MAP EXTRACTION ##############################################

# PROMINENCE of the claim (headline objective vs passing mention). GATED behind

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "httr2", "jsonlite",
                          "xml2", "pdftools"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# CONFIGURATION ###############################################################

run_enabled <- tolower(Sys.getenv("POLICY_RUN_ENABLED", "false")) == "true"
n_draws <- as.integer(Sys.getenv("POLICY_N_DRAWS", "5"))
out_dir <- here("datasets", "output")
pol_dir <- here("datasets", "inputs", "policy_corpus")

manifest <- fread(file.path(pol_dir, "policy_corpus_manifest.csv"),
                  encoding = "UTF-8")

# TEXT EXTRACTION #############################################################

pdfs <- list.files(pol_dir, pattern = "\\.pdf$", full.names = TRUE)
cat(sprintf("Policy PDFs on disk: %d\n", length(pdfs)))
text_dt <- rbindlist(lapply(pdfs, extract_doc_text_fun, max_chars = 130000L))
text_dt[, doc_id := tools::file_path_sans_ext(file)]
fwrite(text_dt[, !c("body", "abstract")],
       file.path(out_dir, "policy_extraction_text.csv"))
cat("Extraction source breakdown:\n"); print(text_dt[, .N, by = source])

codeable <- text_dt[n_chars >= 500]
cat(sprintf("Codeable policy documents: %d / %d\n", nrow(codeable), nrow(text_dt)))

# INSTRUMENT ##################################################################

fun_vocab <- c("crop_production", "water_conservation", "soil_health",
               "risk_management", "social_cohesion", "biodiversity",
               "microclimate_regulation", "pest_control", "groundwater_recharge",
               "livestock", "medicinal_food", "recreation_wellbeing")
tech_vocab <- c("drip", "soil_sensor", "remote_sensing", "on_demand",
                "app_scheduling", "flow_metering", "modernization_general",
                "other")

policy_prompt <- '
SYSTEM ROLE
You extract the PROMISE SPACE of irrigation-modernisation policy: the functions
that a policy or technical document attributes to modernisation and its
technologies, using a fixed function vocabulary.

TASK
Given the text of a policy plan or technical manual, report:
(a) which modernisation technologies it names or promotes, and
(b) for each of a FIXED set of agroecosystem functions the document invokes,
    whether it PROMISES the technology delivers that function (direction
    "delivered") or ADMITS a trade-off / harm to it (direction "degraded"),
    with the scale, the consistency of the claim, and its prominence.

FUNCTION VOCABULARY (use these exact keys; never invent one)
- crop_production: crop yield, growth, quality, agricultural production/output
- water_conservation: water saving, efficiency, reduced abstraction, water productivity
- soil_health: soil structure, fertility, salinity/erosion control
- risk_management: drought/shortage resilience, supply guarantee, reliability, stability
- social_cohesion: collective governance, equity of distribution, rural viability, cooperation
- biodiversity: habitat, ecosystems, species, environmental/nature protection
- microclimate_regulation: local climate, temperature/humidity, frost protection
- pest_control: pest, weed, disease suppression
- groundwater_recharge: aquifer recovery, return flows, water-table protection
- livestock: forage, animal husbandry integration
- medicinal_food: wild food and medicinal plants
- recreation_wellbeing: landscape, cultural, amenity, recreational, wellbeing value

RULES
- Code a function ONLY if the document invokes it in connection with modernisation.
- direction: "delivered" (promised benefit) | "degraded" (admitted trade-off) | "mixed".
- scale: "field" | "farm" | "basin" | "regional" | "mixed" | "unclear".
- consistency: "consistent" | "mixed" | "contested".
- evidence_strength = PROMINENCE: "strong" (headline objective/justification),
  "moderate" (stated benefit), "weak" (passing mention).
- justification: <= 25 words grounded in the text.
- off_vocabulary_benefits: promised benefits outside the 12 functions (energy,
  competitiveness, jobs, modernisation/digitalisation as an end, food security), or "none".
- technologies from: drip, soil_sensor, remote_sensing, on_demand, app_scheduling,
  flow_metering, modernization_general (use when the promise is about modernisation
  as a whole, not a named technology), other.
- evidence_tier: "national_plan" | "regional_plan" | "eu_regulation" | "technical_manual".
- dominant_scale: the spatial scale the document mostly addresses.

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
    direction = list(type = "string", enum = list("delivered", "degraded", "mixed")),
    scale = list(type = "string",
                 enum = list("field", "farm", "basin", "regional", "mixed", "unclear")),
    consistency = list(type = "string",
                       enum = list("consistent", "mixed", "contested")),
    evidence_strength = list(type = "string", enum = list("strong", "moderate", "weak")),
    justification = list(type = "string")),
  required = list("function", "direction", "scale", "consistency",
                  "evidence_strength", "justification"),
  additionalProperties = FALSE)

policy_schema <- list(
  type = "object",
  properties = list(
    technologies = list(type = "array",
                        items = list(type = "string", enum = as.list(tech_vocab))),
    evidence_tier = list(type = "string",
                         enum = list("national_plan", "regional_plan",
                                     "eu_regulation", "technical_manual")),
    dominant_scale = list(type = "string",
                          enum = list("field", "farm", "basin", "regional",
                                      "mixed", "unclear")),
    function_findings = list(type = "array", items = finding_schema),
    off_vocabulary_benefits = list(type = "string"),
    notes = list(type = "string")),
  required = list("technologies", "evidence_tier", "dominant_scale",
                  "function_findings", "off_vocabulary_benefits", "notes"),
  additionalProperties = FALSE)

codeable[, user_text := sprintf("DOCUMENT: %s\n\nTEXT:\n%s", doc_id, body)]

# DUAL-MODEL, N-DRAW EXTRACTION ###############################################

if (!run_enabled) {
  cat("\nGATED: POLICY_RUN_ENABLED != true - no billable API contacted.\n")
} else {

  cl_raw_file <- file.path(out_dir, "policy_extraction_raw_claude.rds")
  ds_raw_file <- file.path(out_dir, "policy_extraction_raw_deepseek.rds")
  items <- codeable[, .(custom_id = doc_id, user_text)]

  fire_draws <- function(cache_file, fire_one, label) {
    cache <- if (file.exists(cache_file)) as.data.table(readRDS(cache_file)) else
      data.table(custom_id = character(), draw = integer(),
                 status = character(), text = character())
    for (dr in seq_len(n_draws)) {
      done <- cache[draw == dr & status == "succeeded" & !is.na(text), custom_id]
      todo <- setdiff(items$custom_id, done)
      if (length(todo) == 0L) { cat(sprintf("%s draw %d/%d: all cached.\n", label, dr, n_draws)); next }
      cat(sprintf("%s draw %d/%d: firing %d.\n", label, dr, n_draws, length(todo)))
      res <- fire_one(items[custom_id %in% todo]); res[, draw := dr]
      cache <- rbind(cache[!(draw == dr & custom_id %in% todo)], res, fill = TRUE)
      saveRDS(cache, cache_file)
    }
    cache
  }
  fire_claude <- function(it) run_claude_direct_fun(
    items = it, system_prompt = policy_prompt, schema = policy_schema,
    max_tokens = 1500L, effort = "medium", concurrency = 6L, timeout = 300L)
  fire_deepseek <- function(it) run_deepseek_batch_fun(
    items = it, system_prompt = policy_prompt, temperature = 0.4, max_tokens = 1500L)

  cl_raw <- fire_draws(cl_raw_file, fire_claude, "Claude")
  ds_raw <- fire_draws(ds_raw_file, fire_deepseek, "DeepSeek")

  # PARSE + AGGREGATE ##########################################################

  parse_doc <- function(txt, cid, model, draw) {
    v <- tryCatch(jsonlite::parse_json(txt), error = function(e) NULL)
    if (is.null(v) || length(v$function_findings) == 0) return(NULL)
    rbindlist(lapply(v$function_findings, function(ff) data.table(
      rec_id = cid, model = model, draw = draw,
      `function` = ff[["function"]] %||% NA_character_,
      direction = ff$direction %||% NA_character_,
      scale = ff$scale %||% NA_character_,
      consistency = ff$consistency %||% NA_character_,
      evidence_strength = ff$evidence_strength %||% NA_character_,
      justification = ff$justification %||% NA_character_)), fill = TRUE)
  }
  parse_cache <- function(raw, model) {
    raw <- raw[status == "succeeded" & !is.na(text)]
    rbindlist(Map(parse_doc, raw$text, raw$custom_id, model, raw$draw), fill = TRUE)
  }
  draws_long <- rbind(parse_cache(cl_raw, "claude"),
                      parse_cache(ds_raw, "deepseek"), fill = TRUE)
  draws_long <- draws_long[!is.na(`function`)]
  fwrite(draws_long, file.path(out_dir, "policy_promise_draws.csv"))

  agg <- aggregate_extraction_fun(
    draws_long[, .(rec_id, model, draw, `function`, direction, scale,
                   consistency, evidence_strength)])
  fwrite(agg$consensus, file.path(out_dir, "policy_promise_map.csv"))
  fwrite(agg$reliability, file.path(out_dir, "policy_promise_reliability.csv"))

  # READOUT ####################################################################

  cat(sprintf("\nDraws: %d/model; %d findings over %d documents.\n",
              n_draws, nrow(draws_long), uniqueN(draws_long$rec_id)))
  cat("\nReliability (Krippendorff alpha):\n"); print(agg$reliability)
  prom <- agg$consensus[delivered_consensus == TRUE]
  cat(sprintf("\nPromised functions (consensus present + agreed): %d cells\n", nrow(prom)))
  print(prom[, .(n_docs = uniqueN(rec_id)), by = `function`][order(-n_docs)])
  core <- c("crop_production", "water_conservation", "soil_health", "risk_management")
  cat(sprintf("\nPromised in efficiency CORE: %.0f%% ; TAIL: %.0f%%\n",
              100 * prom[`function` %in% core, .N] / nrow(prom),
              100 * prom[!`function` %in% core, .N] / nrow(prom)))
}
