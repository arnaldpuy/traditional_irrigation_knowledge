# MOP-UP: RERUN OPENAI LEGS REJECTED BY THE ENQUEUED-TOKEN LIMIT #############
# HISTORICAL: produced the archived replies in datasets/output/arm and
# datasets/output/checkpoint. Depends on prompts and inputs not distributed
# (adversarial, partial and control-set legs); not part of the offline workflow.

# GATED behind MOPUP_RUN_ENABLED (FALSE by default).

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "httr2", "jsonlite", "readxl"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

run_enabled <- as.logical(Sys.getenv("MOPUP_RUN_ENABLED", "FALSE"))
n_samples <- 5L
chk <- here("datasets", "output", "checkpoint")
arm <- here("datasets", "output", "arm")

# Purge the poisoned all-errored caches so the runner retries -----------------

for (f in c(file.path(chk, "chk_adversarial_raw_openai.csv"),
            file.path(arm, "arm_partial_raw_openai.csv"),
            file.path(arm, "arm_severing_raw_openai.csv"),
            file.path(arm, "arm_policy_conservative_raw_openai.csv"))) {
  if (file.exists(f)) {
    d <- fread(f, colClasses = list(character = "text"))
    if (!any(d$status == "succeeded")) {
      file.remove(f); cat("purged failed cache:", basename(f), "\n")
    } else cat("cache has successes, kept:", basename(f), "\n")
  }
}

if (!run_enabled) {
  message("MOPUP_RUN_ENABLED = FALSE -> caches inspected only, API NOT called.")
  quit(save = "no")
}

# Leg 1: adversarial on the corpus (checkpoint leg 3 of Test 2) ---------------

corpus <- as.data.table(read_xlsx(here("datasets", "inputs",
                                       "assumption_list_updated.xlsx")))
setnames(corpus, tolower(names(corpus)))
corpus[, user_text := sprintf(
  "id_cite: %s\ntraditional_knowledge: %s\nunderlying_assumption: %s\nmain_category: %s",
  id_cite, traditional_knowledge, underlying_assumption, main_category)]
items <- corpus[rep(seq_len(nrow(corpus)), each = n_samples),
                .(id_cite, user_text)]
items[, sample := rep(seq_len(n_samples), times = nrow(corpus))]
items[, custom_id := sprintf("%s__s%d", id_cite, sample)]
items <- items[, .(id_cite, custom_id, user_text)]

adv_prompt <- paste(readLines(here("prompts", "efficiency_adversarial_prompt.txt"),
                              warn = FALSE), collapse = "\n")
schema_eff <- list(type = "object",
  properties = list(id_cite = list(type = "string"),
                    efficiency_legible = list(type = "boolean")),
  required = list("id_cite", "efficiency_legible"),
  additionalProperties = FALSE)

raw_adv <- run_three_models_fun(items[, .(custom_id, user_text)], adv_prompt,
                                schema_eff, tag = "chk_adversarial",
                                out_dir = chk, max_tokens = 400L)
fwrite(raw_adv, file.path(chk, "chk_adversarial_raw_all.csv"))

vals <- vapply(raw_adv$text, function(t) {
  if (is.na(t)) return(NA)
  o <- tryCatch(fromJSON(t), error = function(e) NULL)
  v <- o$efficiency_legible
  if (is.null(v)) NA else as.logical(v)
}, logical(1))
raw_adv[, legible := vals]
raw_adv[, id_cite := sub("__s.*$", "", custom_id)]
maj <- raw_adv[!is.na(legible),
               .(legible = as.logical(median(legible) >= 0.5)),
               by = .(provider, id_cite)]
fwrite(maj, file.path(chk, "chk_adversarial_majority.csv"))
cat("\n== ADVERSARIAL on corpus (Test 2), all three providers ==\n")
print(maj[, .(n = .N, irreducible = round(mean(!legible), 3)), by = provider])

# Leg 1b: partial (three-level) legibility, OpenAI rerun (Test 3) -------------

partial_prompt <- paste(readLines(here("prompts", "efficiency_partial_prompt.txt"),
                                  warn = FALSE), collapse = "\n")
schema_partial <- list(type = "object",
  properties = list(id_cite = list(type = "string"),
    legibility = list(type = "string",
                      enum = list("full", "partial", "irreducible"))),
  required = list("id_cite", "legibility"), additionalProperties = FALSE)
raw_par <- run_three_models_fun(items[, .(custom_id, user_text)],
                                partial_prompt, schema_partial,
                                tag = "arm_partial", out_dir = arm,
                                max_tokens = 400L)
fwrite(raw_par, file.path(arm, "arm_partial_raw_all.csv"))
cat("\n== PARTIAL legibility (Test 3) success by provider ==\n")
print(raw_par[, .(succeeded = sum(status == "succeeded"), n = .N), by = provider])

# Leg 1c: severing strength, OpenAI rerun (Test 8), split in halves ----------

sev_levels <- list("direct", "indirect", "potential", "none")
sev_prompt <- paste(readLines(here("prompts", "severing_strength_prompt.txt"),
                              warn = FALSE), collapse = "\n")
schema_sev <- list(type = "object",
  properties = list(id_cite = list(type = "string"),
    drip = list(type = "string", enum = sev_levels),
    soil_sensor = list(type = "string", enum = sev_levels),
    remote_sensing = list(type = "string", enum = sev_levels),
    on_demand = list(type = "string", enum = sev_levels),
    app_scheduling = list(type = "string", enum = sev_levels)),
  required = list("id_cite", "drip", "soil_sensor", "remote_sensing",
                  "on_demand", "app_scheduling"),
  additionalProperties = FALSE)
sev_cache <- file.path(arm, "arm_severing_raw_openai.csv")
if (!file.exists(sev_cache)) {
  half <- split(seq_len(nrow(items)), cut(seq_len(nrow(items)), 2, labels = FALSE))
  res_halves <- lapply(half, function(ix)
    run_openai_batch_fun(items[ix, .(custom_id, user_text)], sev_prompt,
                         reasoning_effort = "low",
                         max_completion_tokens = 600L, poll_seconds = 20L))
  res_s <- rbindlist(res_halves)
  if (any(res_s$status == "succeeded")) fwrite(res_s, sev_cache) else
    warning("severing openai halves failed; cache not written")
}
raw_sev <- run_three_models_fun(items[, .(custom_id, user_text)], sev_prompt,
                                schema_sev, tag = "arm_severing",
                                out_dir = arm, max_tokens = 600L)
fwrite(raw_sev, file.path(arm, "arm_severing_raw_all.csv"))
cat("\n== SEVERING strength (Test 8) success by provider ==\n")
print(raw_sev[, .(succeeded = sum(status == "succeeded"), n = .N), by = provider])

# Leg 2: policy conservative coding (Test 9) ----------------------------------

pol_dir <- here("datasets", "inputs", "policy_corpus")
pdfs <- list.files(pol_dir, pattern = "\\.pdf$", full.names = TRUE)
text_dt <- rbindlist(lapply(pdfs, extract_doc_text_fun, max_chars = 130000L))
text_dt[, doc_id := tools::file_path_sans_ext(file)]
codeable <- text_dt[n_chars >= 500]

source_lines <- readLines(here("code", "36_billable_arm_policy.R"), warn = FALSE)
env36 <- new.env()
fun_vocab <- c("crop_production", "water_conservation", "soil_health",
               "risk_management", "social_cohesion", "biodiversity",
               "microclimate_regulation", "pest_control", "groundwater_recharge",
               "livestock", "medicinal_food", "recreation_wellbeing")
tech_vocab <- c("drip", "soil_sensor", "remote_sensing", "on_demand",
                "app_scheduling", "flow_metering", "modernization_general",
                "other")
prompt_start <- grep("^conservative_prompt <- sprintf", source_lines)
prompt_end <- grep("^cat\\(sprintf\\(\"Prompt SHA-256", source_lines)[1] - 1
eval(parse(text = source_lines[prompt_start:prompt_end]))
schema_start <- grep("^schema_pol <- list", source_lines)
schema_end <- grep("^# Items: one request per document", source_lines)[1] - 1
eval(parse(text = source_lines[schema_start:schema_end]))

itp <- codeable[rep(seq_len(nrow(codeable)), each = n_samples), .(doc_id, body)]
itp[, sample := rep(seq_len(n_samples), times = nrow(codeable))]
itp[, custom_id := sprintf("%s__s%d", doc_id, sample)]
itp[, user_text := sprintf("doc_id: %s\n\nDOCUMENT TEXT:\n%s", doc_id, body)]

pol_cache <- file.path(arm, "arm_policy_conservative_raw_openai.csv")
if (!file.exists(pol_cache)) {
  half <- split(seq_len(nrow(itp)), cut(seq_len(nrow(itp)), 2, labels = FALSE))
  res_halves <- lapply(half, function(ix)
    run_openai_batch_fun(itp[ix, .(custom_id, user_text)], conservative_prompt,
                         reasoning_effort = "low",
                         max_completion_tokens = 2000L, poll_seconds = 20L))
  res_o <- rbindlist(res_halves)
  if (any(res_o$status == "succeeded")) fwrite(res_o, pol_cache) else
    warning("policy openai halves failed; cache not written")
}
raw_pol <- run_three_models_fun(itp[, .(custom_id, user_text)],
                                conservative_prompt, schema_pol,
                                tag = "arm_policy_conservative", out_dir = arm,
                                max_tokens = 2000L, deepseek_concurrency = 4L)
fwrite(raw_pol, file.path(arm, "arm_policy_conservative_raw_all.csv"))
cat("\n== POLICY conservative (Test 9) success by provider ==\n")
print(raw_pol[, .(succeeded = sum(status == "succeeded"), n = .N), by = provider])
cat("\nMOP-UP COMPLETE.\n")
