# FUNCTION-ATTRIBUTION ARMS (PILOT) ###########################################

# ---------------------------------------------------------------------------
# GATED. function_arm_run_enabled is FALSE by default. While FALSE the script
# ---------------------------------------------------------------------------

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "httr2", "jsonlite", "digest",
                          "readxl"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# Run configuration -----------------------------------------------------------

function_arm_run_enabled <- as.logical(Sys.getenv("FUNCTION_ARM_RUN_ENABLED",
                                                  "FALSE"))
phrasing  <- Sys.getenv("ATTRIBUTION_PHRASING", "strict")  # standard|strict|broad
provider  <- Sys.getenv("CODING_PROVIDER", "anthropic")    # anthropic|deepseek|openai
n_samples <- as.integer(Sys.getenv("CODING_N", "5"))
effort               <- "high"
anthropic_thinking   <- "disabled"
poll_interval        <- 60
deepseek_temperature <- 0.7
deepseek_concurrency <- 8L
openai_effort        <- "low"

anthropic_model <- "claude-opus-4-8"
deepseek_model  <- "deepseek-v4-pro"
openai_model    <- "gpt-5.4"
model <- switch(provider, anthropic = anthropic_model,
                deepseek = deepseek_model, openai = openai_model)

max_tokens <- if (provider == "deepseek") 3000L else 800L
openai_max_completion <- 2500L

stopifnot(phrasing %in% c("standard", "strict", "broad"),
          provider %in% c("anthropic", "deepseek", "openai"))
prompt_file <- here("prompts", "analytical_coding_prompt_v1.txt")
input_file <- here("datasets", "inputs", "assumption_list_updated.xlsx")
out <- here("datasets", "output")
tag <- sprintf("attribution_%s_%s", phrasing, provider)

# Pre-registered pilot criterion ----------------------------------------------

pilot_criterion <- list(
  min_shift = 0.3,
  direction = if (phrasing == "strict") "decrease" else "increase")

# Build the variant system prompt ----------------------------------------------

attribution_block <- c(
  strict = paste(
    "functions (multi) — only the purposes the ASSUMPTION ITSELF serves. Code a",
    "  function only if the content of the assumption, read on its own, is what",
    "  serves it. Do not code a function that is served by the wider practice the",
    "  assumption belongs to, or by the setting in which it is used, unless the",
    "  assumption states or entails it. When in doubt, omit the function.",
    "  Allowed values:", sep = "\n"),
  broad = paste(
    "functions (multi) — every purpose that the assumption, or the practice it",
    "  belongs to, serves or contributes to, whether directly or indirectly. Code",
    "  a function if the assumption plausibly bears on it, including where the",
    "  contribution is a side effect or is mediated by the practice. When in",
    "  doubt, include the function.",
    "  Allowed values:", sep = "\n"))

raw_prompt <- readLines(prompt_file, warn = FALSE, encoding = "UTF-8")
start <- grep("^SYSTEM ROLE", raw_prompt)[1]
fn_line <- grep("^functions \\(multi\\)", raw_prompt)
stopifnot(length(start) == 1L, length(fn_line) == 1L, fn_line > start)

if (phrasing != "standard") raw_prompt[fn_line] <- attribution_block[[phrasing]]
system_prompt <- paste(raw_prompt[start:length(raw_prompt)], collapse = "\n")
prompt_sha256 <- digest(system_prompt, algo = "sha256", serialize = FALSE)
schema <- coding_schema_fun("analytical")

# Archive the variant prompt so it ships verbatim alongside the frozen one ----

variant_file <- if (phrasing == "standard") prompt_file else {
  f <- here("prompts",
            sprintf("analytical_coding_prompt_v1_attribution_%s.txt", phrasing))
  writeLines(raw_prompt, f, useBytes = TRUE)
  f
}

# Read the input list ----------------------------------------------------------

inp <- as.data.table(read_xlsx(input_file))
setnames(inp, tolower(names(inp)))
stopifnot(all(c("id_cite", "traditional_knowledge", "underlying_assumption",
                "main_category") %in% names(inp)))

user_text_fun <- function(r) {
  sprintf(paste("id_cite: %s", "traditional_knowledge: %s",
                "underlying_assumption: %s", "main_category: %s", sep = "\n"),
          r$id_cite, r$traditional_knowledge, r$underlying_assumption,
          r$main_category)
}
inp[, user_text := vapply(seq_len(.N), function(i) user_text_fun(inp[i]),
                          character(1))]
items <- inp[rep(seq_len(nrow(inp)), each = n_samples), .(id_cite, user_text)]
items[, sample := rep(seq_len(n_samples), times = nrow(inp))]
items[, custom_id := sprintf("%s__s%d", id_cite, sample)]

# Save the manifest (+ Anthropic payload) for audit / SM (always) -------------

manifest <- data.table(arm = tag, phrasing = phrasing, provider = provider,
                       model = model,
                       prompt_file = basename(variant_file),
                       prompt_sha256 = prompt_sha256,
                       base_prompt_file = basename(prompt_file),
                       n_assumptions = nrow(inp), n_samples = n_samples,
                       thinking = if (provider == "anthropic") anthropic_thinking
                                  else NA_character_,
                       temperature = if (provider == "deepseek")
                         deepseek_temperature else NA_real_,
                       effort = effort, max_tokens = max_tokens,
                       n_requests = nrow(items))
fwrite(manifest, file.path(out, sprintf("coding_manifest_%s.csv", tag)))

if (provider == "anthropic") {
  requests <- lapply(seq_len(nrow(items)), function(i)
    build_coding_request_fun(custom_id = items$custom_id[i],
                             system_prompt = system_prompt,
                             user_text = items$user_text[i], schema = schema,
                             model = model, max_tokens = max_tokens,
                             effort = effort, thinking = anthropic_thinking))
  writeLines(toJSON(list(requests = requests), auto_unbox = TRUE, null = "null",
                    pretty = TRUE),
             file.path(out, sprintf("batch_payload_%s.json", tag)))
}

cat(sprintf("[%s] built %d requests (%d assumptions x %d samples).\n",
            tag, nrow(items), nrow(inp), n_samples))
cat("Variant prompt:", basename(variant_file), "\n")
cat("Prompt SHA-256:", prompt_sha256, "\n")
if (phrasing == "standard") {
  cat("Frozen prompt used verbatim (standard phrasing).\n")
} else {
  cat(sprintf("Replaced line %d of the frozen prompt; all other fields identical.\n",
              fn_line))
}

if (provider == "openai")
  cat(sprintf("openai: %d requests will be auto-chunked under the org's enqueued-token ceiling.\n",
              nrow(items)))

if (!function_arm_run_enabled) {
  message(sprintf(paste("\nfunction_arm_run_enabled = FALSE -> [%s] prompt,",
                        "payload and manifest built, API NOT called. Set the",
                        "flag + the provider API key, re-run."), tag))
  quit(save = "no")
}

# Run the provider -------------------------------------------------------------

if (provider == "anthropic") {
  batch <- submit_batch_fun(requests)
  cat("Submitted batch:", batch$id, "\n")
  batch <- poll_batch_fun(batch$id, interval = poll_interval)
  res <- fetch_batch_results_fun(batch)
} else if (provider == "deepseek") {
  res <- run_deepseek_batch_fun(items, system_prompt, model = model,
                                temperature = deepseek_temperature,
                                max_tokens = max_tokens,
                                concurrency = deepseek_concurrency)
} else {
  res <- run_openai_batch_fun(items[, .(custom_id, user_text)], system_prompt,
                              model = model, reasoning_effort = openai_effort,
                              max_completion_tokens = openai_max_completion)
}
fwrite(res, file.path(out, sprintf("coding_results_raw_%s.csv", tag)))
cat(sprintf("Succeeded: %d / %d\n", sum(res$status == "succeeded"), nrow(res)))

# Flatten and collapse by majority vote ---------------------------------------

res[, sample := as.integer(sub(".*__s", "", custom_id))]
long <- rbindlist(lapply(seq_len(nrow(res)),
                         function(i) flatten_coding_record_fun(res$text[i],
                                                               "analytical")),
                  fill = TRUE)
vote_fields <- c("epistemic_type", "functions", "efficiency_legible",
                 "sensory_channels", "n_env_variables", "eroded_by",
                 "corroboration", "measured_variable", "time_horizon",
                 "inferential_form", "temporal_framing", "knowledge_mode",
                 "is_saying", "is_value_statement", "practice")
coded <- majority_vote_fun(long, id_col = "id_cite", fields = vote_fields)
conf <- long[, .(model_confidence = mean(as.numeric(confidence), na.rm = TRUE)),
             by = id_cite]
coded <- conf[coded, on = "id_cite"]
fwrite(coded, file.path(out, sprintf("coding_majority_%s.csv", tag)))
cat(sprintf("\nWrote coding_majority_%s.csv (%d assumptions).\n", tag,
            nrow(coded)))

# Pilot verdict ----------------------------------------------------------------

funcs12 <- c("crop_production", "water_conservation", "soil_health",
             "risk_management", "groundwater_recharge",
             "microclimate_regulation", "biodiversity", "pest_control",
             "livestock", "medicinal_food", "social_cohesion",
             "recreation_wellbeing")
core2 <- c("crop_production", "water_conservation")
breadth_fun <- function(x) {
  fs <- lapply(strsplit(x, ";", fixed = TRUE),
               function(f) intersect(trimws(f), funcs12))
  list(mean_functions = mean(lengths(fs)),
       share_above_two = mean(lengths(fs) > 2),
       omitted_core2 = mean(vapply(fs, function(f) any(!f %in% core2),
                                   logical(1))))
}

base_file <- if (phrasing == "standard") "" else file.path(out,
  sprintf("coding_majority_attribution_standard_%s.csv", provider))
if (!file.exists(base_file) && provider == "anthropic" && phrasing != "standard")
  base_file <- file.path(out,
    sprintf("coding_majority_analytical_%s.csv", provider))
arm <- breadth_fun(coded$functions)
if (!file.exists(base_file)) {
  cat(sprintf(paste0("\nNo standard-phrasing baseline for %s yet; this arm IS ",
                     "the baseline.\nmean functions/assumption %.2f | >2 functions ",
                     "%.0f%% | omitted under core-2 %.0f%%\n"),
              provider, arm$mean_functions, 100 * arm$share_above_two,
              100 * arm$omitted_core2))
  fwrite(data.table(arm = tag, phrasing = phrasing, provider = provider,
                    arm_mean_functions = arm$mean_functions,
                    arm_omitted_core2 = arm$omitted_core2, passed = NA),
         file.path(out, sprintf("pilot_verdict_%s.csv", tag)))
  quit(save = "no")
}
base <- breadth_fun(fread(base_file)$functions)
shift <- arm$mean_functions - base$mean_functions
passed <- if (pilot_criterion$direction == "decrease") {
  shift <= -pilot_criterion$min_shift
} else {
  shift >= pilot_criterion$min_shift
}

cat("\n--- PILOT VERDICT ------------------------------------------------\n")
cat(sprintf("%-28s %8s %8s %8s\n", "quantity", "frozen", phrasing, "shift"))
cat(sprintf("%-28s %8.2f %8.2f %+8.2f\n", "mean functions/assumption",
            base$mean_functions, arm$mean_functions, shift))
cat(sprintf("%-28s %7.0f%% %7.0f%% %+7.0f%%\n", "share with >2 functions",
            100 * base$share_above_two, 100 * arm$share_above_two,
            100 * (arm$share_above_two - base$share_above_two)))
cat(sprintf("%-28s %7.0f%% %7.0f%% %+7.0f%%\n", "omitted under core-2",
            100 * base$omitted_core2, 100 * arm$omitted_core2,
            100 * (arm$omitted_core2 - base$omitted_core2)))
cat(sprintf("\ncriterion: %s of at least %.1f functions/assumption -> %s\n",
            pilot_criterion$direction, pilot_criterion$min_shift,
            if (passed) "PASS, the factor binds; run the remaining arms"
            else "INERT, do not pay for the remaining arms"))
cat("------------------------------------------------------------------\n")

fwrite(data.table(arm = tag, phrasing = phrasing, provider = provider,
                  base_mean_functions = base$mean_functions,
                  arm_mean_functions = arm$mean_functions, shift = shift,
                  base_omitted_core2 = base$omitted_core2,
                  arm_omitted_core2 = arm$omitted_core2,
                  criterion_min_shift = pilot_criterion$min_shift,
                  criterion_direction = pilot_criterion$direction,
                  passed = passed),
       file.path(out, sprintf("pilot_verdict_%s.csv", tag)))
