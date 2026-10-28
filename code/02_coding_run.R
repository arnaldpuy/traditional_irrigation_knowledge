# REPRODUCIBLE CODING RUN (Anthropic Batch API / DeepSeek) ####################

# ---------------------------------------------------------------------------
# GATED. coding_run_enabled is FALSE by default. While FALSE the script still
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

coding_run_enabled  <- as.logical(Sys.getenv("CODING_RUN_ENABLED", "FALSE"))
provider            <- Sys.getenv("CODING_PROVIDER", "anthropic")  # anthropic | deepseek
prompt_choice       <- Sys.getenv("CODING_PROMPT", "analytical")  # subcategory (Carmen) | analytical (PNAS)
n_samples           <- as.integer(Sys.getenv("CODING_N", "5"))
effort              <- "high"           # anthropic only (ignored when thinking off)
anthropic_thinking  <- "disabled"       # "disabled" | "adaptive"
poll_interval       <- 60               # anthropic only
deepseek_temperature <- 0.7             # >0 so DeepSeek N>1 samples vary
deepseek_concurrency <- 8L

anthropic_model <- "claude-opus-4-8"
deepseek_model  <- "deepseek-chat"
model <- if (provider == "anthropic") anthropic_model else deepseek_model
max_tokens <- if (provider == "deepseek") 800L else
  if (prompt_choice == "analytical") 800L else 600L

prompt_file <- if (prompt_choice == "analytical") {
  here("prompts", "analytical_coding_prompt_v1.txt")
} else {
  here("prompts", "subcategory_prompt_v2.txt")
}
input_file <- here("datasets", "inputs", "assumption_list_updated.xlsx")  # canonical
out <- here("datasets", "output")
tag <- paste(prompt_choice, provider, sep = "_")

# Read the input list ---------------------------------------------------------

inp <- as.data.table(read_xlsx(input_file))
setnames(inp, tolower(names(inp)))
stopifnot(all(c("id_cite", "traditional_knowledge", "underlying_assumption",
                "main_category") %in% names(inp)))

# Build the system prompt (strip the RUN CONFIGURATION preamble) ---------------

raw_prompt <- readLines(prompt_file, warn = FALSE)
start <- grep("^SYSTEM ROLE", raw_prompt)[1]
system_prompt <- paste(raw_prompt[start:length(raw_prompt)], collapse = "\n")
prompt_sha256 <- digest(system_prompt, algo = "sha256", serialize = FALSE)
schema <- coding_schema_fun(prompt_choice)

user_text_fun <- function(r) {
  sprintf(paste("id_cite: %s", "traditional_knowledge: %s",
                "underlying_assumption: %s", "main_category: %s", sep = "\n"),
          r$id_cite, r$traditional_knowledge, r$underlying_assumption,
          r$main_category)
}
inp[, user_text := vapply(seq_len(.N), function(i) user_text_fun(inp[i]),
                          character(1))]
items <- inp[rep(seq_len(nrow(inp)), each = n_samples),
             .(id_cite, user_text)]
items[, sample := rep(seq_len(n_samples), times = nrow(inp))]
items[, custom_id := sprintf("%s__s%d", id_cite, sample)]

# Save the manifest (+ Anthropic payload) for audit / SM (always) -------------

manifest <- data.table(prompt_choice = prompt_choice, provider = provider,
                       model = model, prompt_file = basename(prompt_file),
                       prompt_sha256 = prompt_sha256, n_assumptions = nrow(inp),
                       n_samples = n_samples,
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
             file.path(out, sprintf("batch_payload_%s.json", prompt_choice)))
}

cat(sprintf("[%s] built %d requests (%d assumptions x %d samples).\n",
            tag, nrow(items), nrow(inp), n_samples))
cat("Prompt SHA-256:", prompt_sha256, "\n")
if (provider == "deepseek" && n_samples > 1L && deepseek_temperature == 0)
  message("NOTE: DeepSeek at temperature 0 gives near-identical samples; ",
          "raise deepseek_temperature for meaningful N>1 self-consistency.")

if (!coding_run_enabled) {
  message(sprintf("\ncoding_run_enabled = FALSE -> [%s] payload built, API NOT ",
                  tag), "called. Set the flag + the provider API key, re-run.")
  quit(save = "no")
}

# Run the provider ------------------------------------------------------------

if (provider == "anthropic") {
  batch <- submit_batch_fun(requests)
  cat("Submitted batch:", batch$id, "\n")
  batch <- poll_batch_fun(batch$id, interval = poll_interval)
  res <- fetch_batch_results_fun(batch)
} else {
  res <- run_deepseek_batch_fun(items, system_prompt, model = model,
                                temperature = deepseek_temperature,
                                max_tokens = max_tokens,
                                concurrency = deepseek_concurrency)
}
fwrite(res, file.path(out, sprintf("coding_results_raw_%s.csv", tag)))
cat(sprintf("Succeeded: %d / %d\n", sum(res$status == "succeeded"), nrow(res)))

res[, sample := as.integer(sub(".*__s", "", custom_id))]
long <- rbindlist(lapply(seq_len(nrow(res)),
                         function(i) flatten_coding_record_fun(res$text[i],
                                                               prompt_choice)),
                  fill = TRUE)

# Majority vote across the N samples ------------------------------------------

vote_fields <- if (prompt_choice == "analytical") {
  c("epistemic_type", "functions", "efficiency_legible", "sensory_channels",
    "n_env_variables", "eroded_by", "corroboration", "measured_variable",
    "time_horizon", "inferential_form", "temporal_framing", "knowledge_mode",
    "is_saying", "is_value_statement", "practice")
} else {
  c("primary_subcategory", "secondary_subcategory", "cross_subcategory",
    "cross_main_category", "main_category_flag")
}
coded <- majority_vote_fun(long, id_col = "id_cite", fields = vote_fields)

# Attach mean model-reported confidence -> write the coded corpus -------------

conf <- long[, .(model_confidence = mean(as.numeric(confidence), na.rm = TRUE)),
             by = id_cite]
coded <- conf[coded, on = "id_cite"]
fwrite(coded, file.path(out, sprintf("coding_majority_%s.csv", tag)))
cat(sprintf("\nWrote coding_majority_%s.csv (%d assumptions). ", tag, nrow(coded)))
cat(sprintf("Mean inter-sample agreement: %.3f\n",
            coded[, mean(mean_agreement, na.rm = TRUE)]))
