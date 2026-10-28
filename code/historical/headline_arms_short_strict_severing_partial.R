# BILLABLE ARM (CORPUS LEGS): PARTIAL LEGIBILITY, PROMPT SENSITIVITY, SEVERING #
# HISTORICAL: produced the archived replies in datasets/output/arm and
# datasets/output/checkpoint. Depends on prompts and inputs not distributed
# (adversarial, partial and control-set legs); not part of the offline workflow.

# ---------------------------------------------------------------------------
# GATED behind ARM_RUN_ENABLED (FALSE by default). Raw per-provider responses
# ---------------------------------------------------------------------------

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "httr2", "jsonlite", "readxl",
                          "digest"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# Configuration ---------------------------------------------------------------

run_enabled <- as.logical(Sys.getenv("ARM_RUN_ENABLED", "FALSE"))
n_samples <- as.integer(Sys.getenv("ARM_N", "5"))
out <- here("datasets", "output", "arm")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Items: the 475 corpus expanded to N samples ---------------------------------

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

# The four legs: prompt file, schema, output tokens ----------------------------

sev_levels <- list("direct", "indirect", "potential", "none")
legs <- list(
  partial = list(
    prompt = "efficiency_partial_prompt.txt", max_tokens = 400L,
    schema = list(type = "object",
      properties = list(id_cite = list(type = "string"),
        legibility = list(type = "string",
                          enum = list("full", "partial", "irreducible"))),
      required = list("id_cite", "legibility"), additionalProperties = FALSE)),
  short = list(
    prompt = "efficiency_short_prompt.txt", max_tokens = 400L,
    schema = list(type = "object",
      properties = list(id_cite = list(type = "string"),
                        efficiency_legible = list(type = "boolean")),
      required = list("id_cite", "efficiency_legible"),
      additionalProperties = FALSE)),
  strict = list(
    prompt = "efficiency_strict_prompt.txt", max_tokens = 400L,
    schema = list(type = "object",
      properties = list(id_cite = list(type = "string"),
                        efficiency_legible = list(type = "boolean")),
      required = list("id_cite", "efficiency_legible"),
      additionalProperties = FALSE)),
  severing = list(
    prompt = "severing_strength_prompt.txt", max_tokens = 600L,
    schema = list(type = "object",
      properties = list(id_cite = list(type = "string"),
        drip = list(type = "string", enum = sev_levels),
        soil_sensor = list(type = "string", enum = sev_levels),
        remote_sensing = list(type = "string", enum = sev_levels),
        on_demand = list(type = "string", enum = sev_levels),
        app_scheduling = list(type = "string", enum = sev_levels)),
      required = list("id_cite", "drip", "soil_sensor", "remote_sensing",
                      "on_demand", "app_scheduling"),
      additionalProperties = FALSE)))

for (nm in names(legs)) {
  p <- paste(readLines(here("prompts", legs[[nm]]$prompt), warn = FALSE), collapse = "\n")
  legs[[nm]]$system_prompt <- p
  cat(sprintf("[%s] prompt %s | SHA-256 %s\n", nm, legs[[nm]]$prompt,
              digest(p, "sha256", serialize = FALSE)))
}
cat(sprintf("N samples: %d | requests per model per leg: %d | legs: %d\n",
            n_samples, nrow(items), length(legs)))

if (!run_enabled) {
  message("\nARM_RUN_ENABLED = FALSE -> prompts + items built, API NOT called.")
  quit(save = "no")
}

# Fire the legs sequentially ---------------------------------------------------

for (nm in names(legs)) {
  cat(sprintf("\n########## LEG %s ##########\n", nm))
  raw <- run_three_models_fun(items, legs[[nm]]$system_prompt,
                              legs[[nm]]$schema, tag = paste0("arm_", nm),
                              out_dir = out, max_tokens = legs[[nm]]$max_tokens)
  fwrite(raw, file.path(out, sprintf("arm_%s_raw_all.csv", nm)))
  ok <- raw[, .(succeeded = sum(status == "succeeded"), n = .N), by = provider]
  print(ok)
}

cat("\nARM CORPUS LEGS COMPLETE. Aggregation follows in the analysis notebook.\n")
