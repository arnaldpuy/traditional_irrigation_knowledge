# BILLABLE-ARM CHECKPOINT: STANDARD + ADVERSARIAL EFFICIENCY RECODE ##########
# HISTORICAL: produced the archived replies in datasets/output/arm and
# datasets/output/checkpoint. Depends on prompts and inputs not distributed
# (adversarial, partial and control-set legs); not part of the offline workflow.

# ---------------------------------------------------------------------------
# GATED behind CHECKPOINT_RUN_ENABLED (FALSE by default). Raw per-provider
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

run_enabled <- as.logical(Sys.getenv("CHECKPOINT_RUN_ENABLED", "FALSE"))
n_samples <- as.integer(Sys.getenv("CHECKPOINT_N", "5"))
out <- here("datasets", "output", "checkpoint")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

schema_eff <- list(type = "object",
  properties = list(id_cite = list(type = "string"),
                    efficiency_legible = list(type = "boolean")),
  required = list("id_cite", "efficiency_legible"),
  additionalProperties = FALSE)

# Build the item sets (expanded to N samples) ---------------------------------

user_text_fun <- function(id, tk, ua, mc) {
  sprintf("id_cite: %s\ntraditional_knowledge: %s\nunderlying_assumption: %s\nmain_category: %s",
          id, tk, ua, mc)
}
expand_fun <- function(base) {
  it <- base[rep(seq_len(nrow(base)), each = n_samples)]
  it[, sample := rep(seq_len(n_samples), times = nrow(base))]
  it[, custom_id := sprintf("%s__s%d", id_cite, sample)]
  it[, .(id_cite, custom_id, user_text)]
}

corpus <- as.data.table(read_xlsx(here("datasets", "inputs",
                                       "assumption_list_updated.xlsx")))
setnames(corpus, tolower(names(corpus)))
corpus[, user_text := user_text_fun(id_cite, traditional_knowledge,
                                    underlying_assumption, main_category)]
items_corpus <- expand_fun(corpus[, .(id_cite, user_text)])

ctrl <- fread(here("datasets", "output", "control_set_items.csv"))
ctrl[, user_text := user_text_fun(id, assumption, assumption, "control")]
ctrl_base <- ctrl[, .(id_cite = id, user_text)]
items_ctrl <- expand_fun(ctrl_base)

std_prompt <- paste(readLines(here("prompts", "efficiency_standard_prompt.txt"), warn = FALSE),
                    collapse = "\n")
adv_prompt <- paste(readLines(here("prompts", "efficiency_adversarial_prompt.txt"), warn = FALSE),
                    collapse = "\n")
cat(sprintf("Standard prompt SHA-256: %s\n", digest(std_prompt, "sha256", serialize = FALSE)))
cat(sprintf("Adversarial prompt SHA-256: %s\n", digest(adv_prompt, "sha256", serialize = FALSE)))
cat(sprintf("N samples: %d | corpus requests/model: %d | control requests/model: %d\n",
            n_samples, nrow(items_corpus), nrow(items_ctrl)))

if (!run_enabled) {
  message("\nCHECKPOINT_RUN_ENABLED = FALSE -> item sets + prompts built, API NOT called.")
  quit(save = "no")
}

# Helper: parse efficiency_legible and majority-vote per (id, provider) -------

parse_legible <- function(raw) {
  vals <- vapply(raw$text, function(t) {
    if (is.na(t)) return(NA)
    o <- tryCatch(fromJSON(t), error = function(e) NULL)
    v <- o$efficiency_legible
    if (is.null(v)) NA else as.logical(v)
  }, logical(1))
  raw[, legible := vals]
  raw[, id_cite := sub("__s.*$", "", custom_id)]
  raw[!is.na(legible), .(legible = as.logical(median(legible) >= 0.5)),
      by = .(provider, id_cite)]
}

report_share <- function(maj, label) {
  s <- maj[, .(n = .N, irreducible = round(mean(!legible), 3)), by = provider]
  cat(sprintf("\n== %s ==\n", label)); print(s); s
}

# (1) + (3): STANDARD and ADVERSARIAL on the 475 corpus -----------------------

raw_std <- run_three_models_fun(items_corpus, std_prompt, schema_eff,
                                tag = "chk_standard", out_dir = out)
fwrite(raw_std, file.path(out, "chk_standard_raw_all.csv"))
maj_std <- parse_legible(raw_std)
fwrite(maj_std, file.path(out, "chk_standard_majority.csv"))
report_share(maj_std, "STANDARD on corpus (anchor; expect Claude ~0.89)")

raw_adv <- run_three_models_fun(items_corpus, adv_prompt, schema_eff,
                                tag = "chk_adversarial", out_dir = out)
fwrite(raw_adv, file.path(out, "chk_adversarial_raw_all.csv"))
maj_adv <- parse_legible(raw_adv)
fwrite(maj_adv, file.path(out, "chk_adversarial_majority.csv"))
report_share(maj_adv, "ADVERSARIAL on corpus (Test 2; expect lower but high)")

# (2): STANDARD on the control set (reviewer Test 6) --------------------------

raw_ctrl <- run_three_models_fun(items_ctrl, std_prompt, schema_eff,
                                 tag = "chk_control", out_dir = out)
fwrite(raw_ctrl, file.path(out, "chk_control_raw_all.csv"))
maj_ctrl <- parse_legible(raw_ctrl)
exp <- ctrl[, .(id_cite = id, expected = expected_efficiency_legible,
                type = control_type)]
maj_ctrl <- exp[maj_ctrl, on = "id_cite"]
maj_ctrl[, correct := legible == expected]
cat("\n== CONTROL SET (Test 6): accuracy by provider ==\n")
print(maj_ctrl[, .(n = .N, accuracy = round(mean(correct), 3)),
               by = provider])
cat("\nControl detail (any provider miss):\n")
print(maj_ctrl[correct == FALSE][order(id_cite),
               .(provider, id_cite, type, expected, legible)])
fwrite(maj_ctrl, file.path(out, "chk_control_majority.csv"))

cat("\nCHECKPOINT COMPLETE. Inspect the three blocks above before firing the full arm.\n")
