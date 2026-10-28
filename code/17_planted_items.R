# PLANTED ITEMS: DOES THE INSTRUMENT GET THE EASY CASES RIGHT? ################

# GATED: set PLANTED_RUN_ENABLED=true to call the APIs. Unset, the script builds

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "readxl", "jsonlite", "digest"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# CONFIGURATION ###############################################################

run_enabled <- tolower(Sys.getenv("PLANTED_RUN_ENABLED", "false")) == "true"
n_samples <- as.integer(Sys.getenv("PLANTED_N", "5"))
n_camouflage <- as.integer(Sys.getenv("PLANTED_CAMOUFLAGE", "48"))
out <- here("datasets", "output", "arm")
dir.create(out, showWarnings = FALSE, recursive = TRUE)
set.seed(20260730)

vote_rules <- c(`any draw` = 0.2, majority = 0.6, unanimous = 1.0)
deepseek_model <- "deepseek-v4-pro"
max_tokens <- 3000L

# THE SIGNED KEY ##############################################################

key_file <- here("datasets", "inputs", "planted_items.xlsx")
if (!file.exists(key_file))
  stop("datasets/inputs/planted_items.xlsx not found. The draft is not a key: ",
       "sign it (see PLANTED_ITEMS.md) and rename it before running.")

key <- as.data.table(read_xlsx(key_file))
key[, signed_by := trimws(as.character(signed_by))]
dropped <- key[is.na(signed_by) | signed_by == "" | is.na(planted_key)]
key <- key[!(is.na(signed_by) | signed_by == "" | is.na(planted_key))]
key[, planted_key := as.logical(planted_key)]

cat(sprintf("signed planted items: %d (%d reducible, %d irreducible)\n",
            nrow(key), sum(key$planted_key), sum(!key$planted_key)))
if (nrow(dropped))
  cat(sprintf("dropped as unsigned: %s\n", paste(dropped$id_cite, collapse = ", ")))

# Camouflage: real assumptions, stratified on the planted set's categories -----

corpus <- as.data.table(read_xlsx(here("datasets", "inputs",
                                      "assumption_list_updated.xlsx")))
setnames(corpus, tolower(names(corpus)))
stopifnot(!any(key$id_cite %in% corpus$id_cite))

quota <- key[, .(share = .N / nrow(key)), by = main_category]
quota[, n := floor(n_camouflage * share)]
setorder(quota, -share)
if (sum(quota$n) < n_camouflage)
  quota[seq_len(n_camouflage - sum(quota$n)), n := n + 1L]
quota[, share := NULL]
camo <- corpus[quota, on = "main_category"][, .SD[sample(.N, n[1])],
                                            by = main_category]
camo <- camo[, .(id_cite, site, main_category, underlying_assumption,
                 traditional_knowledge)]
cat(sprintf("camouflage assumptions: %d\n", nrow(camo)))

# ITEMS #######################################################################

both <- rbind(key[, .(id_cite, site, main_category, underlying_assumption,
                      traditional_knowledge)], camo)
both[, user_text := sprintf(
  "id_cite: %s\ntraditional_knowledge: %s\nunderlying_assumption: %s\nmain_category: %s",
  id_cite, traditional_knowledge, underlying_assumption, main_category)]
items <- both[rep(seq_len(nrow(both)), each = n_samples), .(id_cite, user_text)]
items[, sample := rep(seq_len(n_samples), times = nrow(both))]
items[, custom_id := sprintf("%s__s%d", id_cite, sample)]

prompt_path <- here("prompts", "efficiency_standard_prompt.txt")
system_prompt <- paste(readLines(prompt_path, warn = FALSE, encoding = "UTF-8"),
                       collapse = "\n")
stopifnot(grepl("json", system_prompt, ignore.case = TRUE))

schema <- list(type = "object",
               properties = list(id_cite = list(type = "string"),
                                 efficiency_legible = list(type = "boolean")),
               required = list("id_cite", "efficiency_legible"),
               additionalProperties = FALSE)

cat(sprintf("prompt %s | sha256 %s\n", basename(prompt_path),
            digest(system_prompt, algo = "sha256", serialize = FALSE)))
cat(sprintf("items %d x %d draws = %d requests per family, %d in total\n",
            nrow(both), n_samples, nrow(items), 3L * nrow(items)))

raw_file <- file.path(out, "arm_planted_raw_all.csv")
if (!run_enabled && !file.exists(raw_file)) {
  message("\nPLANTED_RUN_ENABLED = FALSE and no archived replies -> items built, APIs NOT called.")
  quit(save = "no")
}

# RUN #########################################################################

if (run_enabled && tolower(Sys.getenv("PLANTED_OPENAI_DIRECT", "true")) == "true") {
  cache_o <- file.path(out, "arm_planted_raw_openai.csv")
  if (!file.exists(cache_o)) {
    cat("OpenAI leg: synchronous endpoint\n")
    res_o <- run_openai_direct_fun(items, system_prompt, model = "gpt-5.4",
                                   max_completion_tokens = max_tokens)
    cat(sprintf("  succeeded %d/%d\n", sum(res_o$status == "succeeded"),
                nrow(res_o)))
    if (any(res_o$status == "succeeded")) fwrite(res_o, cache_o)
  }
}

raw_file <- file.path(out, "arm_planted_raw_all.csv")
if (file.exists(raw_file)) {
  cat("arm_planted_raw_all.csv present, reusing (delete to re-run)\n")
  raw <- as.data.table(utils::read.csv(raw_file, stringsAsFactors = FALSE,
                                       encoding = "UTF-8"))
} else {
  raw <- run_three_models_fun(items, system_prompt, schema, tag = "arm_planted",
                              out_dir = out, deepseek_model = deepseek_model,
                              max_tokens = max_tokens)
  fwrite(raw, raw_file)
}

parse_legible <- function(txt) {
  vapply(txt, function(t) {
    if (is.na(t) || t == "") return(NA)
    o <- tryCatch(fromJSON(t), error = function(e) NULL)
    v <- o[["efficiency_legible"]]
    if (is.null(v)) NA else as.logical(v)
  }, logical(1), USE.NAMES = FALSE)
}
raw[, legible := parse_legible(text)]
raw[, id_cite := sub("__s.*$", "", custom_id)]
print(raw[, .(requests = .N, http_ok = sum(status == "succeeded"),
              valid = sum(!is.na(legible)),
              items = uniqueN(id_cite[!is.na(legible)])), by = provider])

# VERDICTS ####################################################################

draws <- raw[!is.na(legible), .(share_irr = mean(!legible), n_draws = .N),
             by = .(provider, id_cite)]
verdicts <- rbindlist(lapply(names(vote_rules), function(v)
  draws[, .(vote = v, irreducible = share_irr >= vote_rules[[v]]),
        by = .(provider, id_cite, share_irr, n_draws)]))
verdicts <- merge(verdicts, key[, .(id_cite, planted_key, main_category)],
                  by = "id_cite", all.x = TRUE)
verdicts[, set := fifelse(is.na(planted_key), "camouflage", "planted")]
fwrite(verdicts, here("datasets", "output", "planted_items_verdicts.csv"))

# The two error rates, kept apart ----------------------------------------------

err <- verdicts[set == "planted",
                .(n = .N, wrong = sum(irreducible == planted_key),
                  rate = mean(irreducible == planted_key)),
                by = .(provider, vote, intended = fifelse(planted_key,
                                                          "reducible",
                                                          "irreducible"))]
setorder(err, intended, provider, vote)
fwrite(err, here("datasets", "output", "planted_items_errors.csv"))

cat("\n=== Error rate on planted items, by family and vote rule ===\n")
cat("(intended reducible, called irreducible: the error that inflates the claim)\n")
print(dcast(err[intended == "reducible"], provider ~ vote,
            value.var = "rate")[, lapply(.SD, function(z)
              if (is.numeric(z)) sprintf("%.0f%%", 100 * z) else z)])
cat("\n(intended irreducible, called reducible: runs against the finding)\n")
print(dcast(err[intended == "irreducible"], provider ~ vote,
            value.var = "rate")[, lapply(.SD, function(z)
              if (is.numeric(z)) sprintf("%.0f%%", 100 * z) else z)])

cat("\n=== Range over the specifications reported in the paper ===\n")
for (int in c("reducible", "irreducible"))
  cat(sprintf("intended %-12s error %2.0f%% (%2.0f-%2.0f%%) over %d family x vote cells\n",
              int, 100 * median(err[intended == int, rate]),
              100 * min(err[intended == int, rate]),
              100 * max(err[intended == int, rate]),
              nrow(err[intended == int])))

# Which items were missed ------------------------------------------------------

miss <- verdicts[set == "planted" & irreducible == planted_key,
                 .(missed_in = .N, families = uniqueN(provider)),
                 by = .(id_cite, main_category,
                        intended = fifelse(planted_key, "reducible",
                                           "irreducible"))]
setorder(miss, -missed_in)
cat("\n=== Planted items missed in at least one of the 9 family x vote cells ===\n")
if (nrow(miss)) print(miss) else cat("none\n")

# By knowledge domain ----------------------------------------------------------

cat("\n=== Error rate by knowledge domain (median over families and vote rules) ===\n")
dom <- verdicts[set == "planted", .(rate = mean(irreducible == planted_key)),
                by = .(main_category, intended = fifelse(planted_key, "reducible",
                                                         "irreducible"),
                       provider, vote)]
print(dcast(dom[, .(rate = median(rate)), by = .(main_category, intended)],
            main_category ~ intended, value.var = "rate")[, lapply(.SD,
              function(z) if (is.numeric(z)) sprintf("%.0f%%", 100 * z) else z)])

# POSITIVE CONTROL ############################################################

ctrl <- verdicts[set == "camouflage", .(share = mean(irreducible)),
                 by = .(provider, vote)]
cat("\n=== Camouflage assumptions: irreducible share (headline grid: 93% median) ===\n")
print(dcast(ctrl, provider ~ vote, value.var = "share")[, lapply(.SD,
  function(z) if (is.numeric(z)) sprintf("%.0f%%", 100 * z) else z)])
cat(sprintf("median across the 9 cells: %.0f%%\n", 100 * median(ctrl$share)))

# WHAT THE ERROR RATE IMPLIES FOR THE HEADLINE SHARE ##########################

err_red <- err[intended == "reducible", .(provider, vote, e = rate)]
grid_file <- here("datasets", "output", "sobol_headline_grid.csv")

if (file.exists(grid_file)) {
  grid <- fread(grid_file)
  corr <- merge(grid, err_red, by.x = c("model", "vote"),
                by.y = c("provider", "vote"))
  corr[, true_red := fifelse(e >= 1, NA_real_, (1 - share) / (1 - e))]
  corr[, corrected := 1 - true_red]

  cat("\n=== The headline grid, corrected for the one-sided error ===\n")
  cat(sprintf("as published : median %.0f%%, floor %.0f%% (%d cells)\n",
              100 * median(grid$share), 100 * min(grid$share), nrow(grid)))
  cat(sprintf("corrected    : median %.0f%%, floor %.0f%% (%d cells matched)\n",
              100 * median(corr$corrected, na.rm = TRUE),
              100 * min(corr$corrected, na.rm = TRUE), nrow(corr)))
  cat("\nby family and vote rule (median corrected share):\n")
  print(dcast(corr[, .(m = median(corrected, na.rm = TRUE)), by = .(model, vote)],
              model ~ vote, value.var = "m")[, lapply(.SD, function(z)
                if (is.numeric(z)) sprintf("%.0f%%", 100 * z) else z)])
  fwrite(corr, here("datasets", "output", "planted_items_corrected_grid.csv"))
} else {
  cat("\nsobol_headline_grid.csv not found; correction skipped.\n")
}

cat("\nWritten: planted_items_verdicts.csv, planted_items_errors.csv,",
    "planted_items_corrected_grid.csv\n")
