# DOES THE VERDICT DEPEND ON THE ANALYST'S ENGLISH? ###########################

# GATED: set LANGUAGE_RUN_ENABLED=true to call the APIs.

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "readxl", "jsonlite", "digest"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# CONFIGURATION ###############################################################

run_enabled <- tolower(Sys.getenv("LANGUAGE_RUN_ENABLED", "false")) == "true"
n_samples <- as.integer(Sys.getenv("LANGUAGE_N", "5"))
n_sample <- as.integer(Sys.getenv("LANGUAGE_SAMPLE", "200"))
out <- here("datasets", "output")
arm_dir <- file.path(out, "arm"); chk_dir <- file.path(out, "checkpoint")
set.seed(20260730)

vote_rules <- c(`any draw` = 0.2, majority = 0.6, unanimous = 1.0)
max_tokens <- 3000L

# SPLITTING THE RECORDED STATEMENT ############################################

corpus <- as.data.table(read_xlsx(here("datasets", "inputs",
                                      "assumption_list_updated.xlsx")))
setnames(corpus, tolower(names(corpus)))

split_statement <- function(x) {
  pos <- gregexpr('[”“"]', x)[[1]]
  if (length(pos) < 2 || pos[1] == -1) return(c(NA_character_, NA_character_))
  original <- substr(x, pos[1] + 1L, pos[2] - 1L)
  english <- substr(x, pos[2] + 1L, nchar(x))
  english <- sub('^[[:space:][:punct:]]*(Saying\\s*-\\s*)?[”“"]?', "", english)
  c(trimws(original), trimws(english))
}
sp <- t(vapply(corpus$traditional_knowledge, split_statement, character(2),
               USE.NAMES = FALSE))
corpus[, c("tk_original", "tk_english") := list(sp[, 1], sp[, 2])]
bad <- corpus[is.na(tk_original) | nchar(tk_original) < 15 |
                is.na(tk_english) | nchar(tk_english) < 15]
cat(sprintf("splittable: %d of %d (dropped: %s)\n", nrow(corpus) - nrow(bad),
            nrow(corpus), paste(head(bad$id_cite, 10), collapse = ", ")))
ok <- corpus[!id_cite %in% bad$id_cite]

# THE SAMPLE ##################################################################

per_cell <- ok[, .N, by = .(site, main_category)]
per_cell[, take := pmax(1L, round(n_sample * N / sum(N)))]
src <- ok[per_cell, on = .(site, main_category)][
  , .SD[sample(.N, min(take[1], .N))], by = .(site, main_category)]
src <- src[sample(.N, min(n_sample, .N))]
cat(sprintf("sample: %d assumptions across %d communities and %d domains\n",
            nrow(src), uniqueN(src$site), uniqueN(src$main_category)))

# ITEMS #######################################################################

mk <- function(d, cond) {
  txt <- switch(cond,
    original = sprintf("id_cite: %s\ntraditional_knowledge: %s\nmain_category: %s",
                       d$id_cite, d$tk_original, d$main_category),
    english = sprintf(paste0("id_cite: %s\ntraditional_knowledge: %s\n",
                             "underlying_assumption: %s\nmain_category: %s"),
                      d$id_cite, d$tk_english, d$underlying_assumption,
                      d$main_category))
  data.table(tag = sprintf("%s-%s", d$id_cite, cond), user_text = txt)
}
both <- rbind(mk(src, "original"), mk(src, "english"))
items <- both[rep(seq_len(nrow(both)), each = n_samples)]
items[, sample := rep(seq_len(n_samples), times = nrow(both))]
items[, custom_id := sprintf("%s__s%d", tag, sample)]
items[, tag := NULL]

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
cat("condition A is read from the cached standard-wording arms, not re-run\n")

if (!run_enabled) {
  message("\nLANGUAGE_RUN_ENABLED = FALSE -> items built, APIs NOT called.")
  quit(save = "no")
}

# RUN #########################################################################

if (tolower(Sys.getenv("LANGUAGE_CLAUDE_DIRECT", "true")) == "true") {
  cache_a <- file.path(arm_dir, "arm_language_raw_anthropic.csv")
  if (!file.exists(cache_a)) {
    cat("Claude leg: synchronous endpoint\n")
    res_a <- run_claude_direct_fun(items, system_prompt, schema,
                                   max_tokens = 400L)
    cat(sprintf("  succeeded %d/%d\n", sum(res_a$status == "succeeded"), nrow(res_a)))
    if (any(res_a$status == "succeeded")) fwrite(res_a, cache_a)
  }
}

if (tolower(Sys.getenv("LANGUAGE_OPENAI_DIRECT", "true")) == "true") {
  cache_o <- file.path(arm_dir, "arm_language_raw_openai.csv")
  if (!file.exists(cache_o)) {
    cat("OpenAI leg: synchronous endpoint\n")
    res_o <- run_openai_direct_fun(items, system_prompt, model = "gpt-5.4",
                                   max_completion_tokens = max_tokens)
    cat(sprintf("  succeeded %d/%d\n", sum(res_o$status == "succeeded"), nrow(res_o)))
    if (any(res_o$status == "succeeded")) fwrite(res_o, cache_o)
  }
}

raw_file <- file.path(arm_dir, "arm_language_raw_all.csv")
if (file.exists(raw_file)) {
  raw <- as.data.table(utils::read.csv(raw_file, stringsAsFactors = FALSE,
                                       encoding = "UTF-8"))
} else {
  raw <- run_three_models_fun(items, system_prompt, schema, tag = "arm_language",
                              out_dir = arm_dir, deepseek_model = "deepseek-v4-pro",
                              max_tokens = max_tokens)
  fwrite(raw, raw_file)
}

parse_json_field <- function(txt, field) {
  vapply(txt, function(t) {
    if (is.na(t) || t == "") return(NA_character_)
    o <- tryCatch(fromJSON(t), error = function(e) NULL)
    v <- o[[field]]
    if (is.null(v)) NA_character_ else as.character(v)
  }, character(1), USE.NAMES = FALSE)
}
raw[, legible := as.logical(parse_json_field(text, "efficiency_legible"))]
raw[, c("id_cite", "condition") := tstrsplit(sub("__s.*$", "", custom_id), "-",
                                             fixed = TRUE)]
print(raw[, .(requests = .N, valid = sum(!is.na(legible)),
              items = uniqueN(id_cite[!is.na(legible)])), by = .(provider, condition)])

# Condition A, from the cached standard-wording arms ---------------------------

read_arm <- function(f) as.data.table(utils::read.csv(f, stringsAsFactors = FALSE,
                                                      encoding = "UTF-8"))
a <- read_arm(file.path(chk_dir, "chk_standard_raw_all.csv"))
a[, legible := as.logical(parse_json_field(text, "efficiency_legible"))]
a <- a[!is.na(legible), .(provider, id_cite = sub("__s.*$", "", custom_id), legible)]
a4 <- read_arm(file.path(out, "v4_ruleonly_standard_deepseek_raw_all.csv"))
a4[, legible := as.logical(parse_json_field(text, "efficiency_legible"))]
a4 <- a4[!is.na(legible), .(provider = "deepseek",
                            id_cite = sub("__s.*$", "", custom_id), legible)]
a <- rbind(a[provider != "deepseek"], a4)[id_cite %in% src$id_cite]
a[, condition := "as run"]

all_draws <- rbind(
  raw[!is.na(legible), .(provider, id_cite, condition, legible)],
  a[, .(provider, id_cite, condition, legible)])
dr <- all_draws[, .(s_irr = mean(!legible)), by = .(provider, id_cite, condition)]
v <- rbindlist(lapply(names(vote_rules), function(x)
  dr[, .(vote = x, irreducible = s_irr >= vote_rules[[x]]),
     by = .(provider, id_cite, condition)]))
fwrite(v, file.path(out, "language_conditions_verdicts.csv"))

cat("\n=== Irreducible share by condition ===\n")
sh <- v[, .(share = mean(irreducible)), by = .(provider, vote, condition)]
print(dcast(sh, provider + vote ~ condition, value.var = "share")[
  , lapply(.SD, function(z) if (is.numeric(z)) sprintf("%.0f%%", 100 * z) else z)])
cat("\nmedian over the 9 family x vote cells:\n")
print(sh[, .(median = sprintf("%.0f%%", 100 * median(share)),
             range = sprintf("%.0f-%.0f%%", 100 * min(share), 100 * max(share))),
         by = condition])

cat("\n=== Assumptions whose verdict changes when the English is removed ===\n")
w <- dcast(v, provider + vote + id_cite ~ condition, value.var = "irreducible")
setnames(w, "as run", "as_run")
for (cond in c("original", "english")) {
  z <- w[!is.na(as_run) & !is.na(get(cond))]
  cat(sprintf("%-9s vs as run: %.0f%% differ (median over cells %.0f%%, range %.0f-%.0f%%)\n",
              cond, 100 * mean(z$as_run != z[[cond]]),
              100 * median(z[, mean(as_run != get(cond)), by = .(provider, vote)]$V1),
              100 * min(z[, mean(as_run != get(cond)), by = .(provider, vote)]$V1),
              100 * max(z[, mean(as_run != get(cond)), by = .(provider, vote)]$V1)))
}

cat("\nWritten: language_conditions_verdicts.csv\n")
