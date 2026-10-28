# DOES THE VERDICT SURVIVE A REWORDING? #######################################

# GATED: set PARAPHRASE_RUN_ENABLED=true to call the APIs.

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "readxl", "writexl", "jsonlite",
                          "digest"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# CONFIGURATION ###############################################################

run_enabled <- tolower(Sys.getenv("PARAPHRASE_RUN_ENABLED", "false")) == "true"
n_samples <- as.integer(Sys.getenv("PARAPHRASE_N", "5"))
n_random <- as.integer(Sys.getenv("PARAPHRASE_RANDOM", "60"))
n_hard <- as.integer(Sys.getenv("PARAPHRASE_HARD", "60"))
out <- here("datasets", "output")
arm_dir <- file.path(out, "arm"); chk_dir <- file.path(out, "checkpoint")
set.seed(20260730)

vote_rules <- c(`any draw` = 0.2, majority = 0.6, unanimous = 1.0)
max_tokens <- 3000L
rewriter_model <- "claude-opus-4-8"

# THE SAMPLE ##################################################################

corpus <- as.data.table(read_xlsx(here("datasets", "inputs",
                                      "assumption_list_updated.xlsx")))
setnames(corpus, tolower(names(corpus)))
dt <- load_coded_corpus_fun()
corpus <- merge(corpus, dt[, .(id_cite, domain = main_category)], by = "id_cite")

# Consistency of the existing readings, used only to pick the hard cases -------

read_arm <- function(f) as.data.table(utils::read.csv(f, stringsAsFactors = FALSE,
                                                      encoding = "UTF-8"))
parse_json_field <- function(txt, field) {
  vapply(txt, function(t) {
    if (is.na(t) || t == "") return(NA_character_)
    o <- tryCatch(fromJSON(t), error = function(e) NULL)
    v <- o[[field]]
    if (is.null(v)) NA_character_ else as.character(v)
  }, character(1), USE.NAMES = FALSE)
}
bin_files <- c(standard = file.path(chk_dir, "chk_standard_raw_all.csv"),
               short = file.path(arm_dir, "arm_short_raw_all.csv"),
               strict = file.path(arm_dir, "arm_strict_raw_all.csv"))
bin <- rbindlist(lapply(names(bin_files), function(nm) {
  d <- read_arm(bin_files[[nm]])
  d[, legible := as.logical(parse_json_field(text, "efficiency_legible"))]
  d[, .(prompt = nm, provider, id_cite = sub("__s.*$", "", custom_id), legible)]
}))[!is.na(legible)]
consistency <- bin[, .(s_irr = mean(!legible)), by = .(prompt, provider, id_cite)][
  , .(spread = sd(s_irr), mean_irr = mean(s_irr)), by = id_cite]

# 60 hard cases first, then 60 stratified at random from what is left ----------

setorder(consistency, -spread)
hard <- consistency[seq_len(n_hard), id_cite]
rest <- corpus[!id_cite %in% hard]
per_cell <- rest[, .N, by = .(site, domain)]
per_cell[, take := pmax(1L, round(n_random * N / sum(N)))]
rand <- rest[per_cell, on = .(site, domain)][, .SD[sample(.N, min(take[1], .N))],
                                             by = .(site, domain)]$id_cite
rand <- head(rand, n_random)
sample_ids <- unique(c(hard, rand))
cat(sprintf("sample: %d assumptions (%d hard, %d stratified random)\n",
            length(sample_ids), length(hard), length(rand)))

src <- corpus[id_cite %in% sample_ids]

# REWRITING ###################################################################

rewrite_prompt <- paste(
  "You rewrite statements about traditional irrigation so that they say exactly",
  "the same thing in different words.",
  "",
  "RULES:",
  "- Preserve the meaning exactly. Add nothing. Remove nothing. Do not simplify,",
  "  generalise, explain or interpret.",
  "- EVERY part must be reworded, including the statement in Spanish or Catalan.",
  "  Returning that statement unchanged is a failed response. Reword it in the",
  "  SAME language it is written in: a Spanish statement stays Spanish, a Catalan",
  "  statement stays Catalan.",
  "- Change the wording, not the content: different verbs, different word order,",
  "  different construction, similar length and register.",
  "- Keep every quantity, unit, plant, animal, tool and place name unchanged.",
  "",
  "INPUT: id_cite, traditional_knowledge (a statement in its original language",
  "followed by an English translation) and underlying_assumption (an English",
  "proposition).",
  "",
  "OUTPUT: return ONLY a single valid json object, no prose and no code fences:",
  '{"id_cite": "<echo>", "traditional_knowledge": "<rewritten, same structure:',
  'original language in quotation marks, then the English translation>",',
  '"underlying_assumption": "<rewritten English proposition>"}', sep = "\n")

rewrite_schema <- list(type = "object",
  properties = list(id_cite = list(type = "string"),
                    traditional_knowledge = list(type = "string"),
                    underlying_assumption = list(type = "string")),
  required = list("id_cite", "traditional_knowledge", "underlying_assumption"),
  additionalProperties = FALSE)

para_file <- file.path(out, "paraphrase_pairs.csv")

if (!file.exists(para_file)) {
  if (!run_enabled) {
    message("PARAPHRASE_RUN_ENABLED = FALSE -> sample built, rewriting NOT called.")
    quit(save = "no")
  }
  ritems <- src[, .(custom_id = id_cite, user_text = sprintf(
    "id_cite: %s\ntraditional_knowledge: %s\nunderlying_assumption: %s",
    id_cite, traditional_knowledge, underlying_assumption))]

  first_quoted <- function(x) {
    pos <- gregexpr('[”“"]', x)[[1]]
    if (length(pos) < 2 || pos[1] == -1) return(NA_character_)
    trimws(substr(x, pos[1] + 1L, pos[2] - 1L))
  }
  parse_rw <- function(rw) rbindlist(lapply(seq_len(nrow(rw)), function(i) {
    o <- tryCatch(fromJSON(rw$text[i]), error = function(e) NULL)
    if (is.null(o)) return(NULL)
    data.table(id_cite = as.character(o$id_cite)[1],
               traditional_knowledge = as.character(o$traditional_knowledge)[1],
               underlying_assumption = as.character(o$underlying_assumption)[1])
  }))

  cat(sprintf("rewriting %d assumptions with %s\n", nrow(ritems), rewriter_model))
  para <- parse_rw(run_claude_direct_fun(ritems, rewrite_prompt, rewrite_schema,
                                         model = rewriter_model, max_tokens = 1500L))

  for (pass in 2:4) {
    chk <- merge(para[, .(id_cite, new_tk = traditional_knowledge)],
                 src[, .(id_cite, old_tk = traditional_knowledge)], by = "id_cite")
    chk[, unchanged := vapply(new_tk, first_quoted, character(1), USE.NAMES = FALSE) ==
          vapply(old_tk, first_quoted, character(1), USE.NAMES = FALSE)]
    stuck <- chk[unchanged == TRUE | is.na(unchanged), id_cite]
    cat(sprintf("  pass %d: original-language segment still unchanged for %d\n",
                pass - 1L, length(stuck)))
    if (!length(stuck)) break
    redo <- parse_rw(run_claude_direct_fun(
      ritems[custom_id %in% stuck], paste(rewrite_prompt,
        "", "Your previous answer left the Spanish or Catalan statement word for",
        "word as it was. That is a failed response. Rewrite it with different",
        "verbs, a different construction and a different word order, in the same",
        "language, saying exactly the same thing.", sep = "\n"),
      rewrite_schema, model = rewriter_model, max_tokens = 1500L))
    para <- rbind(para[!id_cite %in% stuck], redo)
  }

  para <- merge(para, src[, .(id_cite, site, main_category)], by = "id_cite")
  fwrite(para, para_file)
  cat(sprintf("rewritten: %d of %d\n", nrow(para), nrow(src)))
} else {
  para <- fread(para_file, encoding = "UTF-8")
  cat(sprintf("paraphrase_pairs.csv present: %d rewritten assumptions\n", nrow(para)))
}

spot <- merge(src[, .(id_cite, original_tk = traditional_knowledge,
                      original_ua = underlying_assumption)],
              para[, .(id_cite, rewritten_tk = traditional_knowledge,
                       rewritten_ua = underlying_assumption)], by = "id_cite")
spot <- spot[sample(.N, min(20L, .N))]
spot[, c("meaning_preserved", "checked_by") := list(NA, NA_character_)]
spot_file <- file.path(out, "paraphrase_spotcheck.xlsx")

if (file.exists(spot_file)) {
  prev <- as.data.table(read_xlsx(spot_file))
  if (sum(!is.na(prev$meaning_preserved)) > 0) {
    cat(sprintf("paraphrase_spotcheck.xlsx already carries %d judgments, left untouched\n",
                sum(!is.na(prev$meaning_preserved))))
  } else {
    write_xlsx(spot, spot_file)
    cat("rewrote paraphrase_spotcheck.xlsx (20 pairs, none judged yet)\n")
  }
} else {
  write_xlsx(spot, spot_file)
  cat("wrote paraphrase_spotcheck.xlsx (20 pairs, meaning_preserved to be filled)\n")
}

# ITEMS #######################################################################

both <- rbind(
  src[, .(id_cite, version = "original", traditional_knowledge,
          underlying_assumption, main_category)],
  para[, .(id_cite, version = "rewritten", traditional_knowledge,
           underlying_assumption, main_category)])
both[, tag := sprintf("%s-%s", id_cite, version)]
both[, user_text := sprintf(
  "id_cite: %s\ntraditional_knowledge: %s\nunderlying_assumption: %s\nmain_category: %s",
  id_cite, traditional_knowledge, underlying_assumption, main_category)]

items <- both[rep(seq_len(nrow(both)), each = n_samples), .(tag, user_text)]
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

raw_file <- file.path(arm_dir, "arm_paraphrase_raw_all.csv")
if (!run_enabled && !file.exists(raw_file)) {
  message("\nPARAPHRASE_RUN_ENABLED = FALSE and no archived replies -> items built, classification NOT called.")
  quit(save = "no")
}

# RUN #########################################################################

if (run_enabled && tolower(Sys.getenv("PARAPHRASE_CLAUDE_DIRECT", "true")) == "true") {
  cache_a <- file.path(arm_dir, "arm_paraphrase_raw_anthropic.csv")
  if (!file.exists(cache_a)) {
    cat("Claude leg: synchronous endpoint\n")
    res_a <- run_claude_direct_fun(items, system_prompt, schema,
                                   max_tokens = 400L)
    cat(sprintf("  succeeded %d/%d\n", sum(res_a$status == "succeeded"), nrow(res_a)))
    if (any(res_a$status == "succeeded")) fwrite(res_a, cache_a)
  }
}

if (run_enabled && tolower(Sys.getenv("PARAPHRASE_OPENAI_DIRECT", "true")) == "true") {
  cache_o <- file.path(arm_dir, "arm_paraphrase_raw_openai.csv")
  if (!file.exists(cache_o)) {
    cat("OpenAI leg: synchronous endpoint\n")
    res_o <- run_openai_direct_fun(items, system_prompt, model = "gpt-5.4",
                                   max_completion_tokens = max_tokens)
    cat(sprintf("  succeeded %d/%d\n", sum(res_o$status == "succeeded"), nrow(res_o)))
    if (any(res_o$status == "succeeded")) fwrite(res_o, cache_o)
  }
}

raw_file <- file.path(arm_dir, "arm_paraphrase_raw_all.csv")
if (file.exists(raw_file)) {
  raw <- as.data.table(utils::read.csv(raw_file, stringsAsFactors = FALSE,
                                       encoding = "UTF-8"))
} else {
  raw <- run_three_models_fun(items, system_prompt, schema,
                              tag = "arm_paraphrase", out_dir = arm_dir,
                              deepseek_model = "deepseek-v4-pro",
                              max_tokens = max_tokens)
  fwrite(raw, raw_file)
}

raw[, legible := as.logical(parse_json_field(text, "efficiency_legible"))]
raw[, c("id_cite", "version") := tstrsplit(sub("__s.*$", "", custom_id), "-",
                                           fixed = TRUE)]
print(raw[, .(requests = .N, valid = sum(!is.na(legible)),
              items = uniqueN(id_cite[!is.na(legible)])), by = provider])

# FLIPS #######################################################################

dr <- raw[!is.na(legible), .(s_irr = mean(!legible)), by = .(provider, id_cite, version)]
v <- rbindlist(lapply(names(vote_rules), function(x)
  dr[, .(vote = x, irreducible = s_irr >= vote_rules[[x]]),
     by = .(provider, id_cite, version)]))
w <- dcast(v, provider + vote + id_cite ~ version, value.var = "irreducible")
w <- w[!is.na(original) & !is.na(rewritten)]
w[, flip := original != rewritten]
fwrite(w, file.path(out, "paraphrase_flips.csv"))

cat("\n=== Verdicts that change under a meaning-preserving rewording ===\n")
print(dcast(w[, .(r = sprintf("%.0f%% (%d/%d)", 100 * mean(flip), sum(flip), .N)),
              by = .(provider, vote)], provider ~ vote, value.var = "r"))
cat(sprintf("\nover the 9 family x vote cells: median %.0f%%, range %.0f-%.0f%%\n",
            100 * median(w[, mean(flip), by = .(provider, vote)]$V1),
            100 * min(w[, mean(flip), by = .(provider, vote)]$V1),
            100 * max(w[, mean(flip), by = .(provider, vote)]$V1)))

cat("\ndirection of the flips (of those that change):\n")
print(w[flip == TRUE, .(to_irreducible = sum(rewritten),
                        to_reducible = sum(!rewritten)), by = provider])

cat("\n=== Irreducible share, original versus rewritten ===\n")
sh <- v[, .(share = mean(irreducible)), by = .(provider, vote, version)]
print(dcast(sh, provider + vote ~ version, value.var = "share")[
  , .(provider, vote, original = sprintf("%.0f%%", 100 * original),
      rewritten = sprintf("%.0f%%", 100 * rewritten),
      shift = sprintf("%+.0f", 100 * (rewritten - original)))])

cat("\nrewriting model was", rewriter_model,
    "-> compare its flip rate with the other two above\n")
cat("\nWritten: paraphrase_pairs.csv, paraphrase_flips.csv, paraphrase_spotcheck.xlsx\n")
