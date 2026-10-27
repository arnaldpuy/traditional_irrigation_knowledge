# DEEPSEEK-V4 RULE-ONLY ARMS (RESULTS 1 VERSION ALIGNMENT) ####################

# ---------------------------------------------------------------------------
# GATED behind DEEPSEEK_V4_RUN_ENABLED (FALSE by default). While FALSE the
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

run_enabled <- as.logical(Sys.getenv("DEEPSEEK_V4_RUN_ENABLED", "FALSE"))
n_samples   <- as.integer(Sys.getenv("CODING_N", "5"))
model       <- "deepseek-v4-pro"
max_tokens  <- 3000L
temperature <- 0.7
concurrency <- as.integer(Sys.getenv("DEEPSEEK_CONCURRENCY", "4"))
out <- here("datasets", "output")

wordings <- c(standard = "efficiency_standard_prompt.txt",
              short    = "efficiency_short_prompt.txt",
              strict   = "efficiency_strict_prompt.txt")

# Read the input list ----------------------------------------------------------

inp <- as.data.table(read_xlsx(here("datasets", "inputs",
                                    "assumption_list_updated.xlsx")))
setnames(inp, tolower(names(inp)))
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

cat(sprintf("deepseek-v4 rule-only arms: %d wordings x %d requests = %d calls\n",
            length(wordings), nrow(items), length(wordings) * nrow(items)))

if (!run_enabled) {
  message("DEEPSEEK_V4_RUN_ENABLED = FALSE -> nothing called. Set the flag to run.")
  quit(save = "no")
}

# Run each wording -------------------------------------------------------------

for (w in names(wordings)) {
  tag <- sprintf("v4_ruleonly_%s_deepseek", w)
  raw_file <- file.path(out, sprintf("%s_raw_all.csv", tag))
  if (file.exists(raw_file)) {
    cat(sprintf("[%s] already present, skipping (delete to re-run)\n", tag))
    next
  }

  prompt_path <- here("prompts", wordings[[w]])
  raw_prompt <- readLines(prompt_path, warn = FALSE, encoding = "UTF-8")
  start <- grep("^SYSTEM ROLE", raw_prompt)[1]
  idx <- if (is.na(start)) seq_along(raw_prompt) else start:length(raw_prompt)
  system_prompt <- paste(raw_prompt[idx], collapse = "\n")
  stopifnot(grepl("json", system_prompt, ignore.case = TRUE))
  cat(sprintf("\n[%s] prompt %s | sha256 %s\n", tag, wordings[[w]],
              substr(digest(system_prompt, algo = "sha256",
                            serialize = FALSE), 1, 12)))

  res <- run_deepseek_batch_fun(items, system_prompt, model = model,
                                temperature = temperature,
                                max_tokens = max_tokens,
                                concurrency = concurrency)
  fwrite(res, raw_file)

  ok <- vapply(res$text, function(x)
    !is.null(tryCatch(fromJSON(x), error = function(e) NULL)),
    logical(1), USE.NAMES = FALSE)
  n_assumptions <- uniqueN(sub("__s.*$", "", res$custom_id[ok]))
  cat(sprintf("[%s] http-succeeded %d/%d | valid JSON %d (%.1f%%) | assumptions %d/475\n",
              tag, sum(res$status == "succeeded"), nrow(res), sum(ok),
              100 * mean(ok), n_assumptions))
  if (n_assumptions < 450L)
    warning("Arm ", tag, " covered only ", n_assumptions,
            " assumptions; inspect before using.")

  legible <- vapply(res$text[ok], function(x) {
    o <- tryCatch(fromJSON(x), error = function(e) NULL)
    v <- o[["efficiency_legible"]]
    if (is.null(v)) NA else as.logical(v)
  }, logical(1), USE.NAMES = FALSE)
  share <- mean(tapply(!legible, sub("__s.*$", "", res$custom_id[ok]),
                       function(z) mean(z) >= 0.5), na.rm = TRUE)
  cat(sprintf("[%s] irreducible share (majority vote): %.3f\n", tag, share))
}

cat("\nDone. These arms are the deepseek-v4 legs of the Results 1 grid;\n")
cat("code/39 must be pointed at them to replace the retired deepseek-chat legs.\n")
