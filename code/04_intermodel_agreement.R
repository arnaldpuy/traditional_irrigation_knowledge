# INTER-MODEL AGREEMENT #######################################################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# Configuration ---------------------------------------------------------------

prompt_choice <- Sys.getenv("CODING_PROMPT", "subcategory")  # subcategory | analytical
providers <- c("anthropic", "deepseek")
out <- here("datasets", "output")

vote_fields <- if (prompt_choice == "analytical") {
  c("epistemic_type", "functions", "efficiency_legible", "sensory_channels",
    "n_env_variables", "eroded_by", "corroboration", "measured_variable",
    "time_horizon", "inferential_form", "temporal_framing", "knowledge_mode",
    "is_saying", "is_value_statement", "practice")
} else {
  c("primary_subcategory", "secondary_subcategory", "cross_subcategory",
    "cross_main_category", "main_category_flag")
}

# Reprocess each provider's raw responses -> majority codes --------------------

majority <- list()
for (prov in providers) {
  raw <- as.data.table(read.csv(
    file.path(out, sprintf("coding_results_raw_%s_%s.csv", prompt_choice, prov)),
    stringsAsFactors = FALSE, encoding = "UTF-8"))
  long <- rbindlist(lapply(seq_len(nrow(raw)),
                           function(i) flatten_coding_record_fun(raw$text[i],
                                                                 prompt_choice)),
                    fill = TRUE)
  coded <- majority_vote_fun(long, id_col = "id_cite", fields = vote_fields)
  conf <- long[, .(model_confidence = mean(as.numeric(confidence), na.rm = TRUE)),
               by = id_cite]
  coded <- conf[coded, on = "id_cite"]
  fwrite(coded, file.path(out, sprintf("coding_majority_%s_%s.csv",
                                       prompt_choice, prov)))
  cat(sprintf("%-9s: %d assumptions | mean self-consistency %.3f\n",
              prov, nrow(coded), coded[, mean(mean_agreement, na.rm = TRUE)]))
  majority[[prov]] <- coded
}

# Inter-model agreement, field by field ---------------------------------------

a <- majority[["anthropic"]]
d <- majority[["deepseek"]]
setkey(a, id_cite); setkey(d, id_cite)
common <- intersect(a$id_cite, d$id_cite)
a <- a[.(common)]; d <- d[.(common)]

agree_vec <- function(x, y) {
  both_na <- is.na(x) & is.na(y)
  both_val <- !is.na(x) & !is.na(y) & x == y
  as.numeric(both_na | both_val)
}
agr <- rbindlist(lapply(vote_fields, function(f) {
  v <- agree_vec(a[[f]], d[[f]])
  data.table(field = f, n = length(v), agreement = mean(v))
}))
overall <- data.table(field = "ALL_FIELDS", n = nrow(a) * length(vote_fields),
                      agreement = mean(unlist(lapply(vote_fields,
                        function(f) agree_vec(a[[f]], d[[f]])))))
agr <- rbind(agr, overall)
fwrite(agr, file.path(out, sprintf("coding_intermodel_agreement_%s.csv",
                                   prompt_choice)))

cat(sprintf("\nInter-model agreement (Claude vs DeepSeek), n = %d assumptions:\n",
            length(common)))
print(agr[, .(field, agreement = round(agreement, 3))])
