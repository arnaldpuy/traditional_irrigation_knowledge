# PAGE LOCATIONS FOR THE QUOTED EVIDENCE ######################################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table"))

out <- here("datasets", "output")
policy_dir <- here("datasets", "inputs", "policy_corpus")
lit_dir <- here("datasets", "inputs", "literature_corpus", "fulltexts")

norm_txt <- function(x) gsub(" +", " ", tolower(gsub("[^[:alnum:]]", " ", x)))
n_pages <- function(f) {
  info <- suppressWarnings(system(sprintf("pdfinfo %s 2>/dev/null", shQuote(f)),
                                  intern = TRUE))
  hit <- grep("^Pages:", info, value = TRUE)
  if (!length(hit)) NA_integer_ else as.integer(sub("^Pages:\\s*", "", hit))
}
page_text <- function(f, p) {
  paste(suppressWarnings(system(
    sprintf("pdftotext -f %d -l %d %s - 2>/dev/null", p, p, shQuote(f)),
    intern = TRUE)), collapse = " ")
}

# Page number for every verified policy quote ----------------------------------

quotes <- fread(file.path(out, "policy_quotes_manuscript.csv"))
setnames(quotes, "function", "fn")

locate <- function(doc, quote, max_pages = 3400L) {
  f <- file.path(policy_dir, paste0(doc, ".pdf"))
  if (!file.exists(f)) return(NA_integer_)
  np <- n_pages(f)
  if (is.na(np)) return(NA_integer_)
  needle <- norm_txt(substr(quote, 1, 60))
  for (p in seq_len(min(np, max_pages))) {
    if (grepl(needle, norm_txt(page_text(f, p)), fixed = TRUE)) return(p)
  }
  NA_integer_
}
pages_file <- file.path(out, "policy_quotes_pages.csv")
if (file.exists(pages_file)) {
  quotes <- fread(pages_file)
  cat("Reusing cached page locations; delete policy_quotes_pages.csv to rescan.\n")
} else {
  quotes[, page := vapply(seq_len(.N), function(i) locate(rec_id[i], verbatim_clean[i]),
                          integer(1))]
}
cat(sprintf("Policy quotes located: %d of %d\n", sum(!is.na(quotes$page)),
            nrow(quotes)))
print(quotes[, .(quotes = .N, located = sum(!is.na(page))), by = rec_id][order(-quotes)])
fwrite(quotes, file.path(out, "policy_quotes_pages.csv"))

# Verbatim sentences behind the two literature claims --------------------------

claims <- data.table(
  ref = c("carthy2024", "carthy2024", "kumar2022", "kumar2022"),
  file = c("10_3390_w16131762.pdf", "10_3390_w16131762.pdf",
           "10_1002_agj2_20957.pdf", "10_1002_agj2_20957.pdf"),
  claim = c("equity and governance", "capture by vested interests",
            "soil biodiversity", "aspirational framing"),
  pattern = c("equit|head[- ]end|tail[- ]end|governance|water distribution",
              "vested interest|elite|power|captur",
              "biodivers|microbial|soil health|organic manure",
              "aspiration|potential|could|may |propos"))

sentences <- rbindlist(lapply(seq_len(nrow(claims)), function(i) {
  f <- file.path(lit_dir, claims$file[i])
  if (!file.exists(f)) return(data.table(ref = claims$ref[i], claim = claims$claim[i],
                                         page = NA_integer_, sentence = "FILE NOT FOUND"))
  np <- n_pages(f)
  rbindlist(lapply(seq_len(np), function(p) {
    txt <- gsub("-\n|-\\s+", "", page_text(f, p))
    s <- trimws(unlist(strsplit(txt, "(?<=[.!?]) (?=[A-Z])", perl = TRUE)))
    s <- s[grepl(claims$pattern[i], s, ignore.case = TRUE) & nchar(s) > 60 &
             nchar(s) < 400]
    if (!length(s)) return(NULL)
    data.table(ref = claims$ref[i], claim = claims$claim[i], page = p, sentence = s)
  }))
}))
fwrite(sentences, file.path(out, "literature_claim_sentences.csv"))
cat(sprintf("\nCandidate sentences extracted: %d\n", nrow(sentences)))
print(sentences[, .(candidates = .N, pages = paste(sort(unique(page)), collapse = ", ")),
                by = .(ref, claim)])

# LaTeX tables for the Supplementary Materials --------------------------------

esc <- function(x) {
  x <- gsub("\\\\", "", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  gsub("~", "\\\\textasciitilde{}", x)
}
doc_lab <- c(
  A1_rd329_2002_plan_nacional_regadios = "Plan Nacional de Regadíos (RD 329/2002), MAPA 2002",
  A2_rd287_2006_plan_choque_regadios = "Plan de Choque de Modernización de Regadíos (RD 287/2006), MAPA/MMA 2006",
  A3_prtr_componente3 = "PRTR, Componente 3, Gobierno de España 2021",
  A4_perte_digitalizacion_agua_memoria = "PERTE de Digitalización del Ciclo del Agua, MITECO 2022",
  B1_plan_regadia_andalucia = "Plan REGADIA, Junta de Andalucía 2025",
  B2_pla_regadius_catalunya_memoria = "Pla de Regadius de Catalunya 2008--2020, Memòria, Generalitat de Catalunya 2010",
  B2b_pla_regadius_catalunya_annex2_proposta = "Pla de Regadius de Catalunya 2008--2020, Annex 2, Generalitat de Catalunya 2010",
  C1_pepac_espana_v5_1 = "Plan Estratégico de la PAC de España 2023--2027 (v5.1), MAPA/EC 2025",
  C2_reg_eu_2021_2115 = "Regulation (EU) 2021/2115, European Union 2021",
  D1_fao_id63_masscote = "FAO Irrigation and Drainage Paper 63 (MASSCOTE), FAO 2007",
  D2_burt_styles_1999_wr19_modern_water_control = "Burt \\& Styles, FAO Water Report 19, FAO/IPTRID/World Bank 1999")
fn_lab <- c(crop_production = "Crop production", social_cohesion = "Social cohesion",
            biodiversity = "Biodiversity", water_conservation = "Water conservation",
            risk_management = "Risk management",
            groundwater_recharge = "Groundwater recharge")

q <- fread(file.path(out, "policy_quotes_pages.csv"))
q[, doc_key := gsub("[^A-Za-z0-9]", "_", rec_id)]
q <- q[fn %in% names(fn_lab)]
q[, fn_ord := match(fn, names(fn_lab))]
setorder(q, fn_ord, rec_id)

rows <- q[, sprintf(
  "\\textbf{%s} & \\emph{``%s''} %s \\\\\n & \\footnotesize %s, %s, p.~%s \\\\\n\\addlinespace\n",
  esc(fn_lab[fn]), esc(verbatim_clean),
  fifelse(language == "en", "", sprintf("[%s]", esc(english_translation))),
  doc_lab[doc_key], esc(context),
  fifelse(is.na(page), "n/a", as.character(page)))]
writeLines(c("\\begin{longtable}{p{2.2cm} >{\\raggedright\\arraybackslash}p{11.6cm}}",
             "\\caption{Verified quotations in which the policy documents promise each function. Every passage is an exact substring of the archived document text, read in context by the authors (Text~\\ref{sec:SM_promises}). Quotations in Spanish and Catalan are followed by an English translation in square brackets.}",
             "\\label{tab:SM_policy_quotes}\\\\",
             "\\toprule", "Function & Verified quotation and source \\\\", "\\midrule",
             "\\endfirsthead",
             "\\multicolumn{2}{@{}l}{\\small\\itshape Table~\\ref{tab:SM_policy_quotes} (continued)}\\\\",
             "\\toprule", "Function & Verified quotation and source \\\\", "\\midrule",
             "\\endhead", rows, "\\bottomrule", "\\end{longtable}"),
           here("manuscript", "sm_policy_quotes.tex"))
cat(sprintf("\nWrote manuscript/sm_policy_quotes.tex (%d quotations)\n", nrow(q)))
print(q[, .N, by = fn])
