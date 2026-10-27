# POLICY PROMISES ON IRRIGATION PASSAGES ONLY #################################

# GATED behind POLICY_RECODE_ENABLED (two models, five draws, three documents).

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "httr2", "jsonlite"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

# CONFIGURATION ###############################################################

run_enabled <- tolower(Sys.getenv("POLICY_RECODE_ENABLED", "false")) == "true"
n_draws <- as.integer(Sys.getenv("POLICY_N_DRAWS", "5"))
out_dir <- here("datasets", "output")
pol_dir <- here("datasets", "inputs", "policy_corpus")
lin_dir <- file.path(pol_dir, "linearized")
max_chars <- 200000L
general_docs <- c("C1_pepac_espana_v5-1", "C2_reg_eu_2021-2115",
                  "A4_perte_digitalizacion_agua_memoria")
catalan_parts <- c("B2_pla_regadius_catalunya_memoria",
                   "B2b_pla_regadius_catalunya_annex2_proposta")
catalan_id <- "B2_pla_regadius_catalunya"
irr_pattern <- "\\b(regad[ií]os?|riegos?|regantes?|regadius?|irrigaci[oó]n|irrigation|irrigated|regar|regatge)\\b"

# IRRIGATION PASSAGES #########################################################

read_lin <- function(doc) paste(readLines(file.path(lin_dir, paste0(doc, ".txt")),
                                          warn = FALSE, encoding = "UTF-8"),
                                collapse = "\n")
irrigation_text <- function(doc) {
  lines <- trimws(unlist(strsplit(read_lin(doc), "\n")))
  lines <- lines[nchar(lines) > 0]
  hit <- grepl(irr_pattern, lines, ignore.case = TRUE, perl = TRUE)
  keep <- Reduce(`|`, lapply(-3:3, function(k) shift(hit, k, fill = FALSE)))
  block <- cumsum(keep & !shift(keep, 1, fill = FALSE))
  blocks <- vapply(split(lines[keep], block[keep]), paste, character(1), collapse = " ")
  txt <- paste(blocks, collapse = "\n\n")
  if (nchar(txt) > max_chars) {
    h <- as.integer(max_chars * 0.6)
    txt <- paste0(substr(txt, 1, h), "\n\n[... middle omitted ...]\n\n",
                  substr(txt, nchar(txt) - (max_chars - h) + 1, nchar(txt)))
  }
  data.table(doc_id = doc, n_paras = length(lines), n_kept = sum(hit),
             n_blocks = length(blocks), n_chars = nchar(txt), body = txt)
}
restricted <- rbindlist(lapply(general_docs, irrigation_text))
print(restricted[, .(doc_id, n_paras, n_kept, n_blocks, n_chars)])
fwrite(restricted[, .(doc_id, n_paras, n_kept, n_blocks, n_chars)],
       file.path(out_dir, "policy_recode_text.csv"))

# INSTRUMENT ##################################################################

scope_note <- paste(
  "SCOPE OF THIS TEXT",
  "The text below is NOT the whole document: it is the set of passages of a",
  "broader agricultural or water policy that mention irrigation. Code a function",
  "ONLY where the document attributes it to irrigation modernisation or to",
  "irrigation infrastructure and technologies. Objectives the document states",
  "for agriculture, rural areas or the water cycle in general do not count.",
  sep = "\n")
inst <- policy_promise_instrument_fun(scope_note)
restricted[, user_text := sprintf("DOCUMENT: %s (irrigation passages)\n\nTEXT:\n%s",
                                  doc_id, body)]
items <- restricted[, .(custom_id = doc_id, user_text)]

# DUAL-MODEL, N-DRAW RECODE ###################################################

cl_raw_file <- file.path(out_dir, "policy_recode_raw_claude.rds")
ds_raw_file <- file.path(out_dir, "policy_recode_raw_deepseek.rds")

if (!run_enabled && !(file.exists(cl_raw_file) && file.exists(ds_raw_file))) {
  stop("GATED: POLICY_RECODE_ENABLED != true and no cached replies - nothing to do.")
}

fire_draws <- function(cache_file, fire_one, label) {
  cache <- if (file.exists(cache_file)) as.data.table(readRDS(cache_file)) else
    data.table(custom_id = character(), draw = integer(),
               status = character(), text = character())
  if (run_enabled) {
    for (dr in seq_len(n_draws)) {
      done <- cache[draw == dr & status == "succeeded" & !is.na(text), custom_id]
      todo <- setdiff(items$custom_id, done)
      if (!length(todo)) { cat(sprintf("%s draw %d/%d: cached.\n", label, dr, n_draws)); next }
      cat(sprintf("%s draw %d/%d: firing %d.\n", label, dr, n_draws, length(todo)))
      res <- fire_one(items[custom_id %in% todo]); res[, draw := dr]
      cache <- rbind(cache[!(draw == dr & custom_id %in% todo)], res, fill = TRUE)
      saveRDS(cache, cache_file)
    }
  }
  cache
}
cl_raw <- fire_draws(cl_raw_file, function(it) run_claude_direct_fun(
  items = it, system_prompt = inst$prompt, schema = inst$schema,
  max_tokens = 1500L, effort = "medium", concurrency = 3L, timeout = 300L), "Claude")
ds_raw <- fire_draws(ds_raw_file, function(it) run_deepseek_batch_fun(
  items = it, system_prompt = inst$prompt, temperature = 0.4,
  max_tokens = 1500L), "DeepSeek")

parse_cache <- function(raw, model) {
  raw <- raw[status == "succeeded" & !is.na(text)]
  rbindlist(Map(parse_policy_findings_fun, raw$text, raw$custom_id, model, raw$draw),
            fill = TRUE)
}
recoded <- rbind(parse_cache(cl_raw, "claude"), parse_cache(ds_raw, "deepseek"),
                 fill = TRUE)[!is.na(`function`)]
cat(sprintf("Recoded findings: %d over %d documents\n", nrow(recoded),
            uniqueN(recoded$rec_id)))

# MERGE WITH THE IRRIGATION DOCUMENTS #########################################

draws <- fread(file.path(out_dir, "policy_promise_draws.csv"), encoding = "UTF-8")
draws <- draws[!rec_id %in% general_docs]
draws[rec_id %in% catalan_parts, rec_id := catalan_id]
draws <- rbind(draws, recoded, fill = TRUE)
fwrite(draws, file.path(out_dir, "policy_promise_draws_irrigation.csv"))

agg <- aggregate_extraction_fun(
  draws[, .(rec_id, model, draw, `function`, direction, scale, consistency,
            evidence_strength)])
pm <- agg$consensus
fwrite(pm, file.path(out_dir, "policy_promise_map_irrigation.csv"))
fwrite(agg$reliability, file.path(out_dir, "policy_promise_reliability_irrigation.csv"))

# READOUT: shares before and after -------------------------------------------

old <- fread(file.path(out_dir, "policy_promise_map.csv"))
share <- function(m) m[delivered_consensus == TRUE,
                       .(n_docs = uniqueN(rec_id)), by = `function`][
                         , share := n_docs / uniqueN(m$rec_id)][]
cmp <- merge(share(old)[, .(`function`, old_docs = n_docs, old_share = share)],
             share(pm)[, .(`function`, new_docs = n_docs, new_share = share)],
             by = "function", all = TRUE)
cat(sprintf("\nDocuments: %d before, %d after\n", uniqueN(old$rec_id), uniqueN(pm$rec_id)))
print(cmp[order(-new_share)])
cat("\nRecoded cells (consensus) for the three general documents:\n")
print(pm[rec_id %in% general_docs & delivered_consensus == TRUE,
         .(rec_id, `function`, claude, deepseek)])

# QUOTE TABLE ##################################################################

adj <- rbindlist(list(
  data.table(rec_id = "B1_plan_regadia_andalucia", fn = "risk_management",
             needle = "Reglamento Delegado (UE) 2024/1765", verdict = "drop",
             reason = "title of a regulation in the legal-framework list, not a promise"),
  data.table(rec_id = "C1_pepac_espana_v5-1", fn = "water_conservation",
             needle = "reducirá significativamente el uso y riesgo", verdict = "replace",
             reason = "sentence is about pesticides"),
  data.table(rec_id = "A3_prtr_componente3", fn = "social_cohesion",
             needle = "garantizando de esta forma la cohesión social", verdict = "replace",
             reason = "sentence belongs to the agri-food digitalisation reform, not irrigation"),
  data.table(rec_id = "A4_perte_digitalizacion_agua_memoria", fn = "social_cohesion",
             needle = "el PERTE refuerza la cohesión social", verdict = "drop",
             reason = "about the PERTE as a whole, not its irrigation measures"),
  data.table(rec_id = "A4_perte_digitalizacion_agua_memoria", fn = "biodiversity",
             needle = "fomentando la protección de las aguas", verdict = "drop",
             reason = "water-body objectives of the whole water cycle"),
  data.table(rec_id = "A4_perte_digitalizacion_agua_memoria", fn = "risk_management",
             needle = "Los organismos de cuenca y la AEMET", verdict = "drop",
             reason = "weather alerts for civil protection, not irrigation"),
  data.table(rec_id = "C2_reg_eu_2021-2115", fn = "crop_production",
             needle = "La Unión debe impulsar un sector agrícola", verdict = "drop",
             reason = "general CAP objective"),
  data.table(rec_id = "C2_reg_eu_2021-2115", fn = "social_cohesion",
             needle = "es fundamental reforzar el tejido socioeconómico", verdict = "drop",
             reason = "general CAP objective for rural areas"),
  data.table(rec_id = "C2_reg_eu_2021-2115", fn = "biodiversity",
             needle = "Habida cuenta de la importancia de hacer frente", verdict = "drop",
             reason = "general CAP objective"),
  data.table(rec_id = "C2_reg_eu_2021-2115", fn = "risk_management",
             needle = "si bien los agricultores son los responsables", verdict = "drop",
             reason = "farm risk-management tools of the CAP, not irrigation")))
fwrite(adj, file.path(out_dir, "policy_quotes_adjudicated.csv"))

replacements <- rbindlist(list(
  data.table(rec_id = "C1_pepac_espana_v5-1", fn = "water_conservation", language = "es",
             verbatim_clean = "Este plan de modernización de regadíos consiste en inversiones para la modernización de regadíos sostenibles en cada una de las actuaciones seleccionadas, con el objetivo de promover el ahorro de agua así como la eficiencia y el ahorro energético en los regadíos españoles.",
             english_translation = "This irrigation modernisation plan consists of investments in the modernisation of sustainable irrigation in each of the selected actions, with the objective of promoting water saving as well as efficiency and energy saving in Spanish irrigated areas.",
             context = "Plan para la mejora de la eficiencia y la sostenibilidad en regadíos"),
  data.table(rec_id = "A3_prtr_componente3", fn = "social_cohesion", language = "es",
             verbatim_clean = "Con esta reforma se busca establecer un mecanismo de gobernanza a nivel nacional, que permita a todos los representantes de los sectores afectados (Administraciones públicas, comunidades de regantes, agentes del territorio, entidades privadas, asociaciones de protección del medio ambiente, etc.), cooperar en todos los aspectos relacionados con el regadío español, tales como su sostenibilidad, criterios de ejecución y los aspectos relacionados con toda la normativa que le es de aplicación.",
             english_translation = "This reform seeks to establish a governance mechanism at national level that allows all representatives of the sectors concerned (public administrations, irrigation communities, territorial actors, private entities, environmental protection associations, etc.) to cooperate on every aspect of Spanish irrigation, such as its sustainability, implementation criteria and all the regulations that apply to it.",
             context = "C3.R4 Impulso a la gobernanza y a la gestión sostenible de los regadíos, descripción de la reforma")))

norm <- function(x) gsub(" +", " ", tolower(gsub("[^[:alnum:]]", " ", x)))
page_of <- function(doc, quote) {
  f <- file.path(pol_dir, paste0(doc, ".pdf"))
  np <- as.integer(sub("^Pages:\\s*", "", grep("^Pages:", system(
    sprintf("pdfinfo %s", shQuote(f)), intern = TRUE), value = TRUE)))
  needle <- norm(substr(quote, 1, 60))
  for (p in seq_len(np)) {
    pt <- paste(system(sprintf("pdftotext -f %d -l %d %s - 2>/dev/null", p, p, shQuote(f)),
                       intern = TRUE), collapse = " ")
    if (grepl(needle, norm(pt), fixed = TRUE)) return(p)
  }
  NA_integer_
}
for (i in seq_len(nrow(replacements))) {
  body <- norm(read_lin(replacements$rec_id[i]))
  stopifnot(grepl(norm(replacements$verbatim_clean[i]), body, fixed = TRUE))
}
replacements[, page := vapply(seq_len(.N), function(i)
  page_of(rec_id[i], verbatim_clean[i]), integer(1))]
print(replacements[, .(rec_id, fn, page)])

# Rebuild the two quote files -------------------------------------------------

q <- fread(file.path(out_dir, "policy_quotes_pages.csv"), encoding = "UTF-8")
q[, drop := FALSE]
for (i in seq_len(nrow(adj)))
  q[rec_id == adj$rec_id[i] & fn == adj$fn[i] &
      grepl(adj$needle[i], verbatim_clean, fixed = TRUE), drop := TRUE]
stopifnot(sum(q$drop) == nrow(adj))
q <- q[drop == FALSE][, drop := NULL]
q <- rbind(q, replacements[, .(rec_id, fn, language, verbatim_clean,
                               english_translation, context, page)])

cells <- pm[delivered_consensus == TRUE, .(rec_id, fn = `function`)]
cells <- rbind(cells, CJ(rec_id = catalan_parts, fn = cells[rec_id == catalan_id, fn]))
before <- nrow(q)
q <- q[cells, on = .(rec_id, fn), nomatch = NULL]
cat(sprintf("\nQuotes: %d after adjudication, %d after dropping unpromised cells\n",
            before, nrow(q)))
print(q[, .N, by = fn])
fwrite(q, file.path(out_dir, "policy_quotes_pages.csv"))
m <- q[, .(rec_id, `function` = fn, language, verbatim_clean, english_translation, context)]
fwrite(m, file.path(out_dir, "policy_quotes_manuscript.csv"))
