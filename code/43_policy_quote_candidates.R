# CANDIDATE SENTENCES LINKING A MODERNISATION TECHNOLOGY TO A FUNCTION ########

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "stringi"))

# Settings --------------------------------------------------------------------

utf8 <- function(x) { Encoding(x) <- "UTF-8"; x }   # patterns hold accented letters; valid in any locale
lin_dir <- here("datasets", "inputs", "policy_corpus", "linearized")
out_file <- here("datasets", "output", "policy_modernisation_candidates.csv")
anchor <- paste0("moderniz|modernitz|modernis|mejora (de|del|de los|y consolidaci)|millora (de|dels|del)",
                 "|tecnific|transformaci(ó|o)n? (en|a|del|de) regad|goteo|degoteig|riego localizado|reg localitzat",
                 "|aspersi|presuriz|pressuritz|pressuri[sz]|automat|telecontrol|telegesti|sensor|sonda|digitali",
                 "|contador|comptador|caudal(í|i)metro|cabal(í|i)metre|teledetecci|sat(é|e|è)l(·)?lit|drip|sprinkler",
                 "|remote sensing|scada|flow meter|metering|upgrad|rehabilitat|a la demanda|on-demand|on demand",
                 "|programaci(ó|o)n del riego|asesoramiento al regante|balsa|embalse de regulaci|entubad|canalizaci",
                 "|infraestructuras? de (regad|riego)|infraestructures de reg|instalaci(ó|o)n(es)? de riego|mejorar (una|la|las) instalaci|modern water control|modern irrigation",
                 "|inversi(ó|o)n(es)? en (el |la |las |los )?(regad|riego|instalaciones de riego|infraestructuras)|investments? in irrigation",
                 "|nuevas tecnolog|noves tecnolog|new technolog|tecnolog(í|i)as? (de|del|para el) riego|inteligencia artificial|iot\\b|big data",
                 "|control digital|digital|tecnolog|presuriza|presurizado|riego a presi(ó|o)n|regs? moderns?|reg modern|modern on-farm|hardware")
anchor <- utf8(anchor)
fun_lex <- c(
  water_conservation = "ahorro|ahorr|consumo de agua|uso (eficiente )?del agua|racionaliz|recursos h(í|i)dric|eficiencia (h(í|i)drica|en el uso|de riego|del riego|en la aplicaci)|estalvi|(ú|u)s de l'aigua|aigua|water saving|water-saving|water use efficiency|less water|reduce water|water conservation|water use|dotaci(ó|o)n|p(é|e)rdidas de agua|filtracion|percolaci|p(è|e)rdues|volumen de agua|hm3",
  crop_production = "producci(ó|o)n agr|producci(ó|o)n final|rendimiento|cosech|productividad|yield|crop production|collit|productivitat|producci(ó|o) agr|rentab|rendib|producci(ó|o)n de alimentos",
  risk_management = "sequ(í|i)a|sequera|drought|riesgo|risc|risk|garant(í|i)a (de|del) (suministro|riego)|garantia|seguridad h(í|i)drica|inundaci|cambio clim|canvi clim|climate change|resilien|vulnerab|variabilidad|adaptaci(ó|o)n",
  social_cohesion = "despobla|fijar (la )?poblaci|fijaci(ó|o)n de (la )?poblaci|asentamiento|empleo|ocupaci(ó|o) |llocs de treball|rendes|calidad de vida|qualitat de vida|cohesi(ó|o)n|cohesi(ó|o) |vertebra|comunidades de regantes|comunitats de regants|water user association|equity|equidad|equitat|relevo generacional|j(ó|o)venes|joves|social|fixar poblaci|anarchy",
  biodiversity = "biodivers|ecosistem|h(á|a)bitat|especies|esp(è|e)cies|fauna|avifauna|humedal|zones humides|wetland|espacios naturales|espais naturals|valors ecol|valores ecol|environment",
  groundwater_recharge = "acu(í|i)fero|aq(ü|u)(í|i)fer|aquifer|subterr(á|a)nea|subterr(à|a)ni|groundwater|recarga|rec(à|a)rrega|sobreexplotaci",
  soil_health = "suelo|s(ò|o)l |soil|erosi(ó|o)|saliniz|salinitz|fertilidad|materia org(á|a)nica",
  pest_control = "plaga|plagues|pest|enfermedad|malalti|fitosanitari",
  microclimate_regulation = "microclim|heladas|gelades|frost|temperatura",
  recreation_wellbeing = "paisaj|paisatg|landscape|recreo|ocio|lleure|patrimoni|cultural|bienestar|benestar",
  livestock = "ganad|ramad|livestock|pasto|forraje|farratge",
  medicinal_food = "medicinal|arom(á|a)tic|huerto familiar|autoconsumo")
fun_lex <- vapply(fun_lex, utf8, character(1))

# SENTENCES ###################################################################

split_sentences <- function(txt) {
  txt <- gsub("\\s+", " ", txt)
  s <- unlist(stri_split_regex(txt, utf8("(?<=[.;:])\\s+(?=[A-ZÁÉÍÓÚÀÈÒÇ\\(«“\"])")))
  s <- unlist(stri_split_regex(s, "\\s+(?=[a-z]\\)\\s)"))   # lettered list items of the legal texts
  s <- trimws(s)
  s[nchar(s) >= 40 & nchar(s) <= 1500]
}
irrigation_only <- c("A3_prtr_componente3", "A4_perte_digitalizacion_agua_memoria",
                     "C1_pepac_espana_v5-1", "C2_reg_eu_2021-2115")
irr_term <- utf8("regad|riego|riegos|regant|regadiu|irrigat|irrigaci")
files <- list.files(lin_dir, pattern = "\\.txt$", full.names = TRUE)
cand <- rbindlist(lapply(files, function(f) {
  rec_id <- sub("\\.txt$", "", basename(f))
  s <- split_sentences(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = " "))
  hit_anchor <- grepl(anchor, s, ignore.case = TRUE, perl = TRUE)
  if (rec_id %in% irrigation_only) hit_anchor <- hit_anchor & grepl(irr_term, s, ignore.case = TRUE, perl = TRUE)
  rbindlist(lapply(names(fun_lex), function(fn) {
    hit_fn <- grepl(fun_lex[[fn]], s, ignore.case = TRUE, perl = TRUE)
    keep <- which(hit_anchor & hit_fn)
    if (!length(keep)) return(NULL)
    data.table(rec_id = rec_id, fn = fn, sentence_no = keep, n_chars = nchar(s[keep]),
               anchor_terms = vapply(s[keep], function(x) paste(unique(tolower(
                 regmatches(x, gregexpr(anchor, x, ignore.case = TRUE, perl = TRUE))[[1]])), collapse = "; "), character(1)),
               function_terms = vapply(s[keep], function(x) paste(unique(tolower(
                 regmatches(x, gregexpr(fun_lex[[fn]], x, ignore.case = TRUE, perl = TRUE))[[1]])), collapse = "; "), character(1)),
               sentence = s[keep])
  }))
}))
setorder(cand, rec_id, fn, n_chars)
fwrite(cand, out_file)
cat("candidates:", nrow(cand), "\n")
print(dcast(cand[, .N, by = .(rec_id = substr(rec_id, 1, 3), fn)], fn ~ rec_id, value.var = "N", fill = 0))
