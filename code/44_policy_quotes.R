# QUOTATIONS LINKING IRRIGATION MODERNISATION TO A FUNCTION ###################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "stringi"))

# Settings --------------------------------------------------------------------

utf8 <- function(x) { Encoding(x) <- "UTF-8"; x }
lin_dir <- here("datasets", "inputs", "policy_corpus", "linearized")
pdf_dir <- here("datasets", "inputs", "policy_corpus")
out <- here("datasets", "output")
docs <- c(A1 = "A1_rd329-2002_plan_nacional_regadios", A2 = "A2_rd287-2006_plan_choque_regadios",
          A3 = "A3_prtr_componente3", A4 = "A4_perte_digitalizacion_agua_memoria",
          B1 = "B1_plan_regadia_andalucia", B2 = "B2_pla_regadius_catalunya_memoria",
          B2b = "B2b_pla_regadius_catalunya_annex2_proposta", C1 = "C1_pepac_espana_v5-1",
          C2 = "C2_reg_eu_2021-2115", D1 = "D1_fao_id63_masscote",
          D2 = "D2_burt_styles_1999_wr19_modern_water_control")
lang <- c(A1 = "es", A2 = "es", A3 = "es", A4 = "es", B1 = "es", B2 = "ca", B2b = "ca",
          C1 = "es", C2 = "es", D1 = "en", D2 = "en")

# THE SELECTION ###############################################################

p <- function(doc, fn, start, end, anchor, context = "", note = "", en = "")
  data.table(doc = doc, fn = fn, start = start, end = end, anchor = anchor, context = context,
             note = note, english_translation = en)
picks <- rbindlist(list(
  # water conservation ----------------------------------------------------------
  p("A1", "water_conservation", "Modernizar las infraestructuras de distribución", "reducir los consumos de agua.",
    "modernisation programme", "Artículo 2, objetivo d)", "",
    "Modernise the infrastructures for distributing and applying irrigation water in order to rationalise the use of resources, reduce agricultural pollution of surface and ground waters and promote innovations in irrigation systems to reduce water consumption."),
  p("A2", "water_conservation", "Por ello la necesidad imperiosa de ahorrar agua", "una cuestión prioritaria.",
    "modernisation programme", "Preamble", "",
    "Hence the pressing need to save water when modernising our irrigation systems becomes a priority issue."),
  p("A3", "water_conservation", "las inversiones pondrán a disposición del regante", "presión de extracciones establecidos.",
    "named technologies (drip, sprinkler, digital control)", "C3.I1, impacto en el ahorro de agua", "",
    "the investments will give irrigators more efficient irrigation systems (mainly drip or sprinkler with digital control systems) so as to meet the established requirements for reducing abstraction pressure."),
  p("A4", "water_conservation", "Este control digital de las explotaciones", "mejora de la productividad.",
    "named technology (digital control)", "Section 4.3", "",
    "This digital control of farms will achieve savings in the main farm inputs, such as fertilisers, pesticides and energy, as well as a more efficient use of water and an improvement in productivity."),
  p("B1", "water_conservation", "el fomento y la ejecución de obras destinadas a la transformación", "agua de riego y de la energía",
    "modernisation programme", "Preamble", "",
    "the promotion and execution of works for the transformation, modernisation and consolidation of irrigation systems, including the associated hydraulic infrastructure works, aimed at saving and using irrigation water and energy efficiently"),
  p("B2b", "water_conservation", "Les actuacions de modernització tenen per objectiu", "eficiències mitjanes del 80 %.",
    "modernisation programme (flood to modern irrigation)", "Annex 2, section 2.1.1", "",
    "The modernisation actions aim at a better use of water, acting on abstraction, transport and irrigation itself. Throughout this process, flood irrigation, traditional and predominant in these areas, with efficiencies of around 50%, must be transformed into modern and efficient irrigation, easily reaching average efficiencies of 80%."),
  p("C1", "water_conservation", "Suponiendo una eficiencia media del 70 % para un riego a pie y de un 95%", "ahorro potencial del 25 %",
    "named technology (drip replacing surface irrigation)", "Intervención 6843, ahorro potencial", "",
    "Assuming an average efficiency of 70% for surface irrigation and 95% for drip irrigation, switching from one to the other will entail a potential saving of at least 25%"),
  p("C2", "water_conservation", "Los Estados miembros podrán conceder ayudas a una inversión destinada a mejorar", "instalación o infraestructura existente",
    "investment in irrigation installations", "Artículo 74(4)", "a condition of support rather than a promise: support only if water savings are assessed ex ante",
    "Member States may grant support for an investment in the improvement of an existing irrigation installation or element of irrigation infrastructure only if: (a) it is assessed ex ante as offering potential water savings according to the technical parameters of the existing installation or infrastructure"),
  p("D1", "water_conservation", "the modernized downstream part would need to divert", "earlier surface techniques",
    "named technology (localised irrigation)", "Chapter 7, Cabannes case"),
  p("D2", "water_conservation", "In order to have both a good field-level water delivery service", "modernized in both operation and design.",
    "modernisation programme", "Chapter 2"),
  # crop production -------------------------------------------------------------
  p("A4", "crop_production", "Este control digital de las explotaciones", "mejora de la productividad.",
    "named technology (digital control)", "Section 4.3", "",
    "This digital control of farms will achieve savings in the main farm inputs, such as fertilisers, pesticides and energy, as well as a more efficient use of water and an improvement in productivity."),
  p("B2", "crop_production", "Amb base a l’anàlisi econòmica efectuada", "increment de l’ocupació agrària",
    "modernisation programme (the Plan)", "Memòria, Anàlisi econòmica", "effects attributed to the Plan's modernisations and new schemes together",
    "Based on the economic analysis carried out, which considered the public investment the Plan requires, the effects on the agricultural sector in terms of crop redistribution, increase in productive yields, increase in agricultural production, increase in gross value added and increase in agricultural employment"),
  p("C1", "crop_production", "El incremento de la productividad se debe", "riego presurizado.",
    "named technology (pressurised irrigation)", "Intervención 6843", "",
    "The increase in productivity is due to the improvement in the quality of water distribution and application brought by the new pressurised irrigation infrastructures."),
  p("D1", "crop_production", "The broad goals of modernization are to achieve", "better-quality return flows.",
    "modernisation programme", "Chapter 3"),
  p("D2", "crop_production", "Pressurized irrigation systems have problems", "with pressurized irrigation methods.",
    "named technology (pressurised irrigation)", "Chapter 6"),
  # risk management -------------------------------------------------------------
  p("A2", "risk_management", "actuaciones urgentes de mejora y consolidación de regadíos que se recogen", "efectos de la sequía en el regadío.",
    "modernisation programme", "Artículo 1", "",
    "urgent irrigation improvement and consolidation works listed in the annex as the 'Programme of Actions', with the aim of obtaining water savings capable of mitigating the effects of drought on irrigation."),
  p("B2", "risk_management", "La modernització, entesa com la millora de l’eficiència de reg, permet situar", "escenaris de canvi climàtic i horitzons temporals.",
    "modernisation programme", "Memòria, Anàlisi de tendències", "",
    "Modernisation, understood as the improvement of irrigation efficiency, makes it possible to place water demands significantly below current concessions, in all climate-change scenarios and time horizons."),
  p("C1", "risk_management", "Ayudas a inversiones productivas en explotaciones agrarias vinculadas a contribuir a la mitigación-adaptación al cambio climático, uso eficiente de los recursos naturales y bienestar animal, por el equipamiento", "sistemas eficientes.",
    "modernised plots with efficient systems", "Intervención 6841.1", "",
    "Support for productive investments on farms linked to contributing to climate-change mitigation and adaptation, efficient use of natural resources and animal welfare, through the equipment of modernised irrigated plots with efficient systems."),
  # social cohesion -------------------------------------------------------------
  p("A3", "social_cohesion", "La modernización de regadíos permite el desarrollo de una actividad agraria rentable", "en estos territorios.",
    "modernisation programme", "Vinculación con el Componente 4", "",
    "Irrigation modernisation enables the development of a profitable agricultural activity for the farmer, ensuring their settlement in rural areas and the retention of population in these territories."),
  p("A2", "social_cohesion", "Pero es necesario dar un paso más", "en todas las zonas de influencia.",
    "named technology (communication technologies)", "Preamble", "",
    "But it is necessary to go a step further in the modernisation of our irrigation systems, namely bringing irrigators into the information society through the deployment of modern communication technologies, to achieve greater overall efficiency of the irrigation system while creating quality employment able to manage irrigation water differently, which means the development of a strong services sector linked to irrigation modernisation that undoubtedly helps to consolidate the agro-industrial system associated with irrigation, in turn settling population and improving the quality of life in all the areas of influence."),
  p("B2", "social_cohesion", "A partir dels supòsits considerats", "en el sector agrícola.",
    "modernisation programme", "Memòria, Anàlisi econòmica", "",
    "On the assumptions considered, the implementation of the Irrigation Plan is expected to increase agricultural employment by about 9,678 Agricultural Work Units (7,113 AWU in the new irrigation schemes and 2,565 AWU due to modernisation), a global increase of 15.29% over the total active population registered in Catalonia in 2009 in the agricultural sector."),
  p("B2b", "social_cohesion", "La seva modernització significarà un estalvi potencial", "reg en temps real, telecontrol, ...",
    "named technologies (on-demand, real-time, remote control)", "Annex 2, section 2.1.1", "",
    "Their modernisation will mean a potential saving of water, while allowing a set of improvements in the management of the irrigation communities and of individual irrigators, such as: irrigation strategies with lower allocations, on-demand irrigation, real-time irrigation, remote control, ..."),
  p("D1", "social_cohesion", "The broad goals of modernization are to achieve", "better-quality return flows.",
    "modernisation programme", "Chapter 3"),
  p("D2", "social_cohesion", "Hardware modernization can drastically improve", "water user association can exist.",
    "modernisation programme (hardware)", "Executive summary, positive findings"),
  # groundwater -----------------------------------------------------------------
  p("A3", "groundwater_recharge", "garantizar la continuidad de la modernización de regadíos", "aguas superficiales o subterráneas.",
    "modernisation programme", "C3.R4", "pressure on aquifers, not recharge",
    "ensuring the continuity of irrigation modernisation that allows an improvement in the efficiency of water use, both in transport and in irrigation application systems, as well as the increasingly widespread use of reclaimed water, reducing pressure on surface and ground waters."),
  p("C1", "groundwater_recharge", "En el caso de las actuaciones de modernización para", "zonas de regadío subterráneo.",
    "modernisation programme", "Intervención 6843, Extremadura", "",
    "In the case of modernisation actions [...] in addition to what the national sheet provides, the following operations are specifically included: Recharge infrastructures and re-allocation of groundwater-irrigated areas."),
  p("D1", "groundwater_recharge", "The upstream part, devoted mainly to cereals", "some domestic supply.",
    "named technology (modern surface irrigation)", "Chapter 7, Cabannes case"),
  # soil health -----------------------------------------------------------------
  p("A3", "soil_health", "Se procurará la promoción de proyectos de modernización de los sistemas de riego localizado", "aguas subterráneas y al suelo",
    "named technology (drip, fertigation)", "DNSH assessment", "",
    "The promotion of projects to modernise drip irrigation systems will be sought, which allow fertilisers to be applied in appropriate doses and at the time the crop needs them, through fertigation, thereby reducing the leaching of pollutants into groundwater and soil"),
  p("C1", "soil_health", "Se adapta a su vez a los objetivos de la Directiva de Nitratos", "en el momento que en el cultivo lo necesita",
    "named technologies (sprinkler, drip)", "Section 2, water", "",
    "It is likewise aligned with the objectives of the Nitrates Directive and those of reducing soil contamination by pesticides, since the deployment of sprinkler and drip irrigation systems allows fertilisers and pesticides to be applied in the irrigation water in the strictly necessary dose and at the time the crop needs them"),
  # biodiversity: environment named, biodiversity not ---------------------------
  p("A2", "biodiversity", "En ningún caso, se podrá aumentar", "abastecimiento a las poblaciones.",
    "modernisation programme", "Artículo 3", "environmental needs named; biodiversity not",
    "In no case may the delimitation of the existing irrigation polygons and areas be increased, and the savings produced shall likewise be applied to meeting environmental needs in the area considered and to improving the security of supply to towns."),
  p("B2", "biodiversity", "La modernització permet evitar aquest bloqueig temporal", "sistemes naturals lligats a l’aigua.",
    "modernisation programme", "Memòria, Anàlisi de tendències", "natural systems named; biodiversity not",
    "Modernisation makes it possible to avoid this temporary blocking of the resource at times of deficit, through the differential generated by its greater efficiency, making it available for other uses or to guarantee the functioning of the natural systems linked to water."),
  p("D1", "biodiversity", "The broad goals of modernization are to achieve", "better-quality return flows.",
    "modernisation programme", "Chapter 3", "improved environment named; biodiversity not")
))
for (col in c("start", "end", "english_translation", "context", "note", "anchor")) set(picks, j = col, value = utf8(picks[[col]]))

# CUT, CLEAN AND VERIFY #######################################################

read_doc <- function(doc) gsub("\\s+", " ", paste(readLines(file.path(lin_dir, paste0(docs[[doc]], ".txt")),
                                                      warn = FALSE, encoding = "UTF-8"), collapse = " "))
texts <- lapply(names(docs), read_doc); names(texts) <- names(docs)
cut_quote <- function(doc, start, end) {
  txt <- texts[[doc]]
  s <- regexpr(start, txt, fixed = TRUE)
  if (s < 0) return(NA_character_)
  rest <- substr(txt, s, nchar(txt))
  e <- regexpr(end, rest, fixed = TRUE)
  if (e < 0) return(NA_character_)
  substr(rest, 1, e + nchar(end) - 1L)
}
repair <- function(x) {
  x <- gsub("[[:cntrl:]]", " ", x)                          # bullet glyphs extracted as control characters
  x <- gsub(utf8("­ ?"), "", x)                       # soft hyphen of the PDF line breaks
  x <- gsub(utf8("([a-záéíóúñçàèòïü])- ([a-záéíóúñçàèòïü])"), "\\1\\2", x, perl = TRUE)  # "moderniza- ción"
  gsub("\\s+", " ", x)
}
picks[, rec_id := docs[doc]]
picks[, language := lang[doc]]
picks[, verbatim_raw := mapply(cut_quote, doc, start, end)]
stopifnot(!anyNA(picks$verbatim_raw))
picks[, verbatim_clean := repair(verbatim_raw)]
picks[, verified_raw := mapply(function(d, q) grepl(q, texts[[d]], fixed = TRUE), doc, verbatim_raw)]
picks[, verified_clean := mapply(function(d, q) grepl(q, repair(texts[[d]]), fixed = TRUE), doc, verbatim_clean)]
stopifnot(all(picks$verified_raw), all(picks$verified_clean))
picks[, n_chars := nchar(verbatim_clean)]

# PAGES #######################################################################

norm_txt <- function(x) stri_replace_all_regex(stri_trans_tolower(stri_replace_all_regex(utf8(x), "[^\\p{L}\\p{N}]+", " ")), " +", " ")
pages <- lapply(names(docs), function(d) {
  f <- file.path(pdf_dir, paste0(docs[[d]], ".pdf"))
  if (!file.exists(f)) return(character(0))
  txt <- paste(suppressWarnings(system2("pdftotext", c("-enc", "UTF-8", shQuote(f), "-"), stdout = TRUE, stderr = FALSE)), collapse = "\n")
  vapply(strsplit(utf8(txt), "\f", fixed = TRUE)[[1]], norm_txt, character(1))
}); names(pages) <- names(docs)
locate <- function(d, q) {
  needle <- norm_txt(substr(q, 1, 60))
  hit <- which(vapply(pages[[d]], function(pg) grepl(needle, pg, fixed = TRUE), logical(1)))
  if (length(hit)) hit[1] else NA_integer_
}
picks[, page := mapply(locate, doc, verbatim_clean)]

# OUTPUT ######################################################################

fn_lab <- c(water_conservation = "Water conservation", crop_production = "Crop production",
            risk_management = "Risk management", social_cohesion = "Social cohesion",
            groundwater_recharge = "Groundwater recharge", soil_health = "Soil health maintenance",
            biodiversity = "Biodiversity conservation")
picks[, fn_ord := match(fn, names(fn_lab))]
setorder(picks, fn_ord, doc)
fwrite(picks[, .(rec_id, doc, fn, language, anchor, context, note, verbatim_clean, english_translation,
                 page, verified_raw, verified_clean, n_chars)],
       file.path(out, "policy_quotes_modernisation.csv"))
esc <- function(x) { x <- gsub("([&%$#_{}])", "\\\\\\1", x); gsub("~", "\\\\textasciitilde{}", x) }
doc_lab <- c(A1 = "Plan Nacional de Regadíos (RD 329/2002), MAPA 2002",
             A2 = "Plan de Choque de Modernización de Regadíos (RD 287/2006), MAPA/MMA 2006",
             A3 = "PRTR, Componente 3, Gobierno de España 2021",
             A4 = "PERTE de Digitalización del Ciclo del Agua, MITECO 2022",
             B1 = "Plan REGADIA, Junta de Andalucía 2025",
             B2 = "Pla de Regadius de Catalunya 2008--2020, Memòria, Generalitat de Catalunya 2010",
             B2b = "Pla de Regadius de Catalunya 2008--2020, Annex 2, Generalitat de Catalunya 2010",
             C1 = "Plan Estratégico de la PAC de España 2023--2027 (v5.1), MAPA/EC 2025",
             C2 = "Regulation (EU) 2021/2115, European Union 2021",
             D1 = "FAO Irrigation and Drainage Paper 63 (MASSCOTE), FAO 2007",
             D2 = "Burt \\& Styles, FAO Water Report 19, FAO/IPTRID/World Bank 1999")
doc_lab <- vapply(doc_lab, utf8, character(1)); fn_lab <- vapply(fn_lab, utf8, character(1))
tab <- picks[, .(fns = paste(esc(fn_lab[fn[order(fn_ord)]]), collapse = " \\newline "),
                 fn_first = min(fn_ord), doc = doc[1], language = language[1],
                 english_translation = english_translation[1], context = context[1],
                 page = page[1], note = paste(unique(note[nzchar(note)]), collapse = "; ")),
             by = .(verbatim_clean)]
setorder(tab, fn_first, doc)
rows <- tab[, sprintf(
  "\\textbf{%s} & \\emph{``%s''} %s \\\\\n & \\footnotesize %s%s, p.~%s%s \\\\\n\\addlinespace\n",
  fns, esc(verbatim_clean),
  fifelse(language == "en", "", sprintf("[%s]", esc(english_translation))),
  doc_lab[doc], fifelse(nzchar(context), paste0(", ", esc(context)), ""),
  fifelse(is.na(page), "n/a", as.character(page)),
  fifelse(nzchar(note), paste0(" (", esc(note), ")"), ""))]
writeLines(c("\\begin{longtable}{p{2.2cm} >{\\raggedright\\arraybackslash}p{11.6cm}}",
             "\\caption{Quotations in which the policy documents attribute each function to irrigation modernisation, a named technology or the modernisation investment itself. A passage that names several functions lists all of them in the left column. Quotations in Spanish and Catalan are followed by an English translation in square brackets.}",
             "\\label{tab:SM_policy_quotes}\\\\",
             "\\toprule", "Function & Verified quotation and source \\\\", "\\midrule",
             "\\endfirsthead",
             "\\multicolumn{2}{@{}l}{\\small\\itshape Table~\\ref{tab:SM_policy_quotes} (continued)}\\\\",
             "\\toprule", "Function & Verified quotation and source \\\\", "\\midrule",
             "\\endhead", rows, "\\bottomrule", "\\end{longtable}"),
           here("manuscript", "sm_policy_quotes_modernisation.tex"), useBytes = TRUE)
picks[, document := sub("b$", "", doc)]
cat(sprintf("quotations: %d (%d distinct passages); verified: %d; pages located: %d\n", nrow(picks), nrow(tab), sum(picks$verified_clean), sum(!is.na(picks$page))))
cat("\nDocuments (of 10) with a quotation per function:\n")
print(picks[, .(documents = uniqueN(document), docs = paste(sort(unique(document)), collapse = " ")), by = fn][order(-documents)])
