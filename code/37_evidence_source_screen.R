# DOES THE SOURCE ACTUALLY MENTION THE FUNCTION? ##############################

suppressMessages(library(data.table)); suppressMessages(library(writexl)); suppressMessages(library(here))
d <- fread(here("datasets","output","literature_benefit_space_draws.csv")); setnames(d,"function","fn")
m <- fread(here("datasets","output","literature_fulltext_manifest.csv"))
terms <- list(
  livestock = "\\blivestock|\\bcattle|\\bgrazing|\\bforage|\\bpasture|\\bherds?\\b|\\banimals?\\b",
  medicinal_food = "\\bmedicinal|\\bherbs?\\b|nutraceutic|\\bvitamins?\\b|\\bnutrition|\\bedible",
  recreation_wellbeing = "\\brecreat|\\bleisure|\\bbathing|\\bamenity|\\btourism|\\bcultural\\b|\\bwellbeing|\\bwell-being",
  biodiversity = "\\bbiodivers|\\bspecies\\b|\\bhabitat|\\becolog|\\bfauna|\\bflora",
  social_cohesion = "\\bsocial\\b|\\bcommunity|\\bcohesion|\\bcollective|\\bequity\\b|\\bconflict",
  microclimate_regulation = "\\bmicroclimat|\\bhumidity\\b|\\bcooling\\b|\\bcanopy temperature|\\bmicro-climat",
  groundwater_recharge = "\\brecharg|\\baquifer|\\bgroundwater",
  pest_control = "\\bpests?\\b|\\bdiseases?\\b|\\bweeds?\\b|\\binsect|\\bpathogen")
base <- here("datasets","inputs","literature_corpus","fulltexts")
getxt <- function(f) { if (!file.exists(f)) return(NULL)
  if (grepl("\\.pdf$", f)) system(sprintf("pdftotext '%s' - 2>/dev/null", f), intern=TRUE) else readLines(f, warn=FALSE) }
pairs <- unique(d[fn %in% names(terms), .(fn, rec_id)])
pairs <- merge(pairs, m[, .(rec_id, file, doi, title, year, technology)], by="rec_id", all.x=TRUE)
cache <- new.env()
res <- rbindlist(lapply(seq_len(nrow(pairs)), function(i) {
  k <- pairs$file[i]; if (is.na(k)) return(data.table(pairs[i], words=NA_integer_, hits=NA_integer_))
  if (is.null(cache[[k]])) assign(k, getxt(file.path(base,k)), envir=cache)
  txt <- cache[[k]]
  data.table(pairs[i], words=if(is.null(txt)) NA_integer_ else sum(lengths(strsplit(txt,"\\s+"))),
             hits=if(is.null(txt)) NA_integer_ else sum(grepl(terms[[pairs$fn[i]]], txt, ignore.case=TRUE)))
}))
fwrite(res, here("datasets","output","evidence_source_screen.csv"))
flag <- res[!is.na(words) & words > 500 & hits == 0]
just <- unique(d[, .(fn, rec_id, evidence_strength, justification)])[
  , .(strengths = paste(unique(evidence_strength), collapse="/"),
      justification = justification[1]), by = .(fn, rec_id)]
flag <- merge(flag, just, by=c("fn","rec_id"), all.x=TRUE)
setorder(flag, fn, rec_id)
flag[, c("function_really_discussed","benefit_really_reported","checked_by","notes") :=
       list(NA, NA, NA_character_, NA_character_)]
setcolorder(flag, c("rec_id","fn","function_really_discussed","benefit_really_reported",
                    "checked_by","notes","doi","title","year","technology","strengths",
                    "justification","hits","words","file"))
write_xlsx(flag, here("datasets","output","evidence_source_flags.xlsx"))
cat("screened:", res[!is.na(words) & words>500, .N], "| flagged:", nrow(flag), "\n")
print(flag[, .N, by=fn][order(-N)])
