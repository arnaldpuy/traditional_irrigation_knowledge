# PROVENANCE CHECKS ###########################################################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table", "jsonlite", "readxl"))

out <- here("datasets", "output")

# VALID DRAWS PER ITEM AND ARM ################################################

arm_files <- c(standard_anthropic_openai = "checkpoint/chk_standard_raw_all.csv",
               short_anthropic_openai = "arm/arm_short_raw_all.csv",
               strict_anthropic_openai = "arm/arm_strict_raw_all.csv",
               standard_deepseek = "v4_ruleonly_standard_deepseek_raw_all.csv",
               short_deepseek = "v4_ruleonly_short_deepseek_raw_all.csv",
               strict_deepseek = "v4_ruleonly_strict_deepseek_raw_all.csv")
parse_ok <- function(txt) vapply(txt, function(t) {
  o <- tryCatch(fromJSON(t), error = function(e) NULL)
  !is.null(o) && !is.null(o$efficiency_legible)
}, logical(1), USE.NAMES = FALSE)
audit <- rbindlist(lapply(names(arm_files), function(nm) {
  d <- as.data.table(utils::read.csv(file.path(out, arm_files[[nm]]),
                                     stringsAsFactors = FALSE, encoding = "UTF-8"))
  if (!"provider" %in% names(d)) d[, provider := "deepseek"]
  d[provider == "deepseek" & grepl("deepseek", nm), provider := "deepseek"]
  d[, valid := parse_ok(text)]
  d[, id_cite := sub("__s.*$", "", custom_id)]
  d[, .(arm = nm, draws = .N, valid_draws = sum(valid)), by = .(provider, id_cite)]
}))
audit <- audit[!(grepl("deepseek", arm) & provider != "deepseek")]
audit <- audit[!(!grepl("deepseek", arm) & provider == "deepseek")]
audit[, layer := "headline"]

funcs12 <- c("crop_production", "water_conservation", "soil_health", "pest_control",
             "risk_management", "groundwater_recharge", "social_cohesion", "biodiversity",
             "microclimate_regulation", "recreation_wellbeing", "livestock", "medicinal_plants")
attr_file <- function(fam, ph) {
  if (fam == "anthropic" && ph == "standard") "coding_results_raw_analytical_anthropic.csv"
  else sprintf("coding_results_raw_attribution_%s_%s.csv", ph, fam)
}
names_fun <- function(txt) vapply(txt, function(t) {
  o <- tryCatch(fromJSON(t), error = function(e) NULL)
  !is.null(o) && length(intersect(unlist(o$functions), funcs12)) > 0
}, logical(1), USE.NAMES = FALSE)
attr_audit <- rbindlist(lapply(c("anthropic", "openai", "deepseek"), function(fam)
  rbindlist(lapply(c("strict", "standard", "broad"), function(ph) {
    d <- as.data.table(utils::read.csv(file.path(out, attr_file(fam, ph)),
                                       stringsAsFactors = FALSE, encoding = "UTF-8"))
    d <- d[!duplicated(custom_id)]
    d[, valid := names_fun(text)]
    d[, id_cite := sub("__s.*$", "", custom_id)]
    d[, .(provider = fam, arm = paste("attribution", ph, fam, sep = "_"), layer = "attribution",
          draws = .N, valid_draws = sum(valid)), by = id_cite]
  }))))
audit <- rbind(audit[, .(layer, arm, provider, id_cite, draws, valid_draws)],
               attr_audit[, .(layer, arm, provider, id_cite, draws, valid_draws)])
fwrite(audit, file.path(out, "valid_draw_audit.csv"))
short <- audit[valid_draws < draws]
cat(sprintf("Valid-draw audit: %d item x arm x provider cells, %d with fewer valid draws than fired\n",
            nrow(audit), nrow(short)))
print(short[, .(items = .N, min_valid = min(valid_draws)), by = .(layer, arm, provider)])

# LITERATURE FLOW ##############################################################

raw <- fread(file.path(out, "literature_search_raw.csv"))
queue <- fread(file.path(out, "literature_screening_queue.csv"))
votes <- fread(file.path(out, "literature_screening_votes.csv"))
adj <- as.data.table(read_xlsx(file.path(out, "literature_screening_adjudication.xlsx")))
manifest <- fread(file.path(out, "literature_fulltext_manifest.csv"))
ext <- fread(file.path(out, "literature_extraction_text.csv"))
cons <- fread(file.path(out, "literature_benefit_consensus.csv"))
flags <- fread(file.path(out, "evidence_flags_adjudicated.csv"))

both_in <- votes[cl_include == TRUE & ds_include == TRUE, .N]
both_out <- votes[cl_include == FALSE & ds_include == FALSE, .N]
flow <- data.table(stage = c(
  "raw search records (Scopus + Dimensions exports)",
  "after de-duplication, screened on title and abstract",
  "included by both models", "excluded by both models", "model disagreements, adjudicated by hand",
  "adjudicated include", "adjudicated exclude",
  "full-text manifest rows", "  retrieved", "  manual", "  landmark route",
  "extracted texts", "  usable body", "  abstract only", "  truncated at the character cap",
  "papers with a consensus reading", "excluded at reading (Chinese body text)",
  "papers in the evidence layer"),
  n = c(nrow(raw), nrow(queue), both_in, both_out, nrow(adj),
        adj[human_include == TRUE, .N], adj[human_include == FALSE, .N],
        nrow(manifest), manifest[status == "retrieved", .N], manifest[status == "manual", .N],
        manifest[route == "landmark", .N],
        nrow(ext), ext[source != "xml_abstract_only", .N], ext[source == "xml_abstract_only", .N],
        ext[truncated == TRUE, .N],
        uniqueN(cons$rec_id), flags[verdict == "exclude_paper", uniqueN(rec_id)],
        uniqueN(cons$rec_id) - flags[verdict == "exclude_paper", uniqueN(rec_id)]))
fwrite(flow, file.path(out, "literature_flow.csv"))
print(flow)
