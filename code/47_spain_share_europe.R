# SPAIN'S SHARE OF EUROPE'S IRRIGATED AREA ####################################

# Load packages ---------------------------------------------------------------

if (!requireNamespace("sensobol", quietly = TRUE)) install.packages("sensobol")
sensobol::load_packages(c("here", "data.table"))

# Read -----------------------------------------------------------------------

areas <- fread(here("datasets", "inputs", "country_level_irrigated_areas.csv"))
str(areas)
areas[1:3]

# Share by dataset and resolution ---------------------------------------------

share <- areas[continent == "Europe",
               .(spain_mha = sum(mha[country == "Spain"]), europe_mha = sum(mha)),
               by = .(dataset, resolution)]
share[, spain_share := spain_mha / europe_mha]
setorder(share, dataset, resolution)
fwrite(share, here("datasets", "output", "spain_share_europe.csv"))

# Report ---------------------------------------------------------------------

cat(sprintf("Spain's share of Europe's irrigated area, %d dataset x resolution cells:\n",
            nrow(share)))
cat(sprintf("  median %.0f%%, interquartile range %.0f-%.0f%%, range %.0f-%.0f%%\n",
            100 * median(share$spain_share),
            100 * quantile(share$spain_share, 0.25), 100 * quantile(share$spain_share, 0.75),
            100 * min(share$spain_share), 100 * max(share$spain_share)))
print(share[, .(min = min(spain_share), max = max(spain_share)), by = dataset][order(min)])
