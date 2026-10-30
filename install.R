# Installs the R packages of the release at the versions used to build it.
# CRAN packages come from the CRAN archive by version; the two development
# packages come from GitHub at the recorded commit. Run from the repository root.

if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

remotes::install_version("sensobol", version = "1.2.0")
remotes::install_version("data.table", version = "1.18.4")
remotes::install_version("scales", version = "1.4.0")
remotes::install_version("here", version = "1.0.2")
remotes::install_version("jsonlite", version = "2.0.0")
remotes::install_version("readxl", version = "1.4.5")
remotes::install_version("openxlsx", version = "4.2.8")
remotes::install_version("writexl", version = "1.5.4")
remotes::install_version("sf", version = "1.1.0")
remotes::install_version("rnaturalearth", version = "1.1.0")
remotes::install_version("rnaturalearthdata", version = "1.0.0")
remotes::install_version("ggrepel", version = "0.9.6")
remotes::install_version("httr2", version = "1.2.1")
remotes::install_version("stringi", version = "1.8.7")
remotes::install_version("digest", version = "0.6.39")
remotes::install_version("reticulate", version = "1.43.0")
remotes::install_version("xml2", version = "1.5.2")
remotes::install_version("pdftools", version = "3.6.0")
remotes::install_version("rmarkdown", version = "2.30")
remotes::install_version("knitr", version = "1.51")
remotes::install_version("benchmarkme", version = "1.0.8")
remotes::install_github("tidyverse/ggplot2@6870419aa6e106c3580c45c81d5b688cb31758bd")
remotes::install_github("wilkelab/cowplot@b18d820d3af26b749235c4e4bc18bf416fba88e1")
