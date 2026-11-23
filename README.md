<!-- [![DOI](https://zenodo.org/badge/DOI/XXXX.svg)](https://doi.org/XXXX) -->

# Irrigation modernisation is blind to most traditional knowledge

[Arnald Puy](https://www.arnaldpuy.com), Carmen Aguiló-Rivera, Seth N. Linga, Ana-Maria Crețu, Miguel Banegas García, Gerlo Borghuis, Louise Busschaert, Antonio Candel Turpín, Giovanni De Grandis, Mona Liza F. Delos Reyes, Francisco Garrido Avilés, Pablo Garrido Guillamón, Jose María García Avilés, Jerry W. Knox, Bruce Lankford, Antonio Mirón, Nicola Paciolla, Bich Tran, Saskia van der Kooij, Dominik Wisser, Victoria Reyes-García\*

\*Corresponding author.

R and Python code accompanying the paper *Irrigation modernisation is blind to most traditional knowledge*, whose abstract is the following:

## Abstract

*The modernisation of traditional irrigation systems by technologies designed to minimize water use and maximize crop yield is promoted worldwide to improve global water and food security. However, we do not know how much traditional knowledge gets lost when these efficiency-based irrigation frameworks are implemented. Here we address this gap by analyzing 475 assumptions (fragments of tacit knowledge) extracted from 110 interviews across seven Spanish irrigation systems, where such knowledge has accumulated for over a millennium. We find that at least 60% of the assumptions contain knowledge that is irreducible to a water-in/yield-out metric and that they serve functions spanning production, hydrology, ecology and social organization. red{Although many irrigation technologies promise to serve some of these functions, XXXXXXXXX.} Our results indicate that efficiency is not a sufficient criterion for modernizing irrigation systems: evaluating improvement requires accounting for the knowledge and functions that technological substitution may eliminate.*

## Contents

The analysis combines 475 assumptions extracted from 110 interviews in seven traditional irrigation communities in Spain, a Large Language Model (LLM) coding scheme, a global uncertainty and sensitivity analysis over 1,080 analytical specifications, and a review of the policy and scientific literature on irrigation modernization.

- [Background](#background)
- [Repository structure](#repository-structure)
- [Dependencies](#dependencies)
- [How to run](#how-to-run)
- [What is and is not reproducible offline](#what-is-and-is-not-reproducible-offline)
- [Where each result comes from](#where-each-result-comes-from)
- [Provenance of the assumption corpus](#provenance-of-the-assumption-corpus)
- [Functions](#functions)
- [Outputs](#outputs)
- [Citation](#citation)
- [License](#license)

---

## Background

The modernization of traditional irrigation systems with technologies that minimize water use and maximize crop yield is promoted worldwide. This repository holds the data and code behind the finding that most of the knowledge sustaining these systems cannot be restated in water-in/yield-out terms, and that frameworks measuring water and crop efficiency miss more than two thirds of it.

---

## Repository structure

```
traditional_irrigation_knowledge/
├── code/
│   ├── 00_reproduce.R                          # offline entry point: runs the analysis scripts, checks checksums, renders the notebook
│   ├── traditional_irrigation_knowledge.Rmd    # notebook: every figure and the Results numbers of the paper and the SM
│   ├── 01_parse_assumptions.py                 # the 475 assumptions from the published assumption tables
│   ├── 02_coding_run.R … 04_attribution_arms.R # LLM coding of the assumptions (gated: need API keys)
│   ├── 05_intermodel_agreement.R … 11_severing.R         # uncertainty and sensitivity analysis on the stored replies
│   ├── 12_embeddings.py … 16_coding_collapse.R           # label-free checks (Text S1.2, S1.3)
│   ├── 17_planted_items.R … 20_language_conditions.R     # tests of the instrument (Text S1.1)
│   ├── 21_necessity_search.R … 26_necessity_quote_pages.py   # the necessity premise (Table S1)
│   ├── 27_policy_linearize.py … 31_quote_locations.R    # modernization policies (Text S1.5, Table S4)
│   ├── 32_literature_search.R … 41_link_extraction.R    # the literature on the technologies (Text S1.7)
│   ├── 42_provenance_checks.R                  # valid-draw audit and literature flow table
│   ├── 43_policy_quote_candidates.R … 45_human_validation_ratings.R   # policy quotes; the three human raters
│   ├── 46_within_domain_or.R                   # reducibility by function load, LLMs and human raters, within each domain
│   ├── 47_spain_share_europe.R                 # Spain's share of Europe's irrigated area across ten global datasets
│   ├── 48_fig_multiverse_shares.R              # the 1,080 irreducible shares behind Fig. 1, in the order the TikZ tree reads them
│   └── historical/                             # scripts that produced archived replies but depend on undistributed inputs
├── functions/        # helper functions, one *_fun per file
├── prompts/          # the LLM instruments behind every reported number
└── datasets/
    ├── inputs/       # the 475 assumptions (no interviewee identifiers), the published assumption tables,
    │                 # the planted-item key, the three human raters' workbooks (human_validation/),
    │                 # the policy corpus manifest and linearised text
    └── output/       # stored model replies, every derived table the scripts and notebook read, checksums.csv
```

> Note: the full texts of the reviewed papers and of the necessity-premise corpus are copyrighted and not redistributed; their manifests (DOI, title, retrieval route) are in `datasets/output/`. The policy documents are public and are represented by their manifest and the linearised text the models read. Scripts that call LLM APIs are gated behind environment variables and parse their archived replies when the gate is off.

---

## Dependencies

R 4.5.2, Python 3.12.4. Versions below are those used for the release.

| Package | Purpose |
|---|---|
| `sensobol` 1.2.0 | package loading and Sobol' indices |
| `data.table` 1.18.4 | data manipulation |
| `ggplot2` 4.0.3.9000 | figures |
| `cowplot` 1.2.0.9000 | figure assembly |
| `scales` 1.4.0 | axis formatting |
| `here` 1.0.2 | project-relative paths |
| `jsonlite` 2.0.0 | parsing the stored model replies |
| `readxl` 1.4.5 | reading spreadsheets |
| `openxlsx` 4.2.8 | writing spreadsheets |
| `writexl` 1.5.4 | writing spreadsheets |
| `sf` 1.1.0 | the map (Fig. S1) |
| `rnaturalearth` 1.1.0 | the map (Fig. S1) |
| `rnaturalearthdata` 1.0.0 | the map (Fig. S1) |
| `ggrepel` 0.9.6 | map labels |
| `httr2` 1.2.1 | API calls in the gated scripts |
| `stringi` 1.8.7 | text normalisation |
| `digest` 0.6.39 | prompt and file hashes |
| `reticulate` 1.43.0 | reading the saved embeddings (numpy) |
| `xml2` 1.5.2 | reading Elsevier XML full texts |
| `pdftools` 3.6.0 | text extraction from PDFs |
| `rmarkdown` 2.30 | rendering the notebook |
| `knitr` 1.51 | rendering the notebook |
| `benchmarkme` 1.0.8 | session information |

`ggplot2` and `cowplot` are development versions installed from GitHub at `tidyverse/ggplot2@6870419aa6e106c3580c45c81d5b688cb31758bd` and `wilkelab/cowplot@b18d820d3af26b749235c4e4bc18bf416fba88e1`. `install.R` at the repository root installs every R package above at the release version (CRAN archive by version, the two development packages at those commits):

```bash
Rscript install.R
```

Python: `numpy`, `sentence-transformers` (model `sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2`, Hugging Face snapshot `e8f8c211226b894fcb81acc59f3b34ba3efd5f42`; pass that revision to `SentenceTransformer()` to regenerate the embeddings), `pdfplumber` (script 01), PyMuPDF (`fitz`, scripts 26 and 27), `openpyxl`. `requirements.txt` pins their versions:

```bash
python3 -m pip install -r requirements.txt
```

System tools: `pandoc`, a TeX distribution with `pdflatex`, Poppler (`pdftotext`, `pdfinfo`). Set `RETICULATE_PYTHON` to the interpreter that has `numpy`.

---

## How to run

1. Open the `.Rproj` or set the working directory to the repository root.

```bash
cd traditional_irrigation_knowledge
```

2. Reproduce everything that can be reproduced offline (rebuilds the derived tables from the stored replies, compares every output file with `datasets/output/checksums.csv`, renders the notebook; no API key is needed).

```bash
Rscript code/00_reproduce.R
```

3. Or only render the notebook.

```bash
Rscript -e 'rmarkdown::render("code/traditional_irrigation_knowledge.Rmd")'
```

---

## What is and is not reproducible offline

- **Reproducible from the stored replies**: the 1,080-cell grids and Sobol' indices (06–08), degeneracy (09), the compression frontier (10), severing (11), the embedding checks (14–16, from the saved embeddings), the four instrument tests (17–20), the policy promise map (29), the literature extraction aggregates (37), the evidence tables (39–40), the human-rater tables (45), the function-load odds and risk ratios (46) and the shares drawn around the rim of Fig. 1 (48). `00_reproduce.R` runs them and reports any output whose checksum differs from the release.
- **Needs the LLM APIs**: new model calls in 02–04, 17, 19, 20, 24, 28–30, 37 and 41 (set the script's `*_RUN_ENABLED` variable). Fresh calls will not reproduce the archived text; the archived replies reproduce the published numbers.
- **Optional local preparation**: 01 rebuilds `assumptions_corpus.csv` from the included assumption-table PDFs with `pdfplumber` and Poppler; the deposited CSV is byte-identical to its output, so the runner does not call it.
- **Needs files not distributed**: 12–13 (the sentence-transformer weights, downloaded on first use), 27 and 31 (the policy PDFs), 38 and 41 (the review full texts), 32–36 (search and retrieval against Scopus, Dimensions and publishers), 47 (the country-level irrigated areas of ten global datasets, harmonised by the pipeline of [Where irrigation exists is globally contested](https://github.com/arnaldpuy/detectability_crisis); two of the source datasets carry non-commercial terms and two state no licence, so only the 30-row summary `spain_share_europe.csv` is deposited).
- **Starting point of the policy reproduction**: 29 rebuilds the irrigation promise map from the archived recode replies and from the deposited base tables `policy_promise_draws.csv` and `policy_promise_map.csv`. Those base tables were produced by 28 from the raw promise-map replies and the policy PDFs, which are not redistributed; 28 is therefore outside the runner.
- **Archived-input mode**: the runner sets `LITEXTRACT_ARCHIVED_INPUT=true`, so 37 reads the archived text log whether zero, some or all review full texts are present locally. Unset it to re-extract text from a complete local collection.
- **Strict mode**: `REPRODUCE_STRICT=true Rscript code/00_reproduce.R` exits with a non-zero status when any output file differs from `checksums.csv` or is missing. The report also separates files the run rewrote from archived inputs and replies it never touches, so an all-identical result is not read as an all-recomputed one.
- **No analysis API is called, but the run is not network-free**: `sensobol::load_packages()` installs a package missing from the library, and 12–13 download model weights on first use.
- **Known limits recorded in the data**: 75 of the 158 literature texts exceeded the 90,000-character cap of `extract_doc_text_fun` and were coded on their head and tail (`literature_extraction_text.csv`, column `truncated`); two records were coded on their abstract only; in two DeepSeek V4 headline arms, 3 and 7 of the 475 assumptions have four valid draws instead of five because one reply was not parseable (`valid_draw_audit.csv`, which also covers the nine function-attribution arms: a draw there is valid when it names at least one of the twelve functions, the vote denominator is the number of valid draws, "unanimous" means named in every valid draw, 114 of the 4,275 assumption × arm cells have fewer than five valid draws, 107 of them in the three DeepSeek arms, and 4 cells have none and so drop out of that arm); the authoritative literature-screening adjudication is `literature_screening_adjudication.xlsx`. The scripts in `code/historical/` produced the archived headline-arm replies in `datasets/output/arm` and `checkpoint` and depend on prompts and inputs that are not part of the release.

---

## Where each result comes from

| Item | Produced by | Reads |
|---|---|---|
| Fig. 1 | `manuscript/fig-multiverse-radial-tikz.tex`, compiled with `pdflatex`; script 48 writes its data file `manuscript/fig-multiverse-shares.tex` | `sobol_headline_grid.csv` |
| Fig. 2 | notebook chunks `fig2-shares-build`, `fig2-panels-cd`, `fig2-shares-panels`, `fig2-shares` | `sobol_headline_grid.csv`, `sobol_functions_*.csv`, `function_practice_counts_grid.csv`, `human_validation_ratings.csv` (script 45) |
| Fig. 3 | `fig3-bundles-build`, `fig3-bundles` | `practice_function_*.csv`, `sobol_ladder_bundles_*.csv` |
| Results 1 numbers | `numbers-results1`; scripts 06, 14 | `sobol_headline_*.csv`, `embedding_auc.csv` |
| Results 2 numbers | `numbers-results2`; script 10 | `compression_frontier.csv` |
| Results 3 numbers | `numbers-results3`; scripts 08, 09 | `sobol_ladder_bundles_*.csv`, `practice_function_*.csv` |
| Results 4, policy promises | script 29 | `policy_promise_map_irrigation.csv` |
| Results 4, evidence | scripts 37–40, provisional; being redone with human readers | `evidence_strength_matrix.csv` |
| Severing shares | script 11 | `severing_*.csv` |
| Inter-model agreement (Methods) | script 05 | the stored coding replies |
| Fig. S1 | `figS1-map` | site coordinates in the notebook |
| Fig. S2 | `figS2-sobol-build`, `figS2-sobol` | `sobol_headline_*.csv`, `human_validation_shares.csv` (script 45) |
| Human validation (Methods) | script 45 | `datasets/inputs/human_validation/*.xlsx`; writes `human_validation_{ratings,agreement,shares}.csv` |
| Fig. S3 | `sm-build`, `figS3-embedding` | `emb_*.csv`, `embedding_auc.csv` (scripts 12–14) |
| Fig. S4 | `dimensionality-build`, `figS4-dimensions` | `emb_witness_summary.csv`, the coding spectrum (script 15) |
| Fig. S5 | `manuscript/fig-approach-tikz.tex`, compiled with `pdflatex` | `compression_frontier.csv` for its numbers |
| Fig. S6 | `figS6-frontier-build`, `figS6-frontier` | `compression_frontier.csv` |
| Fig. S7 | `figS7-sensory-build`, `figS7-sensory` | `sensory_practice_grid.csv` |
| Text S1.1 instrument tests | scripts 17–20 | `planted_items.xlsx`, the stored replies |
| Text S1.2, S1.3 label-free checks | scripts 12–16 | `emb_assumption.npy`, `assumptions_corpus.csv` |
| Text S1.5 policy promises, Table S4 | scripts 27–31 | the policy manifest and linearised text, the stored replies, `policy_quotes_adjudicated.csv` |
| Text S1.7 literature assembly | scripts 32–38, 42 | `literature_search_raw.csv`, screening votes and adjudication, manifests, `literature_flow.csv` |
| Table S1 necessity quotes | scripts 21–26 | `necessity_*.csv` |

---

## Provenance of the assumption corpus

The 475 assumptions were not collected for this repository. They are the assumption tables released as the supplementary materials of *Traditional irrigation beyond practice: cultural and cognitive dimensions of Spanish irrigators* (one PDF per community, with the interview template, in `datasets/inputs/assumption_tables/`; TABLES_DOI). Those tables are the earliest form of the material that can be released: the interviews were not recorded, the answers were noted verbatim in Spanish or Catalan in a field notebook, the underlying assumption was extracted from each note and translated into English in post-processing by the interviewer, and the field notebooks, which carry identifying detail, are not part of this release (ethical approval: University of Birmingham STEM Committee, ERN_4315-May2026).

Script 01 parses the seven PDFs into `assumptions_corpus.csv` (one row per assumption: `id`, `site`, `domain`, `traditional_knowledge`, `quote_es`, `quote_en`, `assumption`). `id` numbers the rows 1–475 in the order of the tables (Arriate 1–69, Huétor-Tájar 70–126, Dúrcal 127–200, Oria 201–288, Ricote 289–338, Murcia 339–419, Sudanell 420–475) and equals `our_id` in `assumption_list_updated.xlsx`, the workbook the coding scripts read. `id_cite` in that workbook (for example `CKM_01`) is the key under which every stored model reply and every derived table identifies an assumption; `carmen_crosswalk.csv` maps `id_cite` to `our_id` and to the community. The workbook carries no interviewee identifier. The consolidation of the notes into one assumption each, their de-duplication and the checking of the translations happened before the tables were published and are not documented beyond the published Methods; the repository therefore traces every number back to the tables, not to the interviews.

---

## Functions

```r
r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))
```

| Function | Purpose |
|---|---|
| `aggregate_extraction_fun` | Aggregate the N-draw dual-model literature extraction into a stabilised |
| `build_coding_request_fun` | build_coding_request_fun <- function(custom_id, system_prompt, user_text, |
| `coding_schema_fun` | coding_schema_fun <- function(prompt_choice = c("analytical", "subcategory")) { |
| `crossref_lookup_fun` | Queries the Crossref works API once for a free-text bibliographic string and |
| `extract_doc_text_fun` | Extract plain text from a retrieved full text for LLM review-coding. Handles |
| `fetch_batch_results_fun` | fetch_batch_results_fun <- function(batch, |
| `fetch_fulltext_fun` | Retrieve the full text of one DOI through a waterfall of legal routes: |
| `fetch_openalex_abstracts_fun` | Fetch abstracts for a vector of DOIs from OpenAlex (keyless; polite pool via |
| `flatten_coding_record_fun` | flatten_coding_record_fun <- function(txt, choice) { |
| `fleiss_kappa_fun` | Fleiss' kappa for a papers x raters matrix of categorical answers (NA rows dropped). |
| `get_legend_fun` |  |
| `kripp_alpha_fun` | Krippendorff's alpha via the coincidence-matrix method, with a pluggable |
| `load_coded_corpus_fun` | Load the canonical coded corpus exactly as the analysis notebook does: |
| `majority_vote_fun` | majority_vote_fun <- function(dt_long, id_col = "id_cite", fields) { |
| `normalize_practice_fun` | normalize_practice_fun <- function(x) { |
| `parse_policy_findings_fun` | Parse one promise-map model reply (the JSON the code/18 instrument asks for) |
| `policy_promise_instrument_fun` | The promise-map instrument of code/18 (prompt and JSON schema), made callable |
| `poll_batch_fun` | poll_batch_fun <- function(batch_id, api_key = Sys.getenv("ANTHROPIC_API_KEY"), |
| `run_claude_direct_fun` | Run a set of classification requests against the Claude Messages API |
| `run_deepseek_batch_fun` | run_deepseek_batch_fun <- function(items, system_prompt, |
| `run_openai_batch_fun` | run_openai_batch_fun <- function(items, system_prompt, model = "gpt-5.4", |
| `run_openai_direct_fun` | Run a set of classification requests against the OpenAI Chat Completions API |
| `run_three_models_fun` | run_three_models_fun <- function(items, system_prompt, schema, tag, out_dir, |
| `search_dimensions_fun` | Query the Dimensions DSL API and return the hits as a data.table. Logs in |
| `search_scopus_fun` | Query the Scopus Search API and return the hits as a data.table. Sends a |
| `submit_batch_fun` | submit_batch_fun <- function(requests, api_key = Sys.getenv("ANTHROPIC_API_KEY"), |
| `tabulate_multi_fun` | tabulate_multi_fun <- function(dt, col, sep = ";") { |
| `tabulate_single_fun` | tabulate_single_fun <- function(dt, col) { |
| `theme_AP` | theme_AP <- function() { |

---

## Outputs

### Figures

Written by the notebook to `code/traditional_irrigation_knowledge_files/figure-latex/`.

| File | Content |
|---|---|
| `fig2-shares-1.pdf` | Fig. 2, reducible and irreducible shares |
| `fig3-bundles-1.pdf` | Fig. 3, practices and functions |
| `figS1-map-1.pdf` … `figS7-sensory-1.pdf` | Figs. S1–S7 (S5 is built from `manuscript/fig-approach-tikz.tex` with `pdflatex`). Fig. 1 is built the same way from `manuscript/fig-multiverse-radial-tikz.tex` and its data file `fig-multiverse-shares.tex`. These TikZ sources, with those of Figs. S8 and S9, are the only files of `manuscript/` in the repository; the manuscripts themselves are not distributed here |

### Tables

| File | Content |
|---|---|
| `datasets/output/sobol_headline_grid.csv` | the 1,080 specifications of the irreducible share |
| `datasets/output/compression_frontier.csv` | best set of *k* functions per specification |
| `datasets/output/policy_promise_map_irrigation.csv` | functions promised by each policy document |
| `datasets/output/evidence_strength_matrix.csv`, `evidence_tech_function.csv` | the evidence layer (being redone with human readers) |
| `datasets/output/literature_flow.csv` | the literature corpus from raw search to analysed papers |
| `datasets/output/valid_draw_audit.csv` | valid draws per assumption and arm |
| `datasets/output/spain_share_europe.csv` | Spain's share of Europe's irrigated area, by dataset and resolution (Introduction) |

---

## Citation

CITATION TO BE COMPLETED

---

## License

This project is released under the **MIT License** (code) and **CC BY 4.0** (data).

```
MIT License

Copyright (c) 2026 Arnald Puy

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
