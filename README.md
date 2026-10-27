<!-- [![DOI](https://zenodo.org/badge/DOI/XXXX.svg)](https://doi.org/XXXX) -->

# Irrigation modernisation is blind to most traditional knowledge

[Arnald Puy](https://www.arnaldpuy.com)\*, AUTHOR LIST TO BE COMPLETED

\*Corresponding author.

R and Python code accompanying the paper *Irrigation modernisation is blind to most traditional knowledge*, whose abstract is the following:

## Abstract

*The modernization of traditional irrigation systems by means of technologies designed to minimize water use and maximize crop yield is promoted worldwide to improve global water and food security. However, we do not know how much traditional knowledge gets lost when this efficiency-based irrigation frameworks are implemented. Here we address this gap by analyzing 475 assumptions (fragments of tacit knowledge) extracted from 110 interviews across seven Spanish irrigation systems, where such knowledge has accumulated for over a millennium, while varying our analysis to test whether the result survives defensible analytical choices. We find that at least 60% of the assumptions contain knowledge that is irreducible to a water-in/yield-out metric and that they serve functions spanning production, hydrology, ecology and social organization. Although many irrigation technologies promise to serve some of these functions, the strength of the evidence that they actually do is very limited. Our results indicate that efficiency is not a sufficient criterion for modernizing irrigation systems: evaluating improvement requires accounting for the knowledge and functions that technological substitution may eliminate.*

## Contents

The analysis combines 475 assumptions extracted from 110 interviews in seven traditional irrigation communities in Spain, a Large Language Model (LLM) coding scheme, a global uncertainty and sensitivity analysis over 1,080 analytical specifications, and a review of the policy and scientific literature on irrigation modernization.

- [Background](#background)
- [Repository structure](#repository-structure)
- [Dependencies](#dependencies)
- [How to run](#how-to-run)
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
│   ├── traditional_irrigation_knowledge.Rmd   # notebook reproducing every figure and number of the paper and the SM
│   ├── 01_coding_run.R … 10_severing.R        # LLM coding of the assumptions and its uncertainty (gated scripts need API keys)
│   ├── 11_embeddings.py … 15_coding_collapse.R # label-free checks
│   ├── 16_planted_items.R … 19_language_conditions.R   # tests of the instrument
│   ├── 20_necessity_search.R … 25_necessity_quote_pages.py   # the necessity premise (Table S1)
│   ├── 26_policy_linearize.py … 30_quote_locations.R   # modernization policies (Text S1.5, Table S4)
│   └── 31_literature_search.R … 38_link_extraction.R   # the literature on the technologies
├── functions/        # helper functions (one *_fun per file)
├── prompts/          # the LLM instruments behind every reported number
├── datasets/
│   ├── inputs/       # the 475 assumptions, the planted-item key, the policy corpus manifest and text
│   └── output/       # stored model replies and every derived file the notebook reads
└── (the notebook writes the figures to code/traditional_irrigation_knowledge_files/figure-latex/)
```

> Note: the full texts of the reviewed papers and of the necessity-premise corpus are not redistributed; their manifests (DOI, title, retrieval route) are in `datasets/output/`. Scripts that call LLM APIs are gated behind environment variables and are not needed to reproduce the results, which rest on the stored replies.

---

## Dependencies

| Package | Purpose |
|---|---|
| `sensobol` | package loading and Sobol' indices |
| `data.table` | data manipulation |
| `ggplot2`, `cowplot`, `scales` | figures |
| `here` | project-relative paths |
| `jsonlite` | parsing the stored model replies |
| `readxl`, `openxlsx` | spreadsheets |
| `sf`, `rnaturalearth`, `rnaturalearthdata`, `ggrepel` | the map (Fig. S1) |
| `httr2` | API calls in the gated scripts |
| `stringi` | text normalization |
| `benchmarkme` | session information |

R ≥ 4.3. Python ≥ 3.10 with `sentence-transformers` and `numpy` for the embeddings.

---

## How to run

1. Set the working directory to the repository root (open the `.Rproj`).

```bash
cd traditional_irrigation_knowledge
```

2. Knit the notebook.

```bash
Rscript -e 'rmarkdown::render("code/traditional_irrigation_knowledge.Rmd")'
```

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
| `extract_doc_text_fun` | Extract plain text from a retrieved full text for LLM review-coding. Handles |
| `fetch_batch_results_fun` | fetch_batch_results_fun <- function(batch, |
| `fetch_fulltext_fun` | Retrieve the full text of one DOI through a waterfall of legal routes: |
| `fetch_openalex_abstracts_fun` | Fetch abstracts for a vector of DOIs from OpenAlex (keyless; polite pool via |
| `flatten_coding_record_fun` | flatten_coding_record_fun <- function(txt, choice) { |
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

| File | Content |
|---|---|
| `fig1-shares-1.pdf` | Fig. 1, reducible and irreducible shares |
| `fig2-bundles-1.pdf` | Fig. 2, practices and functions |
| `figS1-map-1.pdf` … `figS7-sensory-1.pdf` | Figs. S1–S7 |

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
