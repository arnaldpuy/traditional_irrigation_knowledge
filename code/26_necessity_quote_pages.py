#!/usr/bin/env python3
"""Locate the PDF page of every verified necessity-premise quote.

Reads datasets/output/necessity_quotes.csv (code/26), and for each verified
quote searches its source PDF page by page for the quote's leading words
(whitespace-normalised, case-insensitive). Writes the page number back as a
`page` column (necessity_quotes_paged.csv), so each SM table row can cite
document AND page. Quotes whose source is an Elsevier XML full text (no
pagination) or that cannot be located get page = NA.

Sources searched, mirroring the code/26 roster:
  datasets/inputs/necessity_corpus/fulltexts/<doc_id>.pdf     (academic)
  datasets/inputs/necessity_corpus/institutional/<doc_id>.pdf (institutional)
  datasets/inputs/policy_corpus/<doc_id>.pdf                  (policy)
"""

import csv
import pathlib
import re
import sys

import fitz  # PyMuPDF

ROOT = pathlib.Path(".")
SRC_DIRS = [
    ROOT / "datasets/inputs/necessity_corpus/fulltexts",
    ROOT / "datasets/inputs/necessity_corpus/institutional",
    ROOT / "datasets/inputs/policy_corpus",
]
IN_CSV = ROOT / "datasets/output/necessity_quotes.csv"
OUT_CSV = ROOT / "datasets/output/necessity_quotes_paged.csv"

WS = re.compile(r"\s+")


def norm(s: str) -> str:
    return WS.sub(" ", s.lower().replace("­", "")).strip()


def find_pdf(doc_id: str) -> pathlib.Path | None:
    for d in SRC_DIRS:
        p = d / f"{doc_id}.pdf"
        if p.exists():
            return p
    return None


def page_of(pdf: pathlib.Path, quote: str) -> int | None:
    """1-based page whose normalised text contains the quote's leading words.

    Uses the first ~12 words: long enough to be unique, short enough to be
    robust to a quote spanning a page break (the lead words sit on one page).
    """
    lead = " ".join(norm(quote).split(" ")[:12])
    if len(lead) < 15:
        return None
    doc = fitz.open(pdf)
    for i, page in enumerate(doc):
        if lead in norm(page.get_text()):
            return i + 1
    return None


def main() -> None:
    if not IN_CSV.exists():
        sys.exit(f"{IN_CSV} not found - run code/26 first")
    with open(IN_CSV, encoding="utf-8") as fh:
        rows = list(csv.DictReader(fh))
    cache: dict[str, pathlib.Path | None] = {}
    n_found = n_tried = 0
    for r in rows:
        r["page"] = ""
        if r.get("verified") not in ("TRUE", "True", "true"):
            continue
        doc_id = r["doc_id"]
        if doc_id not in cache:
            cache[doc_id] = find_pdf(doc_id)
        pdf = cache[doc_id]
        if pdf is None:
            continue  # XML full text or missing binary: page stays NA
        n_tried += 1
        page = page_of(pdf, r["verbatim_quote"])
        if page is not None:
            r["page"] = str(page)
            n_found += 1
    with open(OUT_CSV, "w", encoding="utf-8", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)
    print(f"Paged {n_found}/{n_tried} located quotes -> {OUT_CSV}")


if __name__ == "__main__":
    main()
