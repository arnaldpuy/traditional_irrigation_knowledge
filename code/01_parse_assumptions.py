#!/usr/bin/env python3
"""Parse the seven assumption-table PDFs into one tidy long CSV.

Source : assumption_tables/assumptions_<site>.pdf  (published supplementary tables)
Output : datasets/output/assumptions_corpus.csv

Each output row is one documented assumption with columns:
    id, site, domain, traditional_knowledge, quote_es, quote_en, assumption

The PDFs are LaTeX-generated ruled tables. We use pdfplumber table geometry to
recover row/column structure and word-level extraction to recover inter-word
spacing (extract_tables() alone drops spaces in justified cells). LaTeX accent
glyphs (m´as, a~no, dotless i) are normalised back to proper Unicode.

This is a one-off data-preparation step; the R analysis reads only the CSV.
"""

import csv
import re
import subprocess
import unicodedata
import xml.etree.ElementTree as ET
from pathlib import Path

import pdfplumber

ROOT = Path(__file__).resolve().parents[1]
SRC = ROOT / "datasets" / "inputs" / "assumption_tables"
OUT = ROOT / "datasets" / "output" / "assumptions_corpus.csv"

# Site files in the order listed in the supplementary README.
SITES = ["arriate", "huetortajar", "durcal", "oria", "ricote", "murcia", "sudanell"]

# The four knowledge-domain section headers, as printed in the PDFs.
DOMAINS = [
    "Crop knowledge and management",
    "Irrigation practices and communal water management",
    "Soil and moisture management",
    "Weather and climate conditions",
]

HEADER_CELLS = {"traditional knowledge", "underlying assumption"}


def fix_accents(s: str) -> str:
    """Normalise text taken from poppler word boxes.

    Poppler (pdftotext) emits accented letters with the combining mark already
    correctly positioned after its base, so we only need to fold the dotless
    i/j it uses as the base of í/ì and apply NFC composition.
    """

    if not s:
        return s
    s = s.replace("ı", "i").replace("ȷ", "j")
    return unicodedata.normalize("NFC", s)


def clean(s: str) -> str:
    if not s:
        return ""
    s = s.replace("\n", " ")
    s = re.sub(r"-\s+", "", s) if False else s  # de-hyphenation handled per-word
    s = re.sub(r"\s+", " ", s).strip()
    return fix_accents(s)


def join_words(words):
    """Join extracted words into a string, de-hyphenating line breaks."""

    out = []
    for w in words:
        t = w["text"]
        out.append(t)
    text = " ".join(out)
    # de-hyphenate: "cul- tivos" -> "cultivos"
    text = re.sub(r"(\w)-\s+(\w)", r"\1\2", text)
    return clean(text)


def split_es_en(tk: str):
    """Split a traditional-knowledge cell into Spanish quote and English gloss.

    Convention in the tables: a Spanish phrase in typographic quotes ”...”
    (optionally prefixed by "Saying-") followed by its English translation.
    Returns (quote_es, quote_en); falls back to ('', tk) when no quote found.
    """

    m = re.search(r"[”“\"]([^”“\"]*)[”“\"]\s*(.*)", tk)
    if not m:
        return "", tk
    es = m.group(1).strip()
    en = m.group(2).strip()
    # the closing quote often precedes the sentence-final period, which then
    # leaks to the front of the gloss ("benefician". For ...) -> tidy both.
    if en[:1] in ".,;:" and not es.endswith((".", "!", "?")):
        es = es + en[0]
    en = en.lstrip(".,;: ").strip()
    return es, en


def poppler_words(path: Path):
    """Return per-page lists of words with correct Unicode from poppler.

    Each word is a dict {text, x0, x1, top, bottom} in the same coordinate
    frame as pdfplumber (612x792 PDF points, top-left origin).
    """

    xml = subprocess.run(
        ["pdftotext", "-bbox-layout", str(path), "-"],
        capture_output=True, text=True, check=True,
    ).stdout
    root = ET.fromstring(xml)
    ns = {"x": root.tag.split("}")[0].strip("{")} if "}" in root.tag else {}
    pages = []
    page_tag = f"{{{ns['x']}}}page" if ns else "page"
    word_tag = f"{{{ns['x']}}}word" if ns else "word"
    for page in root.iter(page_tag):
        words = []
        for w in page.iter(word_tag):
            words.append({
                "text": fix_accents((w.text or "").strip()),
                "x0": float(w.attrib["xMin"]), "x1": float(w.attrib["xMax"]),
                "top": float(w.attrib["yMin"]), "bottom": float(w.attrib["yMax"]),
            })
        pages.append([w for w in words if w["text"]])
    return pages


def words_in_bbox(words, bbox):
    """Words whose centre falls within a table-cell bbox (x0, top, x1, bottom)."""

    x0, top, x1, bottom = bbox
    sel = [
        w for w in words
        if x0 <= (w["x0"] + w["x1"]) / 2 <= x1
        and top <= (w["top"] + w["bottom"]) / 2 <= bottom
    ]
    return sorted(sel, key=lambda w: (round(w["top"]), w["x0"]))


def parse_pdf(path: Path, site: str):
    rows = []
    cur_domain = None  # persists across pages until a new header appears
    ppages = poppler_words(path)
    with pdfplumber.open(path) as pdf:
        for i, page in enumerate(pdf.pages):
            pw = ppages[i] if i < len(ppages) else []
            for table in page.find_tables():
                cur_domain = domain_above(pw, table.bbox[1]) or cur_domain
                for row in table.rows:
                    cells = [c for c in row.cells if c is not None]
                    if len(cells) < 2:
                        continue
                    tk = join_words(words_in_bbox(pw, cells[0]))
                    ua = join_words(words_in_bbox(pw, cells[-1]))
                    if tk.lower().strip() in HEADER_CELLS or not tk:
                        continue
                    if cur_domain:
                        rows.append((site, cur_domain, tk, ua))
    return rows


def domain_above(words, y):
    """Return the domain header whose printed position is just above y."""

    best = None
    best_top = -1
    text_by_top = {}
    for w in words:
        text_by_top.setdefault(round(w["top"]), []).append((w["x0"], w["text"]))
    for top, ws in text_by_top.items():
        line = clean(" ".join(t for _, t in sorted(ws)))
        for d in DOMAINS:
            if d.lower() in line.lower() and top < y and top > best_top:
                best = d
                best_top = top
    return best


def main():
    all_rows = []
    for site in SITES:
        path = SRC / f"assumptions_{site}.pdf"
        rows = parse_pdf(path, site)
        print(f"{site:14s} {len(rows):4d} assumptions")
        all_rows.extend(rows)

    OUT.parent.mkdir(parents=True, exist_ok=True)
    with OUT.open("w", newline="", encoding="utf-8") as f:
        wr = csv.writer(f)
        wr.writerow(
            ["id", "site", "domain", "traditional_knowledge", "quote_es",
             "quote_en", "assumption"]
        )
        for i, (site, domain, tk, ua) in enumerate(all_rows, 1):
            es, en = split_es_en(tk)
            wr.writerow([i, site, domain, tk, es, en, ua])
    print(f"\nTotal: {len(all_rows)} assumptions -> {OUT.relative_to(ROOT)}")


if __name__ == "__main__":
    main()
