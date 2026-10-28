#!/usr/bin/env python3
"""Column-aware linearisation of PDF corpora for verbatim-quote extraction.

The default poppler / pdftools reading order sorts text by vertical position
across the full page width, which interleaves the two columns of multi-column
policy documents and splices unrelated sentences together. Here we read each page
as coordinate-tagged text blocks (PyMuPDF), assign every block to a column from
its horizontal position, and emit the text one column at a time, top to bottom, so
each sentence stays contiguous. Single-column pages fall through unchanged (their
blocks span the width and sort by vertical position as before).

Usage: python3 code/22_linearize_policy_pdfs.py [input_dir]
  input_dir defaults to datasets/inputs/policy_corpus (the original use); pass
  e.g. datasets/inputs/necessity_corpus/institutional to linearise that corpus.

Output: <input_dir>/linearized/<doc_id>.txt — clean reading-order text that
code/21 and code/26 read in place of the poppler extraction for quoting.
"""

import pathlib
import sys
import fitz  # PyMuPDF

POL = pathlib.Path(sys.argv[1] if len(sys.argv) > 1
                   else "datasets/inputs/policy_corpus")
OUT = POL / "linearized"
OUT.mkdir(exist_ok=True)


def linearize_page(page):
    blocks = [b for b in page.get_text("blocks") if b[6] == 0 and b[4].strip()]
    if not blocks:
        return ""
    width = page.rect.width
    mid = width / 2

    def column(b):
        # A block wider than 65% of the page is full-width (heading, running
        # text in a single-column page): keep it in the left stream so it stays
        # in vertical order. Otherwise assign by the block's horizontal centre.
        if (b[2] - b[0]) > 0.65 * width:
            return 0
        return 0 if (b[0] + b[2]) / 2 < mid else 1

    # Sort by column, then by a coarsely-binned vertical position so blocks that
    # share a line are not reordered by sub-point differences.
    blocks.sort(key=lambda b: (column(b), round(b[1] / 6)))
    return "\n".join(b[4].strip() for b in blocks)


def main():
    pdfs = sorted(POL.glob("*.pdf"))
    print(f"Linearising {len(pdfs)} PDFs -> {OUT}")
    for pdf in pdfs:
        doc = fitz.open(pdf)
        text = "\n".join(linearize_page(p) for p in doc)
        dest = OUT / (pdf.stem + ".txt")
        dest.write_text(text, encoding="utf-8")
        print(f"  {pdf.stem:<45} {len(doc):>4} pages  {len(text):>8} chars")


if __name__ == "__main__":
    main()
