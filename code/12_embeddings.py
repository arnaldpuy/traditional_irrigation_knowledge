#!/usr/bin/env python3
"""Compute multilingual sentence embeddings for the assumption corpus.

Embeds the distilled English `assumption` (primary, uniform language) and the
verbatim `quote_es` (Spanish/Catalan) with a multilingual model so both share a
cross-lingual space. Outputs:
    datasets/output/emb_assumption.npy   (n x d, row order = ids sorted asc)
    datasets/output/emb_quote_es.npy
    datasets/output/emb_ids.csv          (id order index)

Enables: semantic distance to an efficiency lexicon, contradiction map,
unsupervised clustering vs the four imposed domains.
"""

import csv
from pathlib import Path

import numpy as np
from sentence_transformers import SentenceTransformer

ROOT = Path(__file__).resolve().parents[1]
CSV = ROOT / "datasets" / "output" / "assumptions_corpus.csv"
OUT = ROOT / "datasets" / "output"
MODEL = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2"


def main():
    rows = sorted(csv.DictReader(CSV.open(encoding="utf-8")),
                  key=lambda r: int(r["id"]))
    ids = [int(r["id"]) for r in rows]
    model = SentenceTransformer(MODEL)

    for field, fname in [("assumption", "emb_assumption.npy"),
                         ("quote_es", "emb_quote_es.npy")]:
        texts = [r[field] for r in rows]
        emb = model.encode(texts, normalize_embeddings=True,
                           show_progress_bar=False, batch_size=64)
        np.save(OUT / fname, emb.astype(np.float32))
        print(f"{field:12s} -> {fname}  shape={emb.shape}")

    with (OUT / "emb_ids.csv").open("w", newline="", encoding="utf-8") as f:
        wr = csv.writer(f)
        wr.writerow(["row_index", "id"])
        for i, _id in enumerate(ids):
            wr.writerow([i, _id])
    print(f"ids -> emb_ids.csv  n={len(ids)}")


if __name__ == "__main__":
    main()
