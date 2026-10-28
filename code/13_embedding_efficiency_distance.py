#!/usr/bin/env python3
"""Label-free semantic witness for the reducible/irreducible split.

Uses the multilingual sentence embeddings of the 475 assumptions (built by
02_embeddings.py, never exposed to the 12 function labels or the removal test)
to test whether the same efficiency/non-efficiency structure recovered by the
coding is present in an independent semantic representation.

Steps:
  1. Load emb_assumption.npy (n x 384, L2-normalised) and emb_ids.csv.
  2. Embed an efficiency lexicon with the SAME model. The lexicon is anchored on
     the paper's own two definitions (irrigation efficiency = fraction of
     withdrawn water used by the crop; water productivity = crop yield per unit
     of water) plus the core water/yield variables.
  3. Per assumption, cosine similarity to the lexicon (centroid and max).
  4. Intrinsic dimensionality of the embedding cloud (participation ratio,
     Shannon effective rank, PCs for 90% variance) -- the label-free echo of the
     assumption x variable effective rank.
  5. A 2-D PCA projection of the assumptions and of the lexicon centroid, for
     the figure.

Outputs (datasets/output/):
  emb_efficiency_distance.csv   id, sim_centroid, sim_max, pc1, pc2
  emb_witness_summary.csv       one-row summary of the dimensionality stats
  emb_projection_meta.csv       lexicon-centroid coordinates in the PCA space
  emb_scree.csv                 cumulative explained variance over leading PCs
"""

import csv
from pathlib import Path

import numpy as np
from sentence_transformers import SentenceTransformer

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "datasets" / "output"
MODEL = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2"

# Efficiency lexicon: the water-in / yield-out vocabulary, anchored on the
# paper's definitions of irrigation efficiency and water productivity.
EFFICIENCY_LEXICON = [
    "irrigation efficiency",
    "the fraction of withdrawn water used by the crop",
    "water productivity",
    "crop yield per unit of water consumed",
    "water use efficiency",
    "how much water to apply to the crop",
    "the volume of water applied per hectare",
    "crop water requirement",
    "the amount of water the crop needs",
    "saving water while maintaining yield",
]


def eff_ranks(sv):
    sv = sv[sv > 1e-10]
    lam = sv ** 2
    p = sv / sv.sum()
    pr = (lam.sum() ** 2) / (lam ** 2).sum()
    ent = np.exp(-(p * np.log(p)).sum())
    return pr, ent


def main():
    emb = np.load(OUT / "emb_assumption.npy").astype(np.float64)  # n x d, L2-norm
    ids = [int(r["id"]) for r in csv.DictReader((OUT / "emb_ids.csv").open())]
    assert emb.shape[0] == len(ids)

    model = SentenceTransformer(MODEL)
    lex = model.encode(EFFICIENCY_LEXICON, normalize_embeddings=True).astype(np.float64)
    lex_centroid = lex.mean(axis=0)
    lex_centroid /= np.linalg.norm(lex_centroid)

    sim_centroid = emb @ lex_centroid            # cosine (emb is L2-normalised)
    sim_max = (emb @ lex.T).max(axis=1)

    # Intrinsic dimensionality of the assumption cloud (centred PCA).
    X = emb - emb.mean(axis=0)
    sv = np.linalg.svd(X, compute_uv=False)
    pr, ent = eff_ranks(sv)
    var = (sv ** 2) / (sv ** 2).sum()
    cum = np.cumsum(var)
    pc90 = int(np.searchsorted(cum, 0.90) + 1)

    # 2-D PCA projection (assumptions + lexicon centroid in the same basis).
    U, S, Vt = np.linalg.svd(X, full_matrices=False)
    scores = U[:, :2] * S[:2]
    lex_score = (lex_centroid - emb.mean(axis=0)) @ Vt[:2].T

    with (OUT / "emb_efficiency_distance.csv").open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["id", "sim_centroid", "sim_max", "pc1", "pc2"])
        for i, _id in enumerate(ids):
            w.writerow([_id, f"{sim_centroid[i]:.6f}", f"{sim_max[i]:.6f}",
                        f"{scores[i, 0]:.6f}", f"{scores[i, 1]:.6f}"])

    with (OUT / "emb_witness_summary.csv").open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["n", "dim", "participation_ratio", "shannon_eff_rank",
                    "pcs_for_90pct_variance"])
        w.writerow([emb.shape[0], emb.shape[1], f"{pr:.3f}", f"{ent:.3f}", pc90])

    with (OUT / "emb_projection_meta.csv").open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["object", "pc1", "pc2"])
        w.writerow(["efficiency_lexicon", f"{lex_score[0]:.6f}", f"{lex_score[1]:.6f}"])

    # Scree: cumulative explained variance over the leading components.
    with (OUT / "emb_scree.csv").open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["pc", "cum_var"])
        for k in range(min(150, len(var))):
            w.writerow([k + 1, f"{cum[k]:.6f}"])

    print(f"n={emb.shape[0]} dim={emb.shape[1]}")
    print(f"participation ratio {pr:.1f} | Shannon eff rank {ent:.1f} | "
          f"PCs for 90% variance {pc90}")
    print(f"sim_centroid range [{sim_centroid.min():.3f}, {sim_centroid.max():.3f}] "
          f"mean {sim_centroid.mean():.3f}")


if __name__ == "__main__":
    main()
