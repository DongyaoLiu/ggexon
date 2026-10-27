# Compact MHC example: human, gorilla and siamang

This example reduces the approved 13-track comparison to three species and four
tracks. The full native MHC windows, shared base-pair scale, six shared head rows,
ortholog colors, pseudogene markings, small native MHC arrows, grey context genes
and lower annotation lanes are retained. Width is 180 mm; height is 267.5 mm,
compared with 818.8 mm for the full comparison (67% less height).

| Species | Retained track(s) | Curated MHC heads | Reason |
|---|---|---:|---|
| Human | CHM13 (`HSAh1`) | 37 | Human reference; the input contains no second human haplotype. |
| Gorilla | h1 and h2 (`GGOh1`, `GGOh2`) | 22 / 20 | h1 includes OKO, DRB5 and DRB1 where these heads are not present in the h2 curated set; h2 includes A; DQA2 is functional in h1 and marked pseudogene in h2. |
| Siamang | h1 (`SSYh1`) | 18 | h1 and h2 have identical ordered MHC head identities, copy counts, functional-status calls and strand directions. h1 is the representative; the sequences and distances are not asserted identical. |

The gorilla differences are source annotation differences in the sampled
assemblies. Empty head slots alone are not evidence of biological gene absence,
and the example does not establish species-wide or population frequencies.
`candidate-haplotype-audit.tsv` records the same comparison across all seven
species. `haplotype-head-differences.tsv` lists excess annotated copies by status
and strand. Native coordinate shifts alone are not a reason to repeat a haplotype.

## Reproduce

From the repository root, with minimap2, the R package dependencies and the
previously acquired regional FASTA caches available:

```sh
python3 data-raw/ape_paper/prepare_mhc_compact.py
Rscript data-raw/ape_paper/render_mhc_compact.R
python3 data-raw/ape_paper/verify_mhc_compact.py
```

The DNA audit requires NumPy. In the current local environment the Python with
NumPy is `/Users/liudongyao/miniconda3/bin/python`.
The render entry point first calls `read_mhc_data()` for all 13 tracks, then
`subset_mhc_data()` for the selected tracks, and finally `render_mhc_data()`.
The whole object includes both direct bridge alignments before selection;
all annotation tables and the attached `SynSpecies` are subset together. All style
parameters remain in the full renderer's top parameter block. No package API
changes are needed.

The source FASTA caches are under `results/mhc_continuous/` and are intentionally
ignored by Git. They can be reacquired with `fetch_mhc_continuous.py` if absent.
The compact preparation script reuses the exact direct gorilla h2/h1 PAF and
calculates two new **direct** alignments: siamang h1 → gorilla h2 and gorilla h1 →
human CHM13. It does not connect nonadjacent species by transitive or invented
ribbons. `selection.json` records commands and source FASTA/PAF SHA256 hashes.
Ribbons use the same minimap2 settings, 70% alignment-identity threshold, 1-kb
large-indel split and 500-bp minimum ribbon span as the full comparison.

Deliverables are `mhc_three_species.pdf` (vector) and `mhc_three_species.png`.
`figures/` contains the detailed geometry, annotation and rendering audit tables.
The figure has 97 curated MHC heads/native arrows, 860 grey context arrows,
three nucleotide-link panels and 252 ribbons. Gorilla h2 context remains labelled
as same-species Liftoff projection; other retained context tracks use released
annotations. VR bars retain the published human intervals and chain-supported
ape projections. The original full figure is preserved unchanged.
