# Prepare the compact MHC tutorial

The installed plotting bundle is in `inst/extdata/mhc_haplotypes/`.
Its README documents data sources, pseudogene filtering, native coordinates,
source-status limitations and direct-alignment settings.

```sh
python3 data-raw/mhc_haplotypes/build-mhc-example.py
Rscript data-raw/mhc_haplotypes/render-mhc-example.R
```

`source/` retains bounded historical input tables for Siamang H1, Gorilla H1
and Human CHM13, with original table hashes and the pinned repository commit.
`excluded-models.tsv` records source models excluded as pseudogenes or because
a curated MHC record supersedes them. Representative-transcript selection
then chooses the longest total exonic model at each overlapping named locus.

The builder downloads only the selected 3.63 Mb slices from the assembly
2bit sources and computes the two direct minimap2 alignments. Its default
cache is `~/Library/Caches/ggexon/mhc_haplotypes`; use `--cache-dir` to choose
another location. The raw regional FASTAs and full PAFs stay in that cache.

The renderer executes the vignette code against the current package, verifies
the three selected individuals, gene counts, native panel limits, five-panel
layout and two alignment collections, and writes figures to `figures/`.
The second and final stages require `geom_gene_lollipop()` with all 58
functional MHC heads. Their exported layout tables verify native anchors,
strand direction, four shared rows, label fit and panel bounds in PNG and PDF.
The final PNG also updates the bundled website/homepage preview.
