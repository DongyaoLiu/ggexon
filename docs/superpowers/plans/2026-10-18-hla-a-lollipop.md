# Dense HLA-A Lollipop Implementation Plan

> Executed inline after the user approved the revised design and all eligible named-allele counts.

**Goal:** Draw a polished dense summary of HLA-A amino-acid alternatives.

**Architecture:** A pinned-source Python builder validates aligned coordinates and writes compressed original records. An R example loader computes state/substitution counts. A shared R layout helper applies bounded local fanning and log10(count + 1) heights for the overview and close-up.

**Tech stack:** Python standard library, Biostrings, ggexon, ggplot2, knitr, pkgdown.

## Constraints

- Count all 5,998 eligible named alleles; do not deduplicate their 3,188 mature proteins.
- Use mature coordinates relative to A*02:01:01:01, preserve all 1,212 substitutions, and retain true stem anchors.
- Use the official pinned alignment to verify coordinate correspondence.
- Preserve original source records and data terms; do not change exported package APIs.

## Completed tasks

- [x] Expand `build-hla-a-demo.py` to all eligible names, validate alignment gap patterns, and write deterministic gzip records and a complete manifest.
- [x] Update `load-hla-a-example.R` to return all counts, chemical groups, three example carriers per state, and distinct mature protein totals.
- [x] Implement `hla_a_lollipop_layout()` and the redesigned plotting helper with symmetric bounded fanning, chemical-group colours, log heights, labelled protein regions, and a full-width header.
- [x] Update independent verification of every substitution, reference state totals, all rendered heads, transformed heights, local offsets, and stem endpoints.
- [x] Rewrite the tutorial and source README with selection rules, named-allele weighting, exact transformation, and all five residue-156 alternatives.
- [x] Render both PNG/PDF figures and summary tables; build the pkgdown article from the current source package.
- [x] Visually inspect the new overview and focus; verify byte-reproducible rebuilds and all 5,998 unchanged records and source SHA-256 values.

## Verification commands

```sh
python3 data-raw/hla_a_lollipop/build-hla-a-demo.py
Rscript data-raw/hla_a_lollipop/verify-hla-a-demo.R
Rscript data-raw/hla_a_lollipop/render-hla-a-figure.R
Rscript -e 'pkgload::load_all(quiet=TRUE); pkgdown::build_article("hla-a-lollipop-demo", new_process=FALSE, quiet=TRUE)'
git diff --check
```

All completed successfully. Pandoc reports an existing deprecated highlight-style option, without preventing article rendering. Existing unrelated untracked work remains intact. The subsequent user request authorizes publication of this example to pkgdown.

## Gene projection and publication

- [x] Bundle the exact release-3.66.0 HLA00005 reference snapshot and derive genomic FASTA, GFF3, and exon/intron table.
- [x] Project all 1,212 substitutions through the reference CDS, preserving split-codon fragments and exonic anchors.
- [x] Draw a dense genomic model with eight numbered coding exons, seven introns, three-fold compression, and correct base-edge geometry.
- [x] Verify every codon against an independently enumerated CDS; assert native exon widths and exact three-fold gap compression.
- [x] Update the tutorial, homepage showcase, renderer, and source documentation.
- [ ] Build public artifacts from an isolated snapshot containing only intended tracked inputs; verify package inclusion and installed execution.
- [ ] Publish the reviewed source and generated site while preserving remote updates; verify public article, navigation, and image.
