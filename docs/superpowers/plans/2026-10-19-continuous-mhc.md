# Continuous MHC Integration Implementation Plan

> **For agentic workers:** Use executing-plans or subagent-driven-development to implement the checked tasks, verifying each output.

**Goal:** Combine ED3a gene identities/status with ED3c full-region alignment on continuous native-coordinate tracks for human and both haplotypes of six apes.

**Architecture:** Source-defined orthologous windows remain in native base-pair distances and use a common horizontal scale. Browser nucleotide chain blocks supply ribbons; per-genome genes/exons and regional annotations share one annotation panel. Published discordant intervals are mapped from their documented coordinate system with explicit provenance.

**Tech Stack:** Python requests/pyBigWig for bounded source queries and validation; R ggplot2/ggexon for vector PDF and PNG previews.

## Global constraints

- Reuse `/Users/liudongyao/.codex/worktrees/ape-integrated-annotations/ggexon`; leave the primary checkout intact.
- Continuous MHC I, intervening region and MHC II; no independent class-coordinate resets or per-haplotype length normalization.
- All available gene models, MHC ortholog colors from ED3a source, pseudogenes grey with asterisk; non-MHC genes grey.
- Only nucleotide links get separate panels. Discordant regions and other annotations sit below genes. No topology/tree panels.
- Exact source coordinates only; approximate figure-derived intervals must be marked approximate, never silently precise.
- Do not download whole genomes or fabricate exon models, alignment blocks or missing annotations.

### Task 1: Source contracts and windows

Files: `source_metadata/mhc_continuous/`, `fetch_mhc_continuous.py`, `verify_mhc_continuous.py`.

- [x] Resolve exact ED3c bounds and meaning of 5.31Mb; locate discordant intervals.
- [x] Verify gene/exon source coverage and ortholog RGB colors for all13tracks.
- [x] Extract full human-region chain blocks and derive orthologous native windows; record chromosome orientation and boundaries.
- [x] Check equal native lengths under translation, strand conversions, clipped block endpoints and source hashes.

### Task 2: Continuous data preparation

Files: `results/mhc_continuous/`, `fetch_mhc_continuous_genes.py`.

- [x] Query all available gene models in windows; retain source transcripts/exons and curated MHC status separately.
- [x] Keep original raw coordinates and normalize only display offsets/orientation.
- [x] Retain exact chain-supported ribbons. Distinguish projected annotations from directly measured ones.
- [x] Make missing sources explicit rather than converting missing calls to zeros.

### Task 3: Plot and verification

Files: `render_mhc_continuous.R`, `test-mhc-continuous.R`, `README.md`.

- [x] Verify shared-bp geometry and panel routing before export.
- [x] Render one complete continuous ggexon chain with13annotationpanels and available supported links.
- [x] Inspect density, labels, pseudo markings, all annotations below genes and true length differences.
- [x] Save vector PDF, PNG preview, plotted data and QC tables; document data limitations and source definitions.

Implementation notes: exact original ED3c cuts could not be recovered. The documented source-union window is CHM13chr6 BED0[28669878,33979214),5.309336Mb. Six ape haplotypes lack published general exon models and are explicitly marked curated-MHC-only. See results/mhc_continuous/README.md and figures/verification.json.
