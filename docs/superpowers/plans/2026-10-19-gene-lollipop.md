# Gene Lollipop Implementation Plan

**Goal:** Add the approved reusable gene-lollipop geom and replace the MHC dot/label display with readable gene-arrow heads.

**Architecture:** A pure millimetre layout solver chooses the smallest feasible row count with ordered bounded horizontal shifts. A deferred grid grob measures all layer labels on the real export device, uses a common physical head size and draws stems, arrows and internal text in each panel viewport. Syn resolution uses true gene spans; MHC integration retains native coordinate and source-QC contracts.

**Tech stack:** R ggplot2/ggproto/grid, existing LayerSyn and facet_genomics; testthat and the existing Python MHC verifier.

- [x] Write failing physical-layout, direction, rendering and Syn/facet contract tests.
- [x] Implement `R/gene-lollipop-layout.R` and `R/geom-gene-lollipop.R`, add package collation/exports and Rd documentation.
- [x] Run focused tests and relevant existing gene/facet tests; repair failures without changing unrelated behavior.
- [x] Update `render_mhc_continuous.R`, its verifier and notes for MHC-only lollipop heads, native grey context and one VR lane.
- [x] Inspect exports and the final physical-layout callback records; independently review code and address material findings.
- [x] Deliver the new public geom and updated vector PDF/preview from the existing isolated worktree.

Verification: 281 assertions across gene-lollipop, genebox, genetag and facet-label tests passed without warnings. Independent review findings for inherited mappings and outline bounds were fixed and rechecked. The final 180 x 560 mm PDF and PNG pass the source/CIGAR/native-coordinate checks and physical-layout audit for all 285 heads across 13 haplotypes, with 624 ribbons and 1359 native context arrows.
