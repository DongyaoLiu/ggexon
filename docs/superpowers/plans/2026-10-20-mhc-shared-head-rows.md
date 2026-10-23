# Shared MHC Head Rows Implementation Plan

Goal: implement the requested shared MHC head rows in the existing isolated worktree.
Architecture: explicit numeric head_row metadata constrains a column packer; the existing physical renderer and routes remain responsible for exact anchors, head sizes and collision-free leaders.
Tech: R/ggplot2/grid, testthat, existing source/CIGAR/geometry Python verifier.

- [x] Preserve current outputs, write/run failing fixed-row packing and facet tests.
- [x] Add optional head_row mapping/default, fixed-row column grouping and physical callback flags; retain free packing when no rows are specified.
- [x] Add/export the explicit MHC row template, clarify context coverage labels, regenerate PDF/PNG and detail crops.
- [x] Extend independent verification for row identity/status/copy consistency and physical offsets; run focused/regression tests and inspect results.
- [x] Independently review the change, fix material findings, update public documentation and deliver the figure with the coverage explanation.

Files: R/gene-lollipop-layout.R (constrained column packing); R/geom-gene-lollipop.R (head_row aesthetic and callback); tests/testthat/test-gene-lollipop.R (packing/facets); data-raw/ape_paper/render_mhc_continuous.R (explicit row template and coverage labels); verify_mhc_continuous.py (shared-row audit); results/mhc_continuous/README.md and man/geom_gene_lollipop.Rd (interpretation/API).

Commands: Rscript /tmp/check-gene-lollipop.R; Rscript data-raw/ape_paper/render_mhc_continuous.R; /Users/liudongyao/miniconda3/bin/python data-raw/ape_paper/verify_mhc_continuous.py; python data-raw/ape_paper/render_mhc_tracing_details.py.

Final evidence: 861 assertions pass across focused and relevant regression tests without warnings. Independent review reproductions were fixed and verified; 200 random fixed-row layouts also matched exhaustive partition feasibility/minimum column counts. Final PDF and PNG independently pass source/CIGAR/native-coordinate audits and all 285 heads use the explicit 41-key six-row template with identical physical row offsets across haplotypes, no overlaps, no proper leader crossings and no other-head intersections. Native-scale paired gorilla and human detail crops regenerated and inspected; context coverage labels explain the seven annotated and six curated-only source tracks.
