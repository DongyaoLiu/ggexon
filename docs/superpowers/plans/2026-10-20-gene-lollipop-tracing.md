# Gene Lollipop Tracing Plan

Goal: implement the approved tracing and ordered-group polish in the existing isolated worktree.

- [x] Preserve previous MHC PDF/PNG/layout for comparison. Add failing tests for column order, bounded shifts, clear routing lanes, true anchors, strand direction and nonintersection.
- [x] Extend R/gene-lollipop-layout.R with ordered-column packing and routed path geometry, and R/geom-gene-lollipop.R with routing parameters, matching stem colors, halos and ticks. Retain compact mode.
- [x] Update the MHC renderer and independent Python verifier to use/audit the new layout. Generate physical layout records and a reproducible VR3 detail figure for visual comparison.
- [x] Run focused/regression tests and the independent source/geometry/routing verifier. Inspect full and VR3 exports, obtain independent code review and resolve material findings.
- [x] Regenerate public Rd documentation and deliver links to the full figure and detail comparison.

Files: R/gene-lollipop-layout.R (packing/routes), R/geom-gene-lollipop.R (public API and grid drawing), tests/testthat/test-gene-lollipop.R (behavior), data-raw/ape_paper/render_mhc_continuous.R and verify_mhc_continuous.py (integration/audit), results/mhc_continuous/README.md (interpretation), man/geom_gene_lollipop.Rd (generated API).

Verification commands: Rscript /tmp/check-gene-lollipop.R; Rscript data-raw/ape_paper/render_mhc_continuous.R; /Users/liudongyao/miniconda3/bin/python data-raw/ape_paper/verify_mhc_continuous.py. Review callback-derived routes independently in Python and inspect actual PNG/PDF, including dense human/ape class II clusters.

Final evidence: 833 assertions passed across gene-lollipop, genebox, genetag and facet-label tests, without warnings. Independent review found an alpha/halo styling issue, reproduced by a failing test and fixed. Final source/CIGAR/native-coordinate and PDF+PNG route audits pass for 285 heads, 624 ribbons and 1359 context arrows: zero proper leader crossings, zero intersections with other head outlines, all anchors unchanged. Actual output is 180 x 603.865 mm with per-track annotation heights; human needs six rows, others three or four. Full exports and native-scale human/gorilla class II detail crops inspected.
