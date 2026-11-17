# Prepared gene-lollipop implementation plan

> Implement the approved design in this existing codex branch. Preserve the
> dirty tutorial work. Use test-first implementation and scoped review; do not
> commit or publish. Execution is already authorized.

**Goal:** Prepare a shared gene-slot grid on Syn objects and consume it through
native context-aware geom dispatch, then migrate and render Tutorial 04.

**Architecture:** Named entries in individual plot caches contain source gene
rows and a common slot grid. A small metadata registry detects cleared caches.
The existing LayerSyn dispatcher selects prepared entries and contextual
windows. The geom measures and draws a regular physical grid on the device.

**Tech stack:** Existing R/S4 Syn containers, ggplot2/S7, grid, testthat,
roxygen2 and pkgdown; no new dependencies.

## Constraints

- Source coordinates and strand stay native; no stored PANEL/scale IDs.
- Same shared key/copy occupies the same row and column across panels.
- Empty cells remain reserved; all duplicate copies survive.
- Reference order, unambiguous flanking insertions, explicit ambiguity overrides.
- One prepared grid is automatic; several require `lollipop_layout`.
- Native unprepared and explicit dataframe plotting stay compatible.
- Actual device fitting may fail clearly; never drop genes or shrink text.

## Task 1: Prepared data, identities and slots

Files: new `R/prepared-gene-lollipop.R`, optional focused companion module,
new `tests/testthat/test-prepared-gene-lollipop.R` and test helper.

Interface: public `prepare_gene_lollipop()` and `query_gene_lollipop()` as in
the approved spec. Internal `.gene_lollipop_prepared_data(x, layer)` returns
NULL when no preparation exists, otherwise a contextual gene dataframe.
Required columns: x, y (baseline zero), xmin, xmax, genomic_x,
genomic_xmin, genomic_xmax, strand, label, fill, gene_id, gene_name,
track, individual, head_row, head_col, grid_nrow, grid_ncol,
lollipop_grid (common identity), slot_key.

- [x] Write real Syn fixtures with a reference A/B/C and a second individual
  A/X/B/C/C. Expect X between A/B and two separate C slots in a 2-by-3 grid.
  Assert native anchors, metadata filtering, occupancy, and absent-copy blanks.
- [x] Run `devtools::test(filter="^prepared-gene-lollipop$", stop_on_failure=TRUE)`
  and observe the missing preparation API failure.
- [x] Implement selection, key/homology resolution, reference/flank ordering,
  duplicate slots, explicit dimensions/overrides, caching and query validation.
- [x] Add/run ambiguity, capacity, conflicting slots, explicit copy IDs,
  homology fallback, stale source and cleared cache cases.
- [x] Implement contextual data dispatch, including repeated tracks backed by
  one individual; crop anchors without repacking the stored slots.
- [x] Confirm focused tests pass and review the module against the spec.

## Task 2: Shared-grid drawing and native geom integration

Files: `R/geom-gene-lollipop.R`, `R/gene-lollipop-layout.R`, new
`tests/testthat/test-gene-lollipop-grid.R`.

Interface: `.gene_lollipop_grid()` takes native anchor positions in mm,
slot row/column, declared grid dimensions, width and routing constraints.
Returns the same physical columns as the existing packers, preserving input
order. Use every declared column and row even when some are unoccupied.

- [x] Write a physical geometry test: rows 2/1 and columns 1/3 in a 2-by-3
  grid have the same centres when intermediate occupants are omitted. Reject
  conflicting cells and insufficient width/height/displacement.
- [x] Observe RED before adding the helper.
- [x] Implement equal centres with gutters reserved for all grid rows.
  Validate dimensions, cells, physical clearance and displacement.
- [x] Add `lollipop_layout` to the geom and wire prepared dispatch. Reject
  prepared-coordinate/slot remapping and dataframe/name combinations. Map
  prepared identity and grid fields through scale/facet setup.
- [x] Route prepared heads through the grid helper at draw time. Preserve
  existing compact/column behavior. Capture slot identity in layout callbacks.
- [x] Test real Syn builds, reverse panels, common centres across panels,
  crop-without-repacking and automatic/named grid selection. Use real PDF
  rendering with callback assertions, alongside the existing lollipop tests.

## Task 3: Tutorial migration and exports

Files: `vignettes/mhc-continuous-demo.Rmd`, installed MHC helper,
`data-raw/mhc_haplotypes/render-mhc-example.R`, MHC tests, DESCRIPTION,
NAMESPACE, `_pkgdown.yml`, generated man and website files.

- [x] Replace MHC lollipop preparation with the new verb, selecting
  `where = list(gene_role="MHC")`, `key="plot_label"`, reference HSAh1.
  Supply an explicit source-key order to resolve ambiguous reference gaps.
- [x] Retrieve MHC heads using query_gene_lollipop; remove the helper's
  manual head-row vector and MHC dataframe construction. Keep native spans
  and all background genes in the figure.
- [x] Use implicit Syn data in geom_gene_lollipop; retain only aesthetic and
  physical style arguments. Fit 4 rows and 6 columns, adjusting the explicit
  displacement allowance based on the rendered native anchors.
- [x] Extend render verification to assert shared row AND column AND head
  centres for the same key, all 58 heads, exact anchors, and reverse directions.
- [x] Generate exports/reference docs using `devtools::document()`; preserve
  existing DESCRIPTION Collate order except for the new modules.
- [x] Render PDF/PNG via the vignette-driven renderer and inspect the main
  figure. Run a second size and fresh installed-package draw as well.

## Task 4: Review and website verification

- [x] Run focused preparation, grid, lollipop, MHC, printing and genomic-layout
  tests; run broader tests once, recording the existing missing HOX-vignette
  failure separately if it persists.
- [x] Request scoped review of preparation/identity and drawing/integration;
  resolve actionable findings and re-run the covering tests.
- [x] Build the website with installed-package execution:
  `pkgdown::build_site(devel=FALSE, new_process=FALSE, install=TRUE, preview=FALSE)`.
- [x] Check the generated article, API pages, preview server and figure. Report
  the concrete result and remaining limitations with links. Leave uncommitted.

## Verification record

- Independent preparation re-review: 135 assertions pass across preparation and grid tests; both reported bugs fixed.
- Independent geom re-review: 674 assertions pass across grid and legacy lollipop tests; both reported bugs fixed.
- Full suite: one pre-existing missing-HOX-vignette error at test-hox-cluster-expansion-data.R:363; no other failures.
- Vignette renderer: all three stages pass, 58 heads, native anchors/directions, common cell centres, 108 + 44 direct link blocks.
- Fresh isolated installed package: PDF and PNG pass at 9 x 4.8 and 8 x 5 inches.
- Physical figure uses 4 rows, 6 columns and an explicit 85 mm displacement allowance; native gene spans and anchors are unchanged.
- Full installed-package website build passed; article and both new API pages served HTTP 200 with matching bytes. Browser preview shows the new verbs and shared-grid figure.
