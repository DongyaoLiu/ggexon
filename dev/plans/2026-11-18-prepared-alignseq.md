# Prepared nucleotide detail implementation plan

> Execute with subagent-driven-development in the managed prepared-alignseq
> worktree. The user approved the design and upper human d2d3 connector target.

**Goal:** Store prepared nucleotide detail on Syn objects, dispatch it natively,
and attach marker-free sequence callouts to the reordered AS3MT gene models.

**Architecture:** Reuse `alignseq_data()` and per-individual named plot-cache
entries. Add a `LayerSyn` resolver to `GeomAlignseq`, preserving explicit-table
calls. Resolve transcript connector geometry from the displayed exon layer so
it follows that layer's row mapping, heights, filtering and genomic scale.

**Tech stack:** R, S4 Syn objects, ggplot2/S7, grid, testthat, roxygen2, pkgdown.

## Constraints

- No new runtime dependency or S4 annotation class.
- Keep the existing aligned DNA and half-base gap coordinate semantics.
- Keep the user's prior rectangle/segment removal from the final figure.
- Human d2d3 above full length; human callout targets d2d3, chimp targets chimp.
- Preserve explicit `geom_alignseq(data = ...)` behaviour by default.
- Do not modify the published v1.0.0 tag, source data, or unrelated user drafts.
- Worktree: `/Users/liudongyao/.codex/worktrees/prepared-alignseq/ggexon`.

## Task 1: Stored preparation and query

Files: new `R/prepared-alignseq.R`, new
`tests/testthat/test-prepared-alignseq.R`; Collate entry in `DESCRIPTION`.

Public interfaces:

```r
prepare_alignseq(x, aligned, anchors, reference, name = "alignseq",
                regions = NULL, details = NULL, flank = 3, boundaries = NULL)
query_alignseq(x, name = NULL, individual = NULL,
               chr = NULL, start = NULL, end = NULL)
```

`boundaries`, when supplied, is a data frame with sequence, position and label.
Prepared rows retain `individual` in addition to the existing alignseq columns.
Per-individual entries live in `plot_cache$alignseq[[name]]` with data and recipe.
Use package-owned metadata to distinguish cleared preparations from absent ones.
Queries do not mutate the object. Auto-selection requires one applicable name.

- [ ] Run existing alignseq tests as the baseline.
- [ ] Write failing tests for a stored round trip, object immutability, selected
  individual/windows, boundary records, invalid anchors, multiple names and
  cleared entries; confirm the new verb is missing.
- [ ] Implement by reusing `alignseq_data()` and standard Syn access patterns.
- [ ] Run the focused tests and inspect the diff.
- [ ] Review the task against this interface before consuming it in Task 2.

## Task 2: Native dispatch and transcript connectors

Files: `R/geom-alignseq.R`, `R/syn-layer.R`, new tests for native dispatch and
gene-model attachment; use a separate small internal file if geometry resolution
would make the geom file difficult to follow.

New geom arguments: `data = NULL`, `alignment = NULL`, `species = NULL`,
`chr = NULL`, `subset = NULL`, `show_markers = TRUE`,
`anchor = c("track", "transcript")`, `transcripts = NULL`.

Named `transcripts` map individual IDs to transcript IDs. The approved tutorial
uses c(human = "human_short", chimp = "chimp"). Existing data-frame calls retain
their current default connector and marker rendering.

- [ ] Add failing tests for dispatch without a data frame, explicit-name
  ambiguity, panel/window selection and custom annotation track names.
- [ ] Route Syn input through `LayerSyn`, with `GeomAlignseq$syn_data` querying
  the named cache and binding native coordinates to the actual plot context.
- [ ] Add failing grob/geometry tests for marker suppression and attachment to
  the selected exon model, including changed y mapping and intron compression.
- [ ] Resolve target geometry from the matching exon-layer context; do not
  introduce hardcoded AS3MT row IDs or independent example y coordinates.
- [ ] Hide event points and their labels when show_markers is FALSE, while
  retaining sequence colours, gaps and curved connectors.
- [ ] Pass existing explicit-table tests and the new dispatch/geometry tests.
- [ ] Review this integration before migrating the tutorial.

## Task 3: AS3MT migration and documentation

Files: `inst/extdata/as3mt/as3mt-object-data.R`,
`inst/extdata/as3mt/load-as3mt-example.R`,
`vignettes/as3mt-splice-variants-demo.Rmd`, AS3MT tests, `_pkgdown.yml`, generated
Rd/NAMESPACE files. Preserve the authored roxygen Collate prerequisites.

- [ ] Update source feature row metadata so human_short is above human_full.
- [ ] Derive exon label y positions from current transcript metadata.
- [ ] Prepare the two native DNA views using `prepare_alignseq()` and inspect
  them using `query_alignseq()`; remove DNA tables from example plot-data views.
- [ ] Use native dispatch for both the sequence close-up and final figure.
- [ ] In the final plot, hide markers and attach to human_short/chimp.
- [ ] Update figure text and retain the original DNA evidence/provenance.
- [ ] Generate public documentation and expose the new verbs in the website
  function reference.
- [ ] Render the tutorial, inspect the final image, and run the full test suite.
- [ ] Install from a clean source build, run an installed tutorial smoke, and
  request final whole-change review; resolve findings before completion.

## Completion evidence

Record focused red/green results, full test summary, installed smoke, rendered
figure paths, and review verdict in this plan's local SDD ledger. Final delivery
must name the new verb/query usage and provide the updated tutorial preview.
