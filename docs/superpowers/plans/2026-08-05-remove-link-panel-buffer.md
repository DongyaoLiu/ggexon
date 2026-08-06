# Remove Link-Panel Buffer Rows Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Automatically collapse both vertical gtable spacer rows bordering a linkage-only panel row while preserving annotation-owned spacing.

**Architecture:** Extend `apply_link_panel_layout()` after facet-panel classification. A focused internal helper maps facet rows to rendered panel bounds, excludes rows occupied by panels, horizontal axes, or horizontal strips, and zeros only the unused gtable rows bordering linkage-only rows.

**Tech Stack:** R, ggplot2, gtable, grid, testthat, devtools

## Global Constraints

- Do not commit unless the user explicitly asks.
- Preserve all pre-existing worktree changes.
- Add no public argument or exported API.
- Do not collapse a gtable row shared with an annotation panel.
- Do not rebuild pkgdown for this focused source-and-test change.
- Execute inline unless the user explicitly requests subagents.

---

### Task 1: Collapse linkage-owned spacer rows

**Files:**
- Modify: `tests/testthat/test-geom-synteny-link.R:288`
- Modify: `R/plot-build.R:510-680`

**Interfaces:**
- Consumes: `layout_row_contains_only_link_panels(layout_df, panel_row)`, `panel_gtable_index(table, panel_col, panel_row, n_panels)`, and a rendered `gtable`.
- Produces: `collapse_link_panel_spacers(table, layout_df)`, returning the same gtable with linkage-owned vertical spacer heights set to `grid::unit(0, "pt")`.

- [ ] **Step 1: Strengthen the existing gtable regression fixture**

Add a visible spacing theme to the plot in `facet_genomics can compact link panels in the rendered gtable`:

```r
      facet_genomics(
        ggplot2::vars(track),
        scales = "free_x",
        ncol = 1,
        link_panel_height = 0.25,
        link_axis = "none",
        link_strip = "blank"
      ) +
      ggplot2::theme(panel.spacing.y = grid::unit(9, "pt"))
```

After the existing panel-height assertions, derive the unused rows bordering the linkage panel and assert that both are zero:

```r
  horizontal_structure <- grepl(
    "^(panel($|-)|axis-[tb]($|-)|strip-[tb]($|-))",
    table$layout$name
  )
  occupied_rows <- unique(unlist(Map(
    seq.int,
    table$layout$t[horizontal_structure],
    table$layout$b[horizontal_structure]
  )))
  panel_bounds <- function(name) {
    idx <- which(table$layout$name == name)
    c(top = table$layout$t[[idx]], bottom = table$layout$b[[idx]])
  }
  upper <- panel_bounds("panel-1-1")
  link <- panel_bounds("panel-1-2")
  lower <- panel_bounds("panel-1-3")
  upper_buffer <- setdiff(seq.int(upper[["bottom"]] + 1L, link[["top"]] - 1L), occupied_rows)
  lower_buffer <- setdiff(seq.int(link[["bottom"]] + 1L, lower[["top"]] - 1L), occupied_rows)

  expect_length(upper_buffer, 1L)
  expect_length(lower_buffer, 1L)
  expect_equal(as.numeric(table$heights[upper_buffer]), 0)
  expect_equal(as.numeric(table$heights[lower_buffer]), 0)
```

- [ ] **Step 2: Run the focused test and verify RED**

Run:

```bash
R -q -e 'devtools::test(filter = "geom-synteny-link", stop_on_failure = TRUE)'
```

Expected: the compact-link-panel test fails because `upper_buffer` and `lower_buffer` still retain the configured `9 pt` height.

- [ ] **Step 3: Make link-panel layout processing run for default link settings**

In `apply_link_panel_layout()`, resolve and classify `layout_df` before deciding whether work is necessary. Replace the early return based only on facet parameters with a return that applies only when there are no linkage rows and `annotation_axis` is `"all"`:

```r
  layout_df <- as.data.frame(build@layout$layout)
  if (!all(c("ROW", "COL") %in% names(layout_df))) {
    return(table)
  }

  layout_df$.ggexon_panel_type <- link_panel_type(layout_df)
  link_rows <- layout_df$.ggexon_panel_type == "link"
  link_rows[is.na(link_rows)] <- FALSE
  if (!any(link_rows) && identical(annotation_axis, "all")) {
    return(table)
  }

  link_iter <- if (link_active) which(link_rows) else integer(0)
```

- [ ] **Step 4: Add the internal spacer-collapse helper**

Add this helper beside the existing link-panel gtable helpers:

```r
collapse_link_panel_spacers <- function(table, layout_df) {
  facet_rows <- sort(unique(ggexon_gtable_index(layout_df$ROW)))
  link_only <- vapply(
    facet_rows,
    function(panel_row) layout_row_contains_only_link_panels(layout_df, panel_row),
    logical(1)
  )
  if (!any(link_only)) {
    return(table)
  }

  panel_bounds <- t(vapply(facet_rows, function(panel_row) {
    cells <- which(ggexon_gtable_index(layout_df$ROW) == panel_row)
    panel_idx <- unique(unlist(lapply(cells, function(i) {
      panel_gtable_index(
        table,
        panel_col = ggexon_gtable_index(layout_df$COL[[i]]),
        panel_row = panel_row,
        n_panels = nrow(layout_df)
      )
    })))
    panel_idx <- panel_idx[!is.na(panel_idx)]
    if (length(panel_idx) == 0L) {
      return(c(top = NA_integer_, bottom = NA_integer_))
    }
    c(
      top = min(table$layout$t[panel_idx]),
      bottom = max(table$layout$b[panel_idx])
    )
  }, numeric(2)))

  horizontal_structure <- grepl(
    "^(panel($|-)|axis-[tb]($|-)|strip-[tb]($|-))",
    table$layout$name
  )
  occupied_rows <- unique(unlist(Map(
    seq.int,
    table$layout$t[horizontal_structure],
    table$layout$b[horizontal_structure]
  )))

  rows_between <- function(upper_bottom, lower_top) {
    first <- upper_bottom + 1L
    last <- lower_top - 1L
    if (!is.finite(first) || !is.finite(last) || first > last) {
      return(integer())
    }
    seq.int(first, last)
  }

  spacer_rows <- integer()
  link_positions <- which(link_only)
  for (position in link_positions) {
    if (position > 1L && all(is.finite(panel_bounds[c(position - 1L, position), ]))) {
      spacer_rows <- c(
        spacer_rows,
        setdiff(
          rows_between(
            panel_bounds[position - 1L, "bottom"],
            panel_bounds[position, "top"]
          ),
          occupied_rows
        )
      )
    }
    if (position < length(facet_rows) &&
        all(is.finite(panel_bounds[c(position, position + 1L), ]))) {
      spacer_rows <- c(
        spacer_rows,
        setdiff(
          rows_between(
            panel_bounds[position, "bottom"],
            panel_bounds[position + 1L, "top"]
          ),
          occupied_rows
        )
      )
    }
  }

  spacer_rows <- unique(spacer_rows)
  for (row in spacer_rows) {
    table$heights[[row]] <- grid::unit(0, "pt")
  }
  table
}
```

- [ ] **Step 5: Invoke the helper from `apply_link_panel_layout()`**

After the link-panel axis/strip loop and before annotation-axis collapsing, add:

```r
  table <- collapse_link_panel_spacers(table, layout_df)
```

- [ ] **Step 6: Run the focused test and verify GREEN**

Run:

```bash
R -q -e 'devtools::test(filter = "geom-synteny-link", stop_on_failure = TRUE)'
```

Expected: all `test-geom-synteny-link.R` tests pass, including zero-height upper and lower linkage buffers.

- [ ] **Step 7: Run adjacent gtable tests**

Run:

```bash
R -q -e 'devtools::test(filter = "cross-panel-annotations|scale-genomic|theme-ggexon", stop_on_failure = TRUE)'
```

Expected: all selected gtable-rendering tests pass.

### Task 2: Render the requested example and verify the package

**Files:**
- Create: `/Users/liudongyao/.codex/visualizations/2026/08/05/019fd1cc-36dd-7572-a302-68caaebf1785/link-panel-buffer-example.png`
- Verify only: package source and tests

**Interfaces:**
- Consumes: public `ggexon()`, `geom_genetag()`, `geom_synteny_link()`, and `facet_genomics()` APIs.
- Produces: a PNG with two gene-annotation panels separated by one linkage panel whose buffer rows are absent.

- [ ] **Step 1: Render a two-annotation/one-linkage example**

Use this complete example and render it at `1800 x 1050` pixels:

```r
devtools::load_all(quiet = TRUE)

track_levels <- c("human", "link_human_mouse", "mouse")
genes <- data.frame(
  track = factor(rep(c("human", "mouse"), each = 3), levels = track_levels),
  xmin = c(10, 42, 76, 1000, 1042, 1076),
  xmax = c(30, 64, 94, 1028, 1064, 1098),
  y = 1,
  strand = c("+", "-", "+", "+", "-", "+"),
  gene_key = rep(c("GENE1", "GENE2", "GENE3"), 2),
  label = rep(c("GENE1", "GENE2", "GENE3"), 2),
  stringsAsFactors = FALSE
)
links <- data.frame(
  track = factor(rep("link_human_mouse", 3), levels = track_levels),
  tspecies = "human",
  tchr = "chr1",
  tstart = c(10, 42, 76),
  tend = c(30, 64, 94),
  qspecies = "mouse",
  qchr = "chr1",
  qstart = c(1000, 1042, 1076),
  qend = c(1028, 1064, 1098),
  strand = "+",
  group = 1:3,
  homology = c("GENE1", "GENE2", "GENE3"),
  stringsAsFactors = FALSE
)

p <- ggexon() +
  geom_synteny_link(
    data = links,
    mapping = ggplot2::aes(
      tspecies = tspecies, tchr = tchr, tstart = tstart, tend = tend,
      qspecies = qspecies, qchr = qchr, qstart = qstart, qend = qend,
      strand = strand, group = group, fill = homology
    ),
    inherit.aes = FALSE,
    alpha = 0.55
  ) +
  geom_genetag(
    data = genes,
    mapping = ggplot2::aes(fill = gene_key),
    show_label = TRUE
  ) +
  facet_genomics(
    ggplot2::vars(track), scales = "free_x", ncol = 1,
    link_panel_height = 0.55, link_axis = "none", link_strip = "blank"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    panel.spacing.y = grid::unit(14, "pt"),
    legend.position = "bottom"
  )

grDevices::png(
  "/Users/liudongyao/.codex/visualizations/2026/08/05/019fd1cc-36dd-7572-a302-68caaebf1785/link-panel-buffer-example.png",
  width = 1800,
  height = 1050,
  res = 180
)
grid::grid.draw(ggplot2::ggplot_gtable(ggexon_build(p)))
grDevices::dev.off()
```

Expected: the linkage ribbons meet the two annotation-panel regions without the configured `14 pt` gaps around the linkage panel.

- [ ] **Step 2: Inspect the PNG**

Open the generated PNG and verify that it contains exactly two labeled gene-annotation panels and one central linkage panel, with no blank vertical buffer immediately above or below the linkage ribbons.

- [ ] **Step 3: Run the full test suite**

Run:

```bash
R -q -e 'devtools::test(stop_on_failure = TRUE)'
```

Expected: the full package test suite completes with zero failures.

- [ ] **Step 4: Inspect the focused diff and worktree status**

Run:

```bash
git diff -- R/plot-build.R tests/testthat/test-geom-synteny-link.R docs/superpowers/specs/2026-08-05-remove-link-panel-buffer-design.md docs/superpowers/plans/2026-08-05-remove-link-panel-buffer.md
git status --short
```

Expected: the new changes are limited to the gtable helper, its regression test, the approved design/plan documents, and the external PNG; all unrelated pre-existing modifications remain untouched.
