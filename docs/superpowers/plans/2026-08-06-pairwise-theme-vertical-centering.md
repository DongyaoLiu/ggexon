# Pairwise Theme and Annotation Vertical Centering Implementation Plan

> **For Codex:** Execute this plan task by task with the `executing-plans` workflow. Use test-driven development: add each failing test before its production change. Do not commit or stage changes.

**Goal:** Add a pairwise-alignment presentation with annotation strips on the left and annotation geometry vertically centered above and below a middle linkage panel.

**Architecture:** `facet_genomics()` gains an opt-in `vertical = "center"` mode. During plot building, after position scales are fully trained and linkage panels have received their fixed `c(0, 1)` y-range, the builder finds the visible annotation-body center in each annotation panel and symmetrizes that panel's trained y-range around it. A new `theme_ggexon_pairwise()` composes the existing track and left-side-strip themes; strip placement remains a facet responsibility through `strip.position = "left"`.

**Tech Stack:** R, ggplot2/ggproto, testthat, roxygen2, pkgdown.

---

## Task 1: Add the `facet_genomics(vertical = ...)` API and centered annotation ranges

**Files:**

- Modify: `R/facet-genomics.R`
- Modify: `R/plot-build.R`
- Test: `tests/testthat/test-geom-synteny-link.R`

### Step 1: Write failing integration tests

Add tests that construct two annotation tracks separated by one linkage track using `geom_genetag()` and `geom_synteny_link()`.

The tests must assert:

1. `facet_genomics(vertical = "center")` is accepted and stored in the facet parameters.
2. Each annotation panel's final `y.range` has a midpoint equal to the known gene-body center (`geom_genetag()` uses `exon_height / 2`).
3. The linkage panel still has `y.range == c(0, 1)`.
4. Omitting `vertical` produces the same ranges as `vertical = "default"`, and the asymmetric default annotation range is not silently centered.
5. An unsupported value such as `vertical = "top"` reports an argument-matching error mentioning `vertical`.

Use the built layout to locate panel IDs by `track` and inspect `built$layout$panel_params[[panel_id]]$y.range`; do not depend on fixed panel row numbers.

Add a focused helper test for mixed annotation layer coordinates. Pass synthetic layer data containing `y_middle`, `y`, and `ymin`/`ymax`, plus a linkage-panel row, and verify that annotation centers use this precedence:

```r
y_middle -> y -> (ymin + ymax) / 2
```

For multiple finite centers in one panel, expect the midpoint of their combined range. Confirm that linkage-panel rows are excluded.

Run:

```sh
Rscript -e 'devtools::test(filter = "geom-synteny-link")'
```

Expected: FAIL because `vertical` and the centering helper do not exist yet.

### Step 2: Add and validate the facet argument

In `facet_genomics()` add:

```r
vertical = c("default", "center")
```

Validate it with `rlang::arg_match0()` using the existing validation style, then include the selected value in the facet `params` list. Document both modes in the roxygen block:

- `"default"`: preserve the current trained annotation y-ranges.
- `"center"`: vertically center annotation geometry while leaving linkage panels unchanged.

### Step 3: Implement annotation-body center extraction

In `R/plot-build.R`, add a private helper with a stable contract:

```r
annotation_panel_body_centers <- function(data, annotation_panel_ids)
```

For every data frame in the built layer list:

1. Require a `PANEL` column and at least one row.
2. Normalize panel IDs with `ggexon_gtable_index()`.
3. Keep only IDs in `annotation_panel_ids`.
4. Choose a per-row body center from finite values in `y_middle`, then fill missing values from `y`, then fill remaining values from the midpoint of finite `ymin` and `ymax`.
5. Combine all annotation layers and return one named numeric center per panel, calculated as `mean(range(finite_centers))`.
6. Return an empty named numeric vector when no usable center exists.

This precedence keeps exon/gene body coordinates authoritative while treating inflated label bounds only as a fallback.

### Step 4: Symmetrize only annotation panel ranges

Add:

```r
apply_facet_vertical <- function(layout, data, params = list())
```

Behavior:

1. Return `layout` unchanged unless `params$vertical` is exactly `"center"`.
2. Identify annotation panels from `layout$layout` using `link_panel_type()`; exclude every linkage-only panel.
3. Get centers with `annotation_panel_body_centers()`.
4. For each annotation panel with a finite two-value current range and a finite body center, calculate:

```r
half_range <- max(body_center - current_range[1], current_range[2] - body_center)
centered_range <- body_center + c(-half_range, half_range)
```

5. Update `panel_params[[panel_id]]$y.range` and, when present, the matching `y$continuous_range` and `y.sec$continuous_range` fields.
6. Leave empty panels and unusable ranges unchanged.

Call the helper in the ggexon build sequence after `apply_link_panel_y_range(layout)` and before `layout$map_position(data)`. This preserves the linkage contract and changes only the final coordinate mapping for annotation panels.

### Step 5: Run focused tests

Run:

```sh
Rscript -e 'devtools::test(filter = "geom-synteny-link")'
```

Expected: PASS, including the new default-compatibility, annotation-centering, and fixed-linkage-range assertions.

---

## Task 2: Add `theme_ggexon_pairwise()`

**Files:**

- Modify: `R/theme-ggexon.R`
- Modify: `tests/testthat/test-theme-ggexon.R`
- Modify: `_pkgdown.yml`
- Generated: `NAMESPACE`
- Generated: `man/facet_genomics.Rd`
- Generated: `man/theme_ggexon_pairwise.Rd`

### Step 1: Write failing theme tests

Add tests for:

```r
theme_ggexon_pairwise(
  base_size = 8,
  base_family = "",
  show_x_axis = TRUE,
  show_x_grid = TRUE,
  show_legend = FALSE
)
```

The default theme must inherit from `theme_ggexon_track()` and add the existing left-side strip treatment. Assert:

- the result is a ggplot theme;
- y-axis text, ticks, and title are blank;
- `strip.placement == "outside"`;
- `strip.text.y.left` has `angle == 0` and `hjust == 1`;
- x-axis text is visible by default;
- major x grid lines are visible by default;
- the legend is hidden by default.

Also test `show_x_axis = FALSE`, `show_x_grid = FALSE`, and `show_legend = TRUE`, expecting blank x-axis elements, blank vertical grid elements, and `legend.position == "right"`.

Run:

```sh
Rscript -e 'devtools::test(filter = "theme-ggexon")'
```

Expected: FAIL because `theme_ggexon_pairwise()` does not exist.

### Step 2: Implement and document the public theme

In `R/theme-ggexon.R`, export `theme_ggexon_pairwise()` with the exact signature tested above. Compose:

```r
theme_ggexon_track(
  base_size = base_size,
  base_family = base_family,
  show_x_axis = show_x_axis,
  show_y_axis = FALSE,
  show_x_grid = show_x_grid,
  show_legend = show_legend
) +
  theme_ggexon_side_strips(side = "left", base_size = base_size)
```

Document that panel-label placement is controlled by:

```r
facet_genomics(strip.position = "left", vertical = "center")
```

Include a runnable roxygen example with top annotation, middle linkage, and bottom annotation panels. Do not make the theme mutate facet configuration or layer data.

### Step 3: Add the theme to the pkgdown reference index

Add `theme_ggexon_pairwise` alongside the other ggexon theme functions in `_pkgdown.yml`.

### Step 4: Generate documentation and rerun focused tests

Run:

```sh
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test(filter = "theme-ggexon|geom-synteny-link")'
```

Expected: roxygen updates `NAMESPACE` and the two Rd files; focused tests PASS.

---

## Task 3: Render and verify the pairwise example

**Files:**

- Create: `/Users/liudongyao/.codex/visualizations/2026/08/05/019fd1cc-36dd-7572-a302-68caaebf1785/pairwise-theme-centered-example.png`
- Inspect: all modified source, test, and generated documentation files

### Step 1: Render a representative figure

Create a temporary R script under `/private/tmp` that loads the package with `devtools::load_all()`, builds two labeled annotation panels separated by a linkage panel, and applies:

```r
facet_genomics(
  rows = vars(track),
  scales = "free_x",
  space = "free_x",
  strip.position = "left",
  link_axis = "none",
  link_strip = "blank",
  annotation_axis = "bottom",
  vertical = "center"
) +
  theme_ggexon_pairwise()
```

Save the PNG at 1800 x 1000 pixels with a white background. Include gene labels with enough reserved space to make vertical centering visibly meaningful.

### Step 2: Inspect the rendered PNG

Open the image with the local image viewer and verify visually:

- one annotation panel is above and one below;
- linkage ribbons occupy the middle panel with no linkage-owned vertical buffer rows;
- panel labels are horizontal and on the left;
- gene bodies are vertically centered within both annotation panels;
- labels and axes are not clipped.

If visual defects are found, add or refine a regression test before changing production code.

### Step 3: Run package verification

Run focused tests, then the full suite:

```sh
Rscript -e 'devtools::test(filter = "theme-ggexon|geom-synteny-link")'
Rscript -e 'devtools::test()'
```

Expected: all new tests PASS; no new warnings or failures are introduced. Record any known pre-existing warnings separately.

Run documentation consistency checks:

```sh
Rscript -e 'devtools::document()'
git diff --check
git status --short
```

Expected: a second documentation pass produces no additional changes, `git diff --check` is clean, and only intended files plus pre-existing user changes appear. Do not stage or commit.

### Step 4: Review the final diff against the approved design

Confirm explicitly:

- `vertical = "default"` is backward compatible;
- `vertical = "center"` changes annotation panels only;
- linkage y-ranges remain exactly `c(0, 1)`;
- the theme provides left-oriented strip styling but does not claim to set `strip.position`;
- all public APIs have generated documentation;
- no unrelated dirty-worktree changes were altered.
