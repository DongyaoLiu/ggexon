# Shared ggexon Base Theme Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a reusable `theme_ggexon_base()` that removes decorative backgrounds and make every ggexon theme inherit that background contract while retaining strip text and x-grid lines.

**Architecture:** A private background-only theme modifier owns the six blank background elements. The exported complete base theme combines `theme_minimal()` with that modifier; track and tree themes inherit the complete base, while the composable side-strip helper reuses only the private modifier so it cannot reset axes or grids already supplied by another theme.

**Tech Stack:** R, ggplot2 themes, testthat, roxygen2, pkgdown.

## Global Constraints

- Keep facet-label text visible.
- Keep the major x-grid visible whenever `show_x_grid = TRUE`.
- Default side-strip backgrounds to blank; retain an explicit color override.
- Do not stage or commit changes.
- Preserve unrelated files in the existing dirty checkout.

---

### Task 1: Add the shared background contract and theme inheritance

**Files:**

- Modify: `tests/testthat/test-theme-ggexon.R`
- Modify: `R/theme-ggexon.R`

**Interfaces:**

- Consumes: existing `theme_ggexon_track()`, `theme_ggexon_genomictree()`, `theme_ggexon_side_strips()`, and `theme_ggexon_pairwise()` functions.
- Produces: exported `theme_ggexon_base(base_size = 8, base_family = "")`; private `.theme_ggexon_backgrounds()`; all public ggexon themes with the shared blank-background contract.

- [ ] **Step 1: Write failing base-theme and inheritance tests**

At the top of `tests/testthat/test-theme-ggexon.R`, add a test helper:

```r
expect_blank_ggexon_backgrounds <- function(th) {
  background_elements <- c(
    "plot.background",
    "panel.background",
    "panel.border",
    "strip.background",
    "legend.background",
    "legend.key"
  )
  for (element in background_elements) {
    expect_s3_class(ggplot2::calc_element(element, th), "element_blank")
  }
}
```

Add a base-theme test:

```r
test_that("theme_ggexon_base removes backgrounds but retains labels and x grids", {
  th <- theme_ggexon_base()

  expect_s3_class(th, "theme")
  expect_blank_ggexon_backgrounds(th)
  expect_s3_class(
    ggplot2::calc_element("strip.text.y.left", th),
    "element_text"
  )
  expect_s3_class(
    ggplot2::calc_element("panel.grid.major.x", th),
    "element_line"
  )
})
```

Extend the existing tests to assert the shared contract on:

```r
expect_blank_ggexon_backgrounds(theme_ggexon_track())
expect_blank_ggexon_backgrounds(theme_ggexon_genomictree())
expect_blank_ggexon_backgrounds(theme_ggexon_side_strips("left"))
expect_blank_ggexon_backgrounds(theme_ggexon_pairwise())
```

Change the default side-strip expectation from `element_rect` to `element_blank`, and add this explicit-override assertion:

```r
expect_s3_class(
  ggplot2::calc_element(
    "strip.background",
    theme_ggexon_side_strips("left", background = "grey96")
  ),
  "element_rect"
)
```

Keep the existing pairwise assertion that `panel.grid.major.x` is an `element_line`.

- [ ] **Step 2: Run the theme tests and verify RED**

Run:

```sh
Rscript -e 'devtools::test(filter = "theme-ggexon")'
```

Expected: FAIL because `theme_ggexon_base()` and `.theme_ggexon_backgrounds()` do not exist and the current side-strip default is an `element_rect`.

- [ ] **Step 3: Implement the background-only modifier and exported base theme**

At the start of `R/theme-ggexon.R`, add:

```r
.theme_ggexon_backgrounds <- function() {
  ggplot2::theme(
    plot.background = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank()
  )
}

theme_ggexon_base <- function(base_size = 8, base_family = "") {
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) +
    .theme_ggexon_backgrounds()
}
```

Add roxygen documentation and `@export` above `theme_ggexon_base()`. State that the function removes decorative backgrounds but preserves strip text, axes, and coordinate grids.

- [ ] **Step 4: Make every derived theme share the contract**

In `theme_ggexon_track()`, replace the direct `ggplot2::theme_minimal()` call with:

```r
theme_ggexon_base(base_size = base_size, base_family = base_family)
```

Leave `theme_ggexon_genomictree()` based on `theme_ggexon_track()`.

Change the side-strip signature to:

```r
theme_ggexon_side_strips <- function(side = c("right", "left"),
                                     base_size = 8,
                                     face = "bold",
                                     background = NA)
```

Return the background modifier plus the side-strip-specific theme:

```r
.theme_ggexon_backgrounds() + do.call(ggplot2::theme, args)
```

The order is required: an explicit non-missing `background` in `args` must override the shared blank `strip.background`.

Leave `theme_ggexon_pairwise()` composed from `theme_ggexon_track()` and `theme_ggexon_side_strips()`; the incomplete side modifier must not reset the track's x-grid or axis settings.

- [ ] **Step 5: Run the theme tests and verify GREEN**

Run:

```sh
Rscript -e 'devtools::test(filter = "theme-ggexon")'
```

Expected: PASS with no test warnings. Confirm the default pairwise major x-grid remains an `element_line` and every named background resolves to `element_blank`.

---

### Task 2: Document, render, and verify the theme hierarchy

**Files:**

- Modify: `_pkgdown.yml`
- Generated: `NAMESPACE`
- Generated: `man/theme_ggexon_base.Rd`
- Generated: `man/theme_ggexon_side_strips.Rd`
- Generated: `man/theme_ggexon_track.Rd`
- Create: `/Users/liudongyao/.codex/visualizations/2026/08/05/019fd1cc-36dd-7572-a302-68caaebf1785/pairwise-theme-no-background-bars.png`

**Interfaces:**

- Consumes: `theme_ggexon_base()` and the updated derived themes from Task 1.
- Produces: exported documentation, pkgdown discoverability, and a visually verified pairwise figure with left labels, no gray strip bars, and retained vertical x-grid lines.

- [ ] **Step 1: Update public documentation and pkgdown indexing**

Document the hierarchy in `R/theme-ggexon.R`:

- `theme_ggexon_base()` is the shared complete foundation.
- `theme_ggexon_track()` inherits the base and adds track axes, grids, legend, and spacing.
- `theme_ggexon_genomictree()` inherits through the track theme.
- `theme_ggexon_side_strips()` is an incomplete modifier that shares only the background contract.
- `theme_ggexon_pairwise()` inherits the base through the track theme and adds left side-strip styling.

Update the `background` parameter text to say that `NA` or `"none"` is the default and draws no strip rectangle. Add `theme_ggexon_base` before the other theme functions in `_pkgdown.yml`.

- [ ] **Step 2: Regenerate documentation**

Run:

```sh
Rscript -e 'devtools::document()'
```

Expected: roxygen exports `theme_ggexon_base` and writes its Rd file plus any changed theme-family Rd files.

- [ ] **Step 3: Execute the extracted base-theme documentation example**

Extract and execute the generated example:

```sh
Rscript -e 'tools::Rd2ex("man/theme_ggexon_base.Rd", out = "/private/tmp/theme_ggexon_base-example.R")'
Rscript -e 'devtools::load_all(quiet = TRUE); sys.source("/private/tmp/theme_ggexon_base-example.R", envir = .GlobalEnv)'
```

Expected: exit status 0 with no example error.

- [ ] **Step 4: Render the pairwise visual**

Use the existing pairwise example data and layout:

```r
facet_genomics(
  ggplot2::vars(track),
  ncol = 1,
  scales = "free_x",
  strip.position = "left",
  link_axis = "none",
  link_strip = "blank",
  annotation_axis = "bottom",
  vertical = "center"
) +
  theme_ggexon_pairwise(base_size = 10)
```

Save a 1800 x 1000 white-background PNG to the path listed above. Visually confirm:

- the Human and Mouse labels remain visible on the left;
- gray strip-background bars are absent;
- vertical x-grid lines remain visible;
- annotation bodies remain vertically centered;
- the linkage panel retains its compact, buffer-free layout.

- [ ] **Step 5: Run focused and full verification**

Run:

```sh
Rscript -e 'devtools::test(filter = "theme-ggexon|geom-synteny-link")'
Rscript -e 'devtools::test()'
```

Expected: all focused tests and the complete suite pass; only the three known discrete-alpha warnings and the existing PSL skip may remain.

- [ ] **Step 6: Run the final documentation and diff audit**

Run:

```sh
Rscript -e 'devtools::document()'
git diff --check
git status --short
```

Expected: the documentation pass makes no further theme documentation changes, `git diff --check` exits 0, no files are staged, and unrelated dirty-worktree changes remain untouched.
