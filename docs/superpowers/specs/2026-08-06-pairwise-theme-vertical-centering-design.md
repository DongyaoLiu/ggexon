# Pairwise Alignment Theme and Vertical Centering

## Goal

Provide a concise pairwise-alignment presentation for one top annotation panel,
one middle linkage panel, and one bottom annotation panel. Panel labels should
appear horizontally on the left, and annotation geometry should be vertically
centered within each annotation panel.

## Public API

Add `vertical = c("default", "center")` to `facet_genomics()`.
`"default"` preserves existing behavior. `"center"` centers annotation
geometry without changing linkage-panel coordinates.

Add `theme_ggexon_pairwise()` as a public theme helper. Typical usage is:

```r
facet_genomics(
  ggplot2::vars(track),
  scales = "free_x",
  ncol = 1,
  vertical = "center",
  strip.position = "left",
  link_panel_height = 0.4,
  link_axis = "none",
  link_strip = "blank",
  annotation_axis = "bottom"
) +
theme_ggexon_pairwise()
```

`strip.position = "left"` remains a facet argument because a ggplot2 theme
cannot move facet strips. The pairwise theme styles that left strip but does
not silently replace the plot's facet.

## Vertical-Centering Behavior

Centering applies only to layout rows classified as annotation panels. Linkage
panels retain their existing fixed `c(0, 1)` y-range and anchor behavior.

For each annotation panel, derive the center of the visible annotation bodies
from built annotation-layer coordinates. Use geometry center coordinates when
available (`y_middle` or `y`), falling back to the midpoint of finite
`ymin`/`ymax` values. Multiple annotation layers contribute to one combined
body-center range.

The trained panel y-range can include asymmetric space for outside labels or
other annotation details. To center the annotation bodies without clipping
those details, preserve the larger existing distance from the body center to
either trained boundary and mirror it on the opposite side. In other words,
replace the trained range with a symmetric range around the annotation-body
center whose half-span is the larger of the old upper and lower spans.

If a panel has no finite annotation-body center, leave its y-range unchanged.
Centering is applied after final position training and before geom drawing so
all annotation grobs use the adjusted panel transform.

## Pairwise Theme

`theme_ggexon_pairwise()` builds on `theme_ggexon_track()` and the existing
left-side strip styling. It will:

- hide the y axis used only for annotation positioning;
- retain configurable genomic x-axis and x-grid behavior;
- display horizontal panel labels on the left side outside the panels;
- keep strip backgrounds and compact plot margins consistent;
- retain configurable legend visibility.

Its arguments follow the existing track theme where useful:
`base_size`, `base_family`, `show_x_axis`, `show_x_grid`, and `show_legend`.
The first implementation will not add further styling controls.

## Compatibility and Validation

- Existing calls behave identically because `vertical` defaults to
  `"default"`.
- Invalid `vertical` values produce an argument-validation error.
- Annotation centering works with gene-tag, gene, exon, and other annotation
  layers that expose finite y-center or y-bound coordinates.
- Mixed annotation layers in one panel are centered as one visible annotation
  group.
- Empty annotation panels, linkage panels, panel x ranges, reverse-x behavior,
  axes, strips, and linkage-buffer removal remain unchanged.

## Tests and Visual Verification

Add test-first coverage that:

- proves current asymmetric annotation placement fails the new center
  expectation before implementation;
- verifies the annotation-body center equals the midpoint of each annotation
  panel's final y-range with `vertical = "center"`;
- verifies linkage panels remain `c(0, 1)`;
- verifies `vertical = "default"` preserves existing ranges;
- validates unsupported `vertical` values;
- checks `theme_ggexon_pairwise()` produces horizontal left-strip styling and
  the requested axis, grid, and legend defaults.

Render and inspect a two-annotation/one-linkage PNG using the new facet argument
and theme.

## Scope

This change adds one facet argument, one theme helper, focused tests,
documentation, and a visual example. It does not introduce a new facet class,
change annotation geom coordinates, rebuild unrelated examples, or commit any
work.
