# Set the annotation-panel y-scale policy

`scale_panel_annotation()` controls how first-class annotation panels
inherit y-scale objects in a Syn-aware
[`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md)
layout. With the default `"fixed_y"`, all annotation panels share one
scale object. With `"free_y"`, each annotation panel receives its own
scale object.

## Usage

``` r
scale_panel_annotation(policy = "fixed_y", limits = NULL)
```

## Arguments

- policy:

  One non-missing string, exactly `"fixed_y"` (the default) or
  `"free_y"`.

- limits:

  `NULL` (the default), or a named list of finite, strictly increasing
  numeric length-two vectors. Names must exactly match first-class
  annotation panel names. Manual limits require `policy = "free_y"`.

## Value

An object of class `ggexon_panel_scale_spec` that can be added to a
ggexon plot.

## Details

With `policy = "free_y"`, `limits` can set exact visible y ranges for
selected first-class annotation panels. Names match the layout's `track`
values. Geometry outside a requested range is clipped at the panel
boundary, but source annotation coordinates, built layer data, and
trained scale data are unchanged.

Named limits take precedence over
[`center_panel_annotation()`](https://dongyaoliu.github.io/ggexon/reference/center_panel_annotation.md)
and `facet_genomics(vertical = "center")` for matching panels.
Annotation panels omitted from `limits` retain their centered view when
centering is requested, or their normally trained view otherwise.
Coverage, link, composite coverage, tree, and ordinary non-Syn facet
panels are unchanged.

An explicit annotation policy takes precedence over the y component of
the facet's `scales` argument. A valid specification is a no-op when no
annotation panel is present. Ordinary non-Syn facets and
[`facet_genomictree()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomictree.md)
retain their existing behavior.

## See also

[`scale_panel_coverage()`](https://dongyaoliu.github.io/ggexon/reference/scale_panel_coverage.md),
[`center_panel_annotation()`](https://dongyaoliu.github.io/ggexon/reference/center_panel_annotation.md),
[`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md)

## Examples

``` r
annotation_scales <- ggexon() +
  facet_genomics(ggplot2::vars(track)) +
  scale_panel_annotation("free_y")

manual_annotation_ranges <- ggexon() +
  facet_genomics(ggplot2::vars(track)) +
  scale_panel_annotation(
    "free_y",
    limits = list(ann_a = c(-2, 2))
  ) +
  center_panel_annotation()
```
