# Set the annotation-panel y-scale policy

`scale_panel_annotation()` controls how first-class annotation panels
inherit y-scale objects in a Syn-aware
[`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md)
layout. With the default `"fixed_y"`, all annotation panels share one
scale object. With `"free_y"`, each annotation panel receives its own
scale object.

## Usage

``` r
scale_panel_annotation(policy = "fixed_y")
```

## Arguments

- policy:

  One non-missing string, exactly `"fixed_y"` (the default) or
  `"free_y"`.

## Value

An object of class `ggexon_panel_scale_spec` that can be added to a
ggexon plot.

## Details

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
```
