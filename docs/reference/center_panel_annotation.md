# Center annotation panels

`center_panel_annotation()` requests a post-training view adjustment for
annotation panels in a Syn-aware plot. Each annotation panel's visible y
range is made symmetric around its annotation bodies. Built annotation
data and inherited scale training are not changed, and coverage and link
panel ranges are left alone. Ordinary non-Syn facets are unchanged.

## Usage

``` r
center_panel_annotation()
```

## Value

An object of class `ggexon_annotation_center_spec` that can be added to
a ggexon plot.

## Details

This is a panel-view operation, not a ggplot2 `position_*()` adjustment.
It is the dedicated Syn-aware equivalent of
`facet_genomics(vertical = "center")`; adding both to a Syn-backed plot
applies the same operation once. Repeated additions are idempotent, and
a plot without Syn annotation panels is unchanged.

## See also

[`scale_panel_annotation()`](https://dongyaoliu.github.io/ggexon/reference/scale_panel_annotation.md),
[`scale_panel_coverage()`](https://dongyaoliu.github.io/ggexon/reference/scale_panel_coverage.md),
[`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md)

## Examples

``` r
centered_annotation <- ggexon() +
  facet_genomics(ggplot2::vars(track)) +
  center_panel_annotation()
```
