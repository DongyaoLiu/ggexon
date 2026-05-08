# Inspect effective annotation panel windows

Returns a compact table describing the annotation-panel windows
currently in effect. For a ggexon plot object, this inspects the built
plot so the result reflects the actual panel selection and x ranges
after stored layouts,
[`set_panel_xlim()`](https://dongyaoliu.github.io/ggexon/reference/set_panel_xlim.md),
explicit `subset =`, and link-derived windows have all been resolved.

## Usage

``` r
effective_panel_windows(x)
```

## Arguments

- x:

  A ggexon plot object,
  [`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md),
  or
  [`SynLayout`](https://dongyaoliu.github.io/ggexon/reference/SynLayout.md)
  object.

## Value

A `data.frame` with one row per annotation panel.

## Details

The returned columns distinguish between requested layout windows and
the ranges actually observed in built annotation data:

- `chr`, `start`, `end`: window stored on the layout, usually from
  [`set_panel_xlim()`](https://dongyaoliu.github.io/ggexon/reference/set_panel_xlim.md)
  or direct `SynLayout` edits.

- `observed_start`, `observed_end`: min/max `xmin` and `xmax` seen in
  the built annotation layers for that panel.

These values are often different. For example, if you request
`start = 0, end = 5000` on a scaffold but the first annotated exon
starts at 1283 and the last one ends at 1871, then the observed range
will be `1283..1871` even though the effective panel window remains
`0..5000`. This helper is therefore useful both for checking stored
panel windows and for seeing how much of that window is actually
occupied by annotation.
