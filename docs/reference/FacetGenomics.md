# ggproto backend for `facet_genomics()`

`FacetGenomics` extends ggplot2's `FacetWrap` with Syn-aware layout
logic. The main custom responsibilities are:

## Usage

``` r
FacetGenomics
```

## Format

An object of class `FacetGenomics` (inherits from `FacetWrap`, `Facet`,
`ggproto`, `gg`) of length 5.

## Details

- deciding whether to use a stored `SynLayout`, derive a new comparative
  chain layout, or fall back to standard wrap-style faceting

- reordering link panels so they sit between the relevant annotation
  panels

- annotating link panels with source panel ids (`t_panel`, `q_panel`)

- assigning vertical anchor directions (`target_anchor_y`,
  `query_anchor_y`) for link layers

These panel-level decisions are consumed later by `Layout2` and
[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md).

## Key methods

- `compute_layout()`:

  Chooses the panel table. For `SynSpecies` data it prefers an explicit
  layout override, then a stored `SynLayout` when link layers are
  present, then a derived chain layout, and finally a standard wrap
  layout.

- `compute_alignment_layout()`:

  Reorders link panels relative to their neighboring annotation panels
  and annotates the resulting layout with source panel ids.

- `map_link_direction()`:

  Adds vertical link anchor metadata to link layer data based on whether
  the target species sits above or below the link panel in the resolved
  layout.

## See also

[SynLayout](https://dongyaoliu.github.io/ggexon/reference/SynLayout.md)
