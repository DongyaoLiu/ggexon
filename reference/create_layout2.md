# Create the ggexon layout runtime

`create_layout2()` builds the `Layout2` ggproto object used by
[`ggexon_build()`](https://dongyaoliu.github.io/ggexon/reference/ggexon_build.md).
This runtime object sits between layers, facets, and coordinates during
plot build and is responsible for turning the facet's panel table into
panel-aware layer data.

## Usage

``` r
create_layout2(facet, coord, layout = NULL)
```

## Arguments

- facet:

  A facet ggproto object, usually `FacetGenomics` or a standard ggplot2
  facet.

- coord:

  A coordinate ggproto object.

- layout:

  Optional layout ggproto subclass. Defaults to `Layout2`.

## Value

A ggproto layout object used internally by
[`ggexon_build()`](https://dongyaoliu.github.io/ggexon/reference/ggexon_build.md).

## Details

In ggexon, `Layout2` extends the standard ggplot2 layout pipeline with
Syn- specific behavior:

- `SynSpecies` / `SynIndividual` plot data can be carried into facet
  setup without being treated as an ordinary data frame.

- a stored `SynLayout` can be supplied as an override and reused during
  build.

- link layers can trigger genomic panel reordering and panel metadata
  such as `t_panel` / `q_panel`.

- panel metadata is joined back onto layer data so geoms such as
  [`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md)
  can transform each side of a link against the correct annotation
  panel.
