# Compile rectangular ggtree branches for a ggexon tree panel

`compile_ggtree_rectangular_segments()` converts a rectangular ggtree
plot or tree object into plain segment rows. The returned data can be
drawn with
[`ggplot2::geom_segment()`](https://rdrr.io/pkg/ggplot2/man/geom_segment.html)
inside
[`ggexon()`](https://dongyaoliu.github.io/ggexon/reference/ggexon.md)
and aligned to a gene-tag panel with
[`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md).

## Usage

``` r
compile_ggtree_rectangular_segments(
  tree = NULL,
  tree_plot = NULL,
  layout = "rectangular",
  track = "Tree"
)
```

## Arguments

- tree:

  Optional tree object accepted by
  [`ggtree::ggtree()`](https://rdrr.io/pkg/ggtree/man/ggtree.html).

- tree_plot:

  Optional existing `ggtree` plot. If supplied, `tree` is ignored.

- layout:

  ggtree layout. Currently only `"rectangular"` is supported.

- track:

  Facet-track value assigned to all branch segments.

## Value

A `data.frame` with `track`, `x`, `xend`, `y`, `yend`, `node`, `parent`,
`segment`, `isTip`, and `label` columns.
