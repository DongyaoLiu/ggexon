# Attach a tree for genomic-panel alignment

`geom_genomic_tree()` is a ggexon-native tree layer specification. It
stores a rectangular ggtree layout on the plot so
[`facet_genomictree()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomictree.md)
can order genomic panels by tree tips and inject a single spanning tree
panel.

## Usage

``` r
geom_genomic_tree(
  tree = NULL,
  tree_plot = NULL,
  layout = "rectangular",
  individual = NULL,
  tree_width = grid::unit(1.5, "in"),
  colour = "black",
  linecolour = NULL,
  linewidth = 0.5
)
```

## Arguments

- tree:

  Optional tree object accepted by
  [`ggtree::ggtree()`](https://rdrr.io/pkg/ggtree/man/ggtree.html).

- tree_plot:

  Optional existing rectangular `ggtree` plot. If supplied, `tree` is
  ignored. When this layer is added to `ggexon(SynSpecies)`, omitted
  tree inputs are filled from the stored `SynSpecies` tree slots when
  present.

- layout:

  ggtree layout. Currently only `"rectangular"` is supported.

- individual:

  Optional individual selector. When named, names are tree tip labels
  and values are `SynSpecies` individual ids.

- tree_width:

  Grid unit width of the tree column.

- colour, linecolour:

  Tree segment colour.

- linewidth:

  Tree segment linewidth.

## Value

A ggexon tree specification consumed by
[`facet_genomictree()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomictree.md).
