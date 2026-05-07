# Compile a tree-aligned genomic track layout

`compile_ggtree_genomic_alignment()` prepares a ggalign-style layout
where a rectangular tree provides the shared tip y positions and each
matched `SynSpecies` individual keeps its own genomic x coordinate
system.

## Usage

``` r
compile_ggtree_genomic_alignment(
  x,
  tree = NULL,
  tree_plot = NULL,
  layout = "rectangular",
  individual = NULL,
  chr = NULL,
  start = NULL,
  end = NULL,
  subset = NULL,
  feature_type = "gene",
  inter_genetic = c("scaled", "union"),
  exon_length = c("scaled", "union"),
  tree_track = "Tree"
)
```

## Arguments

- x:

  A `SynSpecies` object.

- tree:

  Optional tree object accepted by
  [`ggtree::ggtree()`](https://rdrr.io/pkg/ggtree/man/ggtree.html).

- tree_plot:

  Optional existing rectangular `ggtree` plot. If supplied, `tree` is
  ignored. When both `tree` and `tree_plot` are omitted, stored values
  on `x` are used when present.

- layout:

  ggtree layout. Currently only `"rectangular"` is supported.

- individual:

  Optional individual selector. When named, names are tree tip labels
  and values are `SynSpecies` individual ids.

- chr:

  Optional chromosome/seqname. May be scalar or named by tree tip or
  individual id.

- start, end:

  Optional coordinate bounds. May be scalar or named by tree tip or
  individual id.

- subset:

  Optional numeric length-2 bounds. May be a scalar vector or a named
  list keyed by tree tip or individual id. Overrides `start` and `end`.

- feature_type:

  Feature type passed to
  [`query_features()`](https://dongyaoliu.github.io/ggexon/reference/query_features.md).
  Defaults to `"gene"`.

- inter_genetic, exon_length:

  Layout modes passed to
  [`compile_ggtree_genetag()`](https://dongyaoliu.github.io/ggexon/reference/compile_ggtree_genetag.md).

- tree_track:

  Facet-track label assigned to tree segments.

## Value

A `ggtree_genomic_alignment` list with `tip_layout`, `tree_segments`,
and `gene_tags` data frames.
