# Compile gene-tag rows for a rectangular ggtree panel

`compile_ggtree_genetag()` extracts rectangular-layout tip positions
from a ggtree plot or tree object, matches tip labels to `SynSpecies`
individuals, and returns one row per gene feature. The first column is
`id`, matching ggtree's `facet_plot()` convention.

## Usage

``` r
compile_ggtree_genetag(
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
  include_y = FALSE
)
```

## Arguments

- x:

  A `SynSpecies` object.

- tree:

  Optional tree object accepted by
  [`ggtree::ggtree()`](https://rdrr.io/pkg/ggtree/man/ggtree.html).

- tree_plot:

  Optional existing `ggtree` plot. If supplied, `tree` is ignored. When
  both `tree` and `tree_plot` are omitted, stored values on `x` are used
  when present.

- layout:

  ggtree layout. Currently only `"rectangular"` is supported.

- individual:

  Optional individual selector. When named, names are tree tip labels
  and values are `SynSpecies` individual ids. When unnamed, values are
  used as both tip labels and individual ids. When `NULL`, matching uses
  identical tree-tip labels and individual ids.

- chr:

  Optional chromosome/seqname. May be a scalar or a named vector/list
  keyed by tree tip or individual id.

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

- inter_genetic:

  Intergenic-gap layout mode. `"scaled"` keeps the original gap between
  consecutive features within each track. `"union"` uses the maximum gap
  observed at each feature step so corresponding gaps are the same
  across tracks in the same panel.

- exon_length:

  Feature-length layout mode. `"scaled"` keeps original feature lengths.
  `"union"` uses the maximum feature length observed at each feature
  step so corresponding features have the same displayed length across
  tracks in the same panel.

- include_y:

  Logical; when `TRUE`, also include a `y` column copied from `tree_y`.
  Keep the default `FALSE` for
  [`ggtree::facet_plot()`](https://rdrr.io/pkg/ggtree/man/facet-plot.html),
  which injects its own `y` column after tip matching.

## Value

A `data.frame` ready for
[`ggtree::facet_plot()`](https://rdrr.io/pkg/ggtree/man/facet-plot.html)
and
[`geom_genetag()`](https://dongyaoliu.github.io/ggexon/reference/geom_genetag.md).
It contains `id`, `individual`, `tree_y`, `xmin`, `xmax`, `strand`, and
gene metadata columns.
