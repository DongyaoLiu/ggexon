# Add a tree or tree plot to a `SynSpecies` object

`add_tree()` stores one tree representation on a `SynSpecies`. The input
can be a single tree file path, a tree object from `ape`, `tidytree`, or
`treeio`, or a rectangular `ggtree` plot. If a new tree object is
stored, any previous stored tree plot is cleared; if a new tree plot is
stored, any previous raw tree object is cleared.

## Usage

``` r
add_tree(x, tree = NULL, tree_file = NULL, tree_plot = NULL, ...)
```

## Arguments

- x:

  A `SynSpecies` object.

- tree:

  Optional tree object. Supported inputs include
  [`ape::phylo`](https://rdrr.io/pkg/ape/man/read.tree.html),
  `tidytree::tbl_tree`,
  [`treeio::treedata`](https://rdrr.io/pkg/tidytree/man/treedata.html),
  or a `ggtree` plot. A single character value is treated as
  `tree_file`.

- tree_file:

  Optional single tree-file path. Newick files are read with
  [`ape::read.tree()`](https://rdrr.io/pkg/ape/man/read.tree.html) and
  Nexus files with
  [`ape::read.nexus()`](https://rdrr.io/pkg/ape/man/read.nexus.html).

- tree_plot:

  Optional rectangular `ggtree` plot.

- ...:

  Reserved for future tree reader options.

## Value

The updated `SynSpecies` object.
