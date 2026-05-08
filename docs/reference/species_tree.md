# Access tree objects stored on a `SynSpecies`

`species_tree()` returns the stored raw tree object, such as an
[`ape::phylo`](https://rdrr.io/pkg/ape/man/read.tree.html).
`species_tree_plot()` returns the stored rectangular `ggtree` plot.

## Usage

``` r
species_tree(x)

species_tree_plot(x)
```

## Arguments

- x:

  A `SynSpecies` object.

## Value

The stored tree or tree plot object, or `NULL`.
