# Compute and store the ggexon chain layout on a `SynSpecies`

Compute and store the ggexon chain layout on a `SynSpecies`

## Usage

``` r
store_chain_layout(
  x,
  vars = ggplot2::vars(track),
  free = list(x = FALSE, y = FALSE)
)
```

## Arguments

- x:

  A `SynSpecies` object.

- vars:

  Facet vars. Defaults to `ggplot2::vars(track)`.

- free:

  List with logical `x` and `y` entries controlling scale grouping.

## Value

The updated `SynSpecies` object.
