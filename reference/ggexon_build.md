# Build a ggexon plot

`ggexon_build()` is ggexon's plot-build generic. It mirrors ggplot2's
build pipeline while returning ggexon-specific built objects internally
and plain ggplot2 built objects through `ggplot_build.ggexon()`.

## Usage

``` r
ggexon_build(plot, ...)
```

## Arguments

- plot:

  A ggexon plot object.

- ...:

  Passed through to methods.
