# Declare the intended output size for a ggexon plot

`ggexon_output_size()` records the final render size that downstream
layout code can use as an output-size contract. Text annotation layout
still measures each panel viewport when possible, but this metadata
gives ggexon a stable fallback and documents the dimensions the figure
is designed for.

## Usage

``` r
ggexon_output_size(width, height, units = "in", dpi = 300)
```

## Arguments

- width, height:

  Positive numeric output dimensions.

- units:

  Unit for `width` and `height`. One of `"in"`, `"cm"`, `"mm"`, or
  `"px"`.

- dpi:

  Pixel density used when `units = "px"`.

## Value

A ggexon output-size specification.
