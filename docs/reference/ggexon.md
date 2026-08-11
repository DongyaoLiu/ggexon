# Create a ggexon plot

`ggexon()` starts a ggplot-like object for genomic annotations, synteny,
and associated track data. It follows the same basic shape as
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html),
while preserving `SynIndividual` and `SynSpecies` containers so ggexon
layers can resolve their plotting data lazily during plot build.

## Usage

``` r
ggexon(data = NULL, mapping = aes(), ..., environment = parent.frame())

# Default S3 method
ggexon(data = NULL, mapping = aes(), ..., environment = parent.frame())
```

## Arguments

- data:

  A data frame, `SynIndividual`, `SynSpecies`, or another object that
  can be fortified for plotting.

- mapping:

  Default aesthetic mappings created by
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).

- ...:

  Additional arguments passed to `fortify()` when `data` is not a ggexon
  Syn object.

- environment:

  Plot environment. Defaults to the caller environment.

## Value

A ggexon plot object inheriting from `ggplot`.

## Panel specifications

Add
[`scale_panel_annotation()`](https://dongyaoliu.github.io/ggexon/reference/scale_panel_annotation.md),
[`scale_panel_coverage()`](https://dongyaoliu.github.io/ggexon/reference/scale_panel_coverage.md),
and
[`center_panel_annotation()`](https://dongyaoliu.github.io/ggexon/reference/center_panel_annotation.md)
to configure Syn-aware panels. ggexon stores those requests internally
on the plot and resolves them during build; the corresponding plot
properties are not intended for direct assignment.

## See also

[`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md),
[`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md),
[`SynIndividual()`](https://dongyaoliu.github.io/ggexon/reference/SynIndividual.md),
[`SynSpecies()`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
