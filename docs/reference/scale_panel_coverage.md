# Set the coverage-panel y-scale policy

`scale_panel_coverage()` controls how first-class BigWig coverage panels
inherit y-scale objects. `"fixed_y"` (the default) gives all coverage
panels one shared raw-depth scale; `"free_y"` gives every coverage panel
its own raw-depth scale.

## Usage

``` r
scale_panel_coverage(policy = "fixed_y")
```

## Arguments

- policy:

  One non-missing string, exactly `"fixed_y"` (the default) or
  `"free_y"`.

## Value

An object of class `ggexon_panel_scale_spec` that can be added to a
ggexon plot.

## Details

When this wrapper is absent, the y component of
[`facet_genomics(scales = ...)`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md)
supplies the coverage fallback: `"fixed"` and `"free_x"` mean
`"fixed_y"`, while `"free_y"` and `"free"` mean `"free_y"`. Annotation
panels remain `"fixed_y"` by default. An explicit coverage policy takes
precedence over that fallback, and a valid specification is a no-op when
no coverage panel is present. Ordinary non-Syn facets and
[`facet_genomictree()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomictree.md)
are unchanged.

## See also

[`scale_panel_annotation()`](https://dongyaoliu.github.io/ggexon/reference/scale_panel_annotation.md),
[`center_panel_annotation()`](https://dongyaoliu.github.io/ggexon/reference/center_panel_annotation.md),
[`geom_coverage()`](https://dongyaoliu.github.io/ggexon/reference/geom_coverage.md),
[`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md)

## Examples

``` r
fixture_dir <- system.file("extdata", "peel1_coverage", package = "ggexon")
gtf <- file.path(
  fixture_dir,
  "WS285.ugt31-zeel1-peel1-nekl1.gtf"
)
strains <- c("XZ1516", "ECA2091", "ECA701", "ECA2191")

coverage_species <- SynSpecies(name = "PEEL-1 coverage")
for (strain in strains) {
  individual <- SynIndividual(
    annotation_file = gtf,
    annotation_format = "gtf",
    id = strain
  )
  individual <- add_annotation(
    individual,
    SynBigWigAnnotation(
      name = "coverage",
      bigwig_file = file.path(fixture_dir, paste0(strain, ".raw.bw"))
    )
  )
  coverage_species <- add_individual(coverage_species, individual)
}

coverage_layers <- ggexon(coverage_species) +
  geom_coverage(annotation = "coverage") +
  geom_exon(
    species = "XZ1516",
    chr = "I",
    subset = c(2332338L, 2373985L)
  ) +
  facet_genomics(
    ggplot2::vars(track),
    ncol = 1,
    strip.position = "left"
  )

# One shared raw-depth scale across all coverage panels.
shared_depth <- coverage_layers + scale_panel_coverage("fixed_y")

# One raw-depth scale per coverage panel, plus a centered gene model.
free_depth <- coverage_layers +
  scale_panel_coverage("free_y") +
  center_panel_annotation() +
  theme_ggexon_track() +
  theme_ggexon_side_strips("left")
```
