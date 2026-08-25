# Set the coverage-panel y-scale policy

`scale_panel_coverage()` controls how first-class BigWig coverage panels
inherit y-scale objects. `"fixed_y"` (the default) gives all coverage
panels one shared raw-depth scale; `"free_y"` gives every coverage panel
its own raw-depth scale.

## Usage

``` r
scale_panel_coverage(policy = "fixed_y", limits = NULL, auto = FALSE)
```

## Arguments

- policy:

  One non-missing string, exactly `"fixed_y"` (the default) or
  `"free_y"`.

- limits:

  `NULL` (the default), or a named list of finite, strictly increasing
  numeric length-two vectors. Names must exactly match first-class
  coverage panel names. Manual limits require `policy = "free_y"`.

- auto:

  One non-missing logical value. When `TRUE`, apply the automatic
  span-weighted median-at-85-percent rule to every first-class coverage
  panel. Automatic limits require `policy = "free_y"`. The default is
  `FALSE`.

## Value

An object of class `ggexon_panel_scale_spec` that can be added to a
ggexon plot.

## Details

With `policy = "free_y"`, `limits` can set exact visible y ranges for
selected coverage panels. Values outside a requested range are clipped
at the panel boundary, but the underlying BigWig scores and built layer
data are unchanged. Panels omitted from `limits` keep their
automatically trained ranges.

With `policy = "free_y"` and `auto = TRUE`, every first-class coverage
panel receives an automatic visible range of `c(0, Q50 / 0.85)`, where
`Q50` is the span-weighted median of the finite, non-negative rendered
coverage intervals. Inclusive genomic interval widths are used when
available, without expanding BigWig records to one row per base. This
places `Q50` at 85 percent of the panel height, so roughly half the
rendered span reaches or exceeds that line; tied coverage values can
prevent the fraction from being exactly 50 percent. Panels with no
usable positive `Q50` keep their trained ranges. Named `limits` override
automatic ranges for matching panels.

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

# Put each panel's span-weighted median at 85% of panel height.
automatic_depth <- coverage_layers +
  scale_panel_coverage("free_y", auto = TRUE) +
  center_panel_annotation() +
  theme_ggexon_track() +
  theme_ggexon_side_strips("left")

# Manual ranges override automatic ranges for matching panels.
automatic_with_override <- coverage_layers +
  scale_panel_coverage(
    "free_y",
    limits = list(ECA2091 = c(0, 200)),
    auto = TRUE
  )
```
