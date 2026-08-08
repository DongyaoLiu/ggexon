# PEEL-1 Raw BigWig Coverage

## Overview

This article plots raw PEEL-1 coverage for four *C. elegans* strains
over the same chromosome-I window. The bundled fixture contains the four
BigWig files and a WS285 GTF subset for the four genes in the interval,
so the workflow is self-contained and does not refer to source BAMs or
other external paths.

The fixture BigWigs were made from one-base bins of the source BAMs
without normalization. The plotted scores remain raw depth:
[`geom_coverage()`](https://dongyaoliu.github.io/ggexon/reference/geom_coverage.md)
neither normalizes, smooths, thresholds, nor expands the stored
intervals to one row per base.

``` r

library(ggexon)

fixture_dir <- system.file("extdata", "peel1_coverage", package = "ggexon")
if (!nzchar(fixture_dir)) {
  fixture_dir <- file.path(
    dirname(dirname(knitr::current_input(dir = TRUE))),
    "inst", "extdata", "peel1_coverage"
  )
}
gtf <- file.path(
  fixture_dir,
  "WS285.ugt31-zeel1-peel1-nekl1.gtf"
)
strains <- c("XZ1516", "ECA2091", "ECA701", "ECA2191")

coverage_species <- SynSpecies(name = "PEEL-1 raw coverage")
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
      bigwig_file = file.path(fixture_dir, paste0(strain, ".raw.bw")),
      metadata = list(signal_unit = "raw_depth")
    )
  )
  coverage_species <- add_individual(coverage_species, individual)
}

ggexon(coverage_species) +
  geom_coverage(annotation = "coverage", fill = "#4C78A8") +
  geom_exon(
    chr = "I",
    subset = c(2332338L, 2373985L),
    annotation_type = "exon"
  ) +
  facet_genomics(
    ggplot2::vars(track),
    scales = "free_y",
    vertical = "center"
  ) +
  theme_ggexon_track()
```

![A ggexon plot with four PEEL-1 raw coverage tracks for strains XZ1516,
ECA2091, ECA701, and ECA2191. Each track has raw coverage above a
four-gene exon annotation band over the same chromosome-I
interval.](bigwig-coverage-demo_files/figure-html/peel1-raw-coverage-1.png)

Raw, unnormalized PEEL-1 coverage for four C. elegans strains over
chromosome I:2,332,338-2,373,985. Each attached BigWig annotation is
queried only for the displayed window.

## Reading the tracks

All four coverage panels use the same maximum raw score, making their
heights directly comparable despite the facet’s otherwise free y scales.
Each exon annotation is placed in a band from negative 25% of that
shared maximum up to zero. That band is a layout device only: it does
not make gene coordinates or coverage values negative. The coverage
therefore remains above the four-gene annotation for every strain while
the underlying BigWig scores remain raw.

For reproducibility, `manifest.tsv` in the fixture directory records
each source BAM, output file, interval, bin size, normalization setting,
and checksum. The BAM files themselves are not distributed with the
package.
