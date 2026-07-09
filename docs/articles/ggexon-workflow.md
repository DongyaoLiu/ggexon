# ggexon Workflow

## Overview

`ggexon` is designed for synteny-aware genome plotting with support for
multiple annotation layers, derived CDS and protein sequences, and
patch-based curation workflows.

This vignette sketches the core workflow:

1.  Create a `SynIndividual`
2.  Load a feature annotation
3.  Attach additional annotation layers
4.  Set human-readable labels
5.  Patch corrected gene models
6.  Translate proteins from selected genes
7.  Combine multiple individuals in a `SynSpecies` object
8.  Query and filter pairwise alignments
9.  Plot directly from a `SynSpecies` container

## Create a `SynIndividual`

``` r

library(ggexon)

x <- SynIndividual(
  genome_file = system.file("extdata", "XZ1516.fasta", package = "ggexon"),
  annotation_file = system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
)
```

## Load the active feature annotation

``` r

x <- load_annotation(x)
ann <- get_annotation(x)
```

At this point the active layer is a `SynFeatureAnnotation`.

## Attach additional annotation layers

You can attach variant, signal, and protein-domain layers to the same
`SynIndividual`.

``` r

x <- add_annotation(
  x,
  SynVCFAnnotation(
    name = "variants",
    vcf_file = system.file(
      "extdata",
      "DL238.rename.ChrV.XZ1516.vcf.gz",
      package = "ggexon"
    )
  )
)

x <- add_annotation(
  x,
  SynProteinDomainAnnotation(
    name = "interpro",
    domain_file = system.file(
      "extdata",
      "InterProScan.tsv",
      package = "ggexon"
    ),
    keytype = "protein_id",
    source_db = "InterPro"
  )
)
```

## Set plot labels

Stable IDs should remain unchanged. Use
[`set_gene_labels()`](https://dongyaoliu.github.io/ggexon/reference/set_gene_labels.md)
to add readable labels for plotting.

``` r

x <- set_gene_labels(
  x,
  c(
    FUN_000001 = "unc-1",
    FUN_000002 = "unc-2"
  )
)
```

The active feature annotation will then carry a `plot_label` metadata
column.

## Apply a corrected patch from a small GFF

Small corrected GFF fragments can be imported and applied as structured
patches.

``` r

x <- patch_annotation_from_gff(
  x,
  patch_file = system.file("extdata", "XZ1516.TA.gff", package = "ggexon"),
  mode = "replace",
  name = "ta-correction"
)
```

This replaces matching target gene models in the active feature
annotation and clears stale sequence caches.

## Translate proteins for selected genes

``` r

x <- translate_protein(
  x,
  genes = c("FUN_000001", "FUN_000002")
)

protein_seq(get_annotation(x))
```

## Query attached non-feature layers

Region-based layers are queried through their type-specific verbs.

``` r

variant_layer <- get_annotation(x, "variants")
query_variants(variant_layer, chr = "V_RagTag", start = 21574336, end = 21574400)
```

## Group individuals in a `SynSpecies`

Once you have multiple genomes or species, `SynSpecies` can hold both
the `SynIndividual` objects and their cross-species alignments.

``` r

x2 <- SynIndividual(other_genome_file, other_annotation_file)

sp <- SynSpecies(name = "Caenorhabditis")
sp <- add_individual(sp, x)
sp <- add_individual(sp, x2)

sp <- add_pairwise_alignment(
  sp,
  SynPairAlignment(
    name = "XZ1516_vs_other",
    query_individual = "XZ1516",
    target_individual = syn_id(x2),
    file = "XZ1516_vs_other.paf"
  )
)
```

For multiple-species alignments:

``` r

sp <- add_multiple_alignment(
  sp,
  SynMultiAlignment(
    name = "worm-maf",
    individuals = c("XZ1516", "N2", "CB4856"),
    file = "worms.maf"
  )
)
```

## Query pairwise alignments

Pairwise alignments can now be read back as PAF-like tables from either
a `SynSpecies` object or a `SynPairAlignment` object.

Use `subset =` to trim on both query and target coordinates, and use
`filter =` to discard short fragments by `alen`.

``` r

pairwise_alignment_data(
  sp,
  alignment = "XZ1516_vs_other",
  subset = c(
    XZ1516 = "RagTag_V:21550000-21680000",
    other = "V:20450000-20510000"
  ),
  filter = 200
)
```

The helper wrappers
[`subset_pairwise_alignment()`](https://dongyaoliu.github.io/ggexon/reference/subset_pairwise_alignment.md)
and
[`filter_pairwise_alignment()`](https://dongyaoliu.github.io/ggexon/reference/filter_pairwise_alignment.md)
expose the same operations separately when that reads more clearly in
user code.

## Plot directly from a `SynSpecies`

You can now pass a `SynSpecies` object directly to
[`ggexon()`](https://dongyaoliu.github.io/ggexon/reference/ggexon.md)
and let a syn-aware geom resolve the plotting data lazily at build time.

For example, this draws the exon models for `XZ1516` across the
`RagTag_V:21550000-21680000` region. Because this `SynSpecies` contains
only one individual, you do not need to specify `species =`. Internally,
the plotting code resolves that sole individual, maps the user-facing
chromosome label `RagTag_V` to the annotation seqname used in the GFF
(`V_RagTag` in the bundled example), and creates the default `aes()`
mapping needed by
[`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md).

``` r

x <- SynIndividual(
  genome_file = system.file("extdata", "XZ1516.fasta", package = "ggexon"),
  annotation_file = system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  ),
  id = "XZ1516"
)

sp <- SynSpecies(name = "Caenorhabditis")
sp <- add_individual(sp, x)

ggexon(sp) +
  geom_exon(
    chr = "RagTag_V",
    subset = c(21550000, 21680000)
  )
```

If a `SynSpecies` contains multiple individuals, then `species =`
becomes required so the geom knows which one to plot.

The same direct plotting workflow is also available for
[`geom_genetag()`](https://dongyaoliu.github.io/ggexon/reference/geom_genetag.md),
which collapses the region to one directional span per `gene_id`.

``` r

ggexon(sp) +
  geom_genetag(
    chr = "RagTag_V",
    subset = c(21550000, 21680000)
  )
```

Use the two geoms for different levels of detail:

- [`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md)
  keeps exon-level structure and is useful when transcript architecture
  matters.
- [`geom_genetag()`](https://dongyaoliu.github.io/ggexon/reference/geom_genetag.md)
  reduces each gene to a start-to-end span with an arrow showing strand
  direction.

Both geoms now use absolute genomic coordinates, so the x-axis stays in
the original coordinate system of the chromosome rather than being
re-based to a local zero point for each track.

For pairwise comparison, draw the annotation tracks and then let
[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md)
resolve the stored alignment into the middle link panel. The reference
species supplies the input coordinates.

``` r

ggexon(sp) +
  geom_exon(
    species = c("N2", "XZ1516"),
    chr = "RagTag_V",
    subset = c(21574445, 21584356)
  ) +
  geom_nuclink(
    reference = "XZ1516",
    chr = "RagTag_V",
    subset = c(21574445, 21584356),
    alignment = "XZ1516_vs_N2"
  ) +
  facet_genomics(ggplot2::vars(track), scales = "free_y")
```

For custom pairwise views, the PAF helper and the plotting grammar are
meant to work together: derive or specify the windows you want, filter
short PAF rows with `filter =`, and then draw the comparison.

At the moment, this direct plotting path is implemented for
[`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md),
[`geom_genetag()`](https://dongyaoliu.github.io/ggexon/reference/geom_genetag.md),
and
[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md).
The same pattern can be extended to other geoms so they can consume
`SynSpecies` or `SynIndividual` objects without requiring a manual
`fortify()` step.

## Notes

- `SynFeatureAnnotation` is the structural layer used for CDS extraction
  and protein translation.
- `SynVCFAnnotation` and `SynBigWigAnnotation` are designed for lazy,
  region-based access.
- `SynProteinDomainAnnotation` provides protein-space annotation tied to
  transcript or protein identifiers.
- `SynSpecies` is the comparison container that binds individuals and
  alignments together.

## Next steps

Suggested follow-up material for this vignette:

- comparing two feature annotation layers on the same genome
- plotting patched genes before and after correction
- combining synteny plots with variant and signal layers
