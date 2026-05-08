# ggexon Classes and Verbs

## Purpose

This tutorial introduces the main object classes and verbs in `ggexon`.

The package now has three main layers of objects:

- annotation-layer objects
- genome-level individual objects
- cross-species collection objects

The central design idea is:

- keep stable biological identifiers for computation
- add readable labels for plotting
- separate feature annotation, variants, signal, and protein-domain data
- keep species-level alignment state above individual genome state

## Object hierarchy

The current object model looks like this:

- `SynAnnotation`
  - `SynFeatureAnnotation`
  - `SynVCFAnnotation`
  - `SynBigWigAnnotation`
  - `SynProteinDomainAnnotation`
- `SynIndividual`
- `SynSpecies`
  - `SynPairAlignment`
  - `SynMultiAlignment`

## 1. Create a `SynIndividual`

`SynIndividual` is the genome-level container. It usually starts from a
genome FASTA plus one structural annotation file.

``` r
library(ggexon)

x <- SynIndividual(
  genome_file = system.file("extdata", "XZ1516.fasta", package = "ggexon"),
  annotation_file = system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  ),
  id = "XZ1516"
)
```

At construction time, `ggexon` checks whether sequence names in the
annotation file exist in the FASTA headers.

## 2. Load the active feature annotation

The default structural annotation layer is a `SynFeatureAnnotation`.

``` r
x <- load_annotation(x)
ann <- get_annotation(x)

class(ann)
annotation_names(x)
active_feature_annotation(x)
```

Feature annotations are the layers used for:

- gene/transcript/CDS queries
- CDS extraction
- protein translation
- gene-label mapping
- patching corrected models

## 3. Attach additional annotation layers

You can attach several heterogeneous annotation layers to the same
`SynIndividual`.

### Variants

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
```

### Protein domains

``` r
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

### BigWig signal

``` r
x <- add_annotation(
  x,
  SynBigWigAnnotation(
    name = "coverage",
    bigwig_file = "coverage.bw"
  )
)
```

## 4. Use annotation-layer query verbs

Different annotation subclasses have different verbs.

### Feature annotation

``` r
cds_gr <- query_features(
  x,
  genes = c("FUN_000001", "FUN_000002"),
  feature_type = "CDS"
)
```

### Variant annotation

``` r
variant_layer <- get_annotation(x, "variants")

query_variants(
  variant_layer,
  chr = "V_RagTag",
  start = 21574336,
  end = 21574450
)
```

### BigWig signal

``` r
signal_layer <- get_annotation(x, "coverage")

query_signal(
  signal_layer,
  chr = "chr1",
  start = 1,
  end = 10000
)
```

### Protein-domain annotation

``` r
domain_layer <- get_annotation(x, "interpro")

query_domains(
  domain_layer,
  ids = "FUN_000001-T1",
  domains = "PF00001"
)
```

## 5. Add readable gene labels for plotting

Structural annotation IDs are often machine-friendly but not
plot-friendly. Keep the real IDs, and add readable labels separately.

``` r
x <- set_gene_labels(
  x,
  c(
    FUN_000001 = "unc-1",
    FUN_000002 = "unc-2"
  )
)
```

This writes a `plot_label` column into the active feature annotation
while leaving the original IDs unchanged.

## 6. Patch corrected gene models

Small corrected GFF fragments can be imported and applied as gene-model
patches.

### Read a patch file

``` r
patch_gr <- read_patch_gff(
  system.file("extdata", "XZ1516.TA.gff", package = "ggexon")
)
```

### Apply a patch directly from file

``` r
x <- patch_annotation_from_gff(
  x,
  patch_file = system.file("extdata", "XZ1516.TA.gff", package = "ggexon"),
  mode = "replace",
  name = "ta-correction"
)
```

### Inspect or clear patch history

``` r
list_patches(x)

x <- clear_patches(x)
```

Supported patch modes are:

- `replace`
- `add`
- `drop`

## 7. Translate proteins from selected genes

Protein translation is built on the active `SynFeatureAnnotation`.

``` r
x <- translate_protein(
  x,
  genes = c("FUN_000001", "FUN_000002")
)

protein_seq(get_annotation(x))
```

You can also extract the CDS nucleotide sequences directly:

``` r
x <- extract_cds_seq(
  x,
  genes = c("FUN_000001", "FUN_000002")
)

nucleotide_seq(get_annotation(x))
```

## 8. Build a `SynSpecies` object

`SynSpecies` is the container for multiple `SynIndividual` objects and
their alignment relationships.

``` r
x2 <- SynIndividual(
  genome_file = other_genome_fasta,
  annotation_file = other_annotation_gff,
  id = "N2"
)

sp <- SynSpecies(name = "Caenorhabditis")
sp <- add_individual(sp, x)
sp <- add_individual(sp, x2)
```

## 9. Add alignment layers

### Pairwise alignment

For a PAF-like alignment, store the direction explicitly:

``` r
sp <- add_pairwise_alignment(
  sp,
  SynPairAlignment(
    name = "XZ1516_vs_N2",
    query_individual = "XZ1516",
    target_individual = "N2",
    file = system.file("extdata", "V_alginment.paf", package = "ggexon")
  )
)
```

### Multiple alignment

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

## 10. Recommended workflow pattern

A good working pattern in `ggexon` is:

1.  create a `SynIndividual`
2.  load the active feature annotation
3.  attach optional VCF, BigWig, or domain layers
4.  set readable labels
5.  patch corrected gene models if needed
6.  derive CDS or protein sequences
7.  collect several individuals inside a `SynSpecies`
8.  attach PAF or MAF alignment layers
9.  build comparative plots

## Notes for developers

- `SynFeatureAnnotation` is the only annotation layer currently used for
  CDS extraction and protein translation.
- `SynVCFAnnotation` and `SynBigWigAnnotation` are designed for lazy,
  region-based queries.
- `SynProteinDomainAnnotation` is table-backed and filtered by
  ID/domain.
- `SynSpecies` keeps alignment state above individual genome state.

## See also

- [`vignette("ggexon-workflow", package = "ggexon")`](https://dongyaoliu.github.io/ggexon/articles/ggexon-workflow.md)
- `docs/object-model.md`
