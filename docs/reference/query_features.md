# Query annotation features from a SynIndividual object

Query annotation features from a SynIndividual object

## Usage

``` r
query_features(
  x,
  genes = NULL,
  transcripts = NULL,
  chr = NULL,
  start = NULL,
  end = NULL,
  feature_type = "CDS",
  all = FALSE
)
```

## Arguments

- x:

  A `SynIndividual` or `SynFeatureAnnotation` object.

- genes:

  Optional character vector of gene names or gene identifiers.

- transcripts:

  Optional character vector of transcript identifiers.

- chr:

  Optional chromosome name.

- start:

  Optional start coordinate.

- end:

  Optional end coordinate.

- feature_type:

  Feature type to return. Defaults to `"CDS"`.

- all:

  Logical; when `TRUE`, return all matching `feature_type` records.

## Value

A `GRanges` object containing the requested features.

## Details

When `chr` is supplied and the annotation has not yet been fully loaded,
`query_features()` attempts a region-restricted import first. This is
especially helpful for large indexed `.gff3.gz` or `.gtf.gz` files.

If a reusable lookup index has been created with
[`build_feature_index()`](https://dongyaoliu.github.io/ggexon/reference/build_feature_index.md),
`query_features()` uses it to accelerate repeated gene-, transcript-,
seqname-, and type-based filtering on fully loaded annotations.
