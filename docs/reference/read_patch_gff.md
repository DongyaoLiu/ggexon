# Read a small GFF/GTF patch file

Imports a patch fragment and normalizes it to the same metadata schema
used by feature annotations in `ggexon`.

## Usage

``` r
read_patch_gff(path, format = c("auto", "gff", "gtf"))
```

## Arguments

- path:

  Path to a small GFF or GTF patch file.

- format:

  One of `"auto"`, `"gff"`, or `"gtf"`. Currently used for validation
  and future extension.

## Value

A normalized `GRanges` object suitable for
[`patch_annotation()`](https://dongyaoliu.github.io/ggexon/reference/patch_annotation.md).
