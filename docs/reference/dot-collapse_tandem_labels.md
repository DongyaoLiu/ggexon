# Collapse consecutive genes with identical labels into tandem groups

For each track, genes are ordered by genomic position. Consecutive genes
that share the same `label` are merged into a single label row spanning
the full tandem array. Member positions are stored as an attribute for
connector drawing.

## Usage

``` r
.collapse_tandem_labels(data2)
```

## Arguments

- data2:

  A data frame with one row per gene, already sorted by `orig_x_mid`
  within each track.

## Value

A data frame with collapsed tandem rows plus a `"tandem_anchors"`
attribute.
