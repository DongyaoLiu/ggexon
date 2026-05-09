# Rank BLAST hits for deduplication

Sorts a BLAST data frame by one or more ranking columns so that the best
hit per query appears first. The sort direction for `"evalue"` is
ascending (lower is better); all other columns are sorted descending
(higher is better).

## Usage

``` r
.rank_blast_hits(blast_df, rank_by = "bitscore")
```

## Arguments

- blast_df:

  A data frame with at minimum `normalized_query` and the columns named
  in `rank_by`.

- rank_by:

  Character vector of column names to sort by.

## Value

The input data frame, sorted.
