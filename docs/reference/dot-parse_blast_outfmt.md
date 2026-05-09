# Parse a BLAST outfmt string into column names

Strips the leading `"6 "` (tabular format specifier) and splits the
remainder on whitespace.

## Usage

``` r
.parse_blast_outfmt(outfmt)
```

## Arguments

- outfmt:

  Character string as passed to `blastp -outfmt`.

## Value

Character vector of column names.
