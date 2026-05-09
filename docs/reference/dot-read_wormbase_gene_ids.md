# Read a WormBase gene ID mapping file

Parses a file like `c_elegans.PRJNA13758.WS285.geneIDs.txt`. Expected
columns: tax_id, WBGeneID, gene_name, locus_tag, status, type. Returns a
named vector `locus_tag → gene_name`.

## Usage

``` r
.read_wormbase_gene_ids(path)
```

## Arguments

- path:

  Path to the gene ID file.

## Value

A named character vector.
