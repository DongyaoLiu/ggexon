# Translate locus tags to gene names

Looks up each value in a named mapping vector. Tries the exact value
first; on miss, strips locus-tag isoform suffixes (trailing letter +
optional digits, e.g. `"B0250.18a"` → `"B0250.18"`) and retries.
Unmapped values are returned unchanged.

## Usage

``` r
.translate_locus_tags(x, id_map)
```

## Arguments

- x:

  Character vector of locus tags.

- id_map:

  Named character vector (`locus_tag → gene_name`).

## Value

Character vector of translated gene names.
