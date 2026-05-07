# Constructor for SynBigWigAnnotation

Constructor for SynBigWigAnnotation

## Usage

``` r
SynBigWigAnnotation(name, bigwig_file, metadata = list(), lazy = TRUE)
```

## Arguments

- name:

  Short unique label for the annotation layer.

- bigwig_file:

  Path to the BigWig file.

- metadata:

  Optional metadata list.

- lazy:

  Logical; defaults to `TRUE` for windowed loading.

## Value

A `SynBigWigAnnotation` object.
