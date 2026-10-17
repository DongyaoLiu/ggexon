# Constructor for SynMultiAlignment

Constructor for SynMultiAlignment

## Usage

``` r
SynMultiAlignment(
  name,
  individuals,
  file,
  format = c("maf", "odgi"),
  data = NULL,
  metadata = list()
)
```

## Arguments

- name:

  Alignment label.

- individuals:

  Character vector of included individuals.

- file:

  Path to the alignment file.

- format:

  Alignment format. `SynMultiAlignment` can store already parsed MAF
  data supplied through `data`, but
  [`load_alignment()`](https://dongyaoliu.github.io/ggexon/reference/load_alignment.md)
  cannot parse a MAF file. ODGI-backed file loading is supported.

- data:

  Optional cached parsed alignment representation.

- metadata:

  Optional metadata list.

## Value

A `SynMultiAlignment` object.
