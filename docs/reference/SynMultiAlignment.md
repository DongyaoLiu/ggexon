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

  Alignment format. Currently `"maf"` and `"odgi"` are supported.

- data:

  Optional cached parsed alignment representation.

- metadata:

  Optional metadata list.

## Value

A `SynMultiAlignment` object.
