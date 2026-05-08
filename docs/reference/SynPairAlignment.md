# Constructor for SynPairAlignment

Constructor for SynPairAlignment

## Usage

``` r
SynPairAlignment(
  name,
  query_individual,
  target_individual,
  file,
  format = c("paf", "psl", "odgi"),
  data = NULL,
  metadata = list()
)
```

## Arguments

- name:

  Alignment label.

- query_individual:

  Query-side individual name.

- target_individual:

  Target-side individual name.

- file:

  Path to the alignment file.

- format:

  Alignment format. Currently `"paf"`, `"psl"`, or `"odgi"`.

- data:

  Optional cached parsed alignment representation. For PSL files this
  can be either one row per PSL record or one row per ungapped block.

- metadata:

  Optional metadata list. This may include loader state such as
  `psl_more`, although that value is normally managed by
  [`load_alignment()`](https://dongyaoliu.github.io/ggexon/reference/load_alignment.md).

## Value

A `SynPairAlignment` object.

## Details

For PSL-backed alignments, use
[`load_alignment()`](https://dongyaoliu.github.io/ggexon/reference/load_alignment.md)
with `more = TRUE` when you want the cached alignment table expanded to
one row per ungapped block instead of one row per PSL record.
