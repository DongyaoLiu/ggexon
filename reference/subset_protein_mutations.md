# Subset protein-mutation tables

`subset_protein_mutations()` filters normalized protein-mutation data
frames by individual/strain IDs, protein-coordinate windows, domain
names, and reference amino acids. Protein-domain filters are converted
to coordinate windows before filtering.

## Usage

``` r
subset_protein_mutations(
  mutations,
  individuals = NULL,
  protein_ranges = NULL,
  domains = NULL,
  protein_domains = NULL,
  ref = NULL,
  position = "position",
  protein_start = 1,
  protein_length = NULL,
  domain_start = "start",
  domain_end = "end",
  domain = NULL,
  individual_index = attr(mutations, "individual_index", exact = TRUE)
)
```

## Arguments

- mutations:

  A data frame returned by
  [`read_protein_mutation_counts()`](https://dongyaoliu.github.io/ggexon/reference/read_protein_mutation_counts.md)
  or another data frame with at least a protein-position column.

- individuals:

  Optional strain/species/individual IDs. A single string, character
  vector, or list is accepted.

- protein_ranges:

  Optional protein-coordinate windows such as `"10-50"`. A single
  string, character vector, or list is accepted. Bounds are validated
  and clamped to the inferred protein boundaries.

- domains:

  Optional protein-domain interval table,
  [`S4Vectors::DataFrame`](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html),
  or `SynProteinDomainAnnotation` used with `protein_domains`.

- protein_domains:

  Optional domain names. Matching domain intervals are treated like
  protein-coordinate windows.

- ref:

  Optional reference amino acids. Values are matched against `ref` or
  common reference-amino-acid columns.

- position:

  Mutation protein-coordinate column.

- protein_start:

  Lower protein-coordinate boundary.

- protein_length:

  Optional protein length used as the upper coordinate boundary. When
  `NULL`, the upper boundary is inferred from mutation and domain
  coordinates.

- domain_start, domain_end:

  Domain interval start/end columns.

- domain:

  Optional domain-name column. When omitted, a common domain column name
  is inferred.

- individual_index:

  Optional long mutation-individual index. Defaults to the
  `individual_index` attribute created by
  [`read_protein_mutation_counts()`](https://dongyaoliu.github.io/ggexon/reference/read_protein_mutation_counts.md).

## Value

A filtered mutation `data.frame`.
