# Query protein-mutation annotations

Query protein-mutation annotations

## Usage

``` r
query_protein_mutations(
  x,
  annotation = NULL,
  individual = NULL,
  genes = NULL,
  event_type = NULL,
  min_sample_count = NULL,
  strains = NULL,
  mutation = NULL,
  protein_ranges = NULL,
  domains = NULL,
  protein_domains = NULL,
  ref = NULL,
  protein_length = NULL
)
```

## Arguments

- x:

  A `SynProteinMutationAnnotation`, `SynIndividual`, or `SynSpecies`.

- annotation:

  Optional annotation-layer name. Defaults to the first attached
  `SynProteinMutationAnnotation`.

- individual:

  Optional individual name(s) when `x` is a `SynSpecies`.

- genes:

  Optional gene identifiers.

- event_type:

  Optional mutation event type filter.

- min_sample_count:

  Optional minimum `sample_count`.

- strains:

  Optional strain/individual identifiers to match against a routed
  mutation table.

- mutation:

  Optional mutation labels, for example `"C316H"`.

- protein_ranges:

  Optional protein-coordinate windows such as `"100-160"`. Values
  outside the inferred or supplied protein boundaries are clamped to
  those boundaries.

- domains:

  Optional protein-domain interval table used with `protein_domains`.

- protein_domains:

  Optional domain names. Matching domain intervals are converted to
  protein-coordinate windows.

- ref:

  Optional reference amino acid filter. Values must be one-letter
  amino-acid codes.

- protein_length:

  Optional protein length used to clamp `protein_ranges`.

## Value

A `data.frame` of matching mutation records.
