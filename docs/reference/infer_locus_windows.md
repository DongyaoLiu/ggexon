# Infer comparable locus windows from Syn annotations

Searches each individual in a `SynSpecies` for direct focal genes and,
when missing, falls back to anchor genes to infer a local syntenic
window. This helper returns a `SynLocusSet`; attach it with
[`add_locus_set()`](https://dongyaoliu.github.io/ggexon/reference/add_locus_set.md)
and call
[`use_locus_grid()`](https://dongyaoliu.github.io/ggexon/reference/use_locus_grid.md)
to store the corresponding panel layout.

## Usage

``` r
infer_locus_windows(
  x,
  loci,
  anchors = NULL,
  name = "locus_windows",
  individual = NULL,
  reference = NULL,
  flank = 450000,
  annotation = NULL,
  feature_type = "gene",
  prefix_anchors = TRUE,
  missing = c("drop", "error")
)
```

## Arguments

- x:

  A `SynSpecies` object.

- loci:

  Character vector of focal loci / grid columns.

- anchors:

  Optional character vector used for every locus, or a named list of
  anchor genes keyed by `loci`.

- name:

  Locus-set name.

- individual:

  Optional individuals to include. Defaults to all.

- reference:

  Optional reference individual recorded in metadata.

- flank:

  Number of bases to add around direct focal genes and anchor clusters.

- annotation:

  Optional feature annotation layer to query.

- feature_type:

  Feature type used for gene rows. Defaults to `"gene"`.

- prefix_anchors:

  Treat anchor names as gene-prefixes. Useful for gene families such as
  `NBPF`.

- missing:

  What to do when neither a focal gene nor anchors are found.

## Value

A `SynLocusSet` object.
