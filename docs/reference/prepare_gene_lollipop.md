# Prepare a shared gene-lollipop grid

Selects native gene features and assigns a common, device-independent
grid to all participating individuals. Empty cells remain reserved.
Automatic ordinal copy correspondence is a display convention, not copy
orthology.

## Usage

``` r
prepare_gene_lollipop(
  x,
  name = "gene_lollipop",
  individuals = NULL,
  annotation = NULL,
  genes = NULL,
  where = NULL,
  key = NULL,
  homology = NULL,
  copy_key = NULL,
  reference = NULL,
  nrow = NULL,
  ncol = NULL,
  order = NULL,
  slots = NULL,
  label = NULL,
  fill = NULL
)
```

## Arguments

- x:

  A SynIndividual or SynSpecies.

- name:

  Name of the prepared grid.

- individuals:

  Optional participating individual IDs.

- annotation:

  Optional source feature-annotation name; defaults to each individual's
  active feature annotation.

- genes:

  Optional gene IDs/names, or a list named by individual.

- where:

  Named list of metadata values. Alternatives within each field are
  combined with OR; different fields are combined with AND.

- key:

  Optional metadata column identifying shared genes.

- homology:

  Optional stored homology-annotation names. Explicit `key` and explicit
  `homology` cannot be combined.

- copy_key:

  Optional metadata column identifying corresponding copies.

- reference:

  Reference individual ID. Required for multiple individuals unless a
  unique common homology reference can be inferred.

- nrow, ncol:

  Positive integer grid dimensions. Supply at least one.

- order:

  Optional complete shared-key order, overriding reference/flanks.

- slots:

  Optional partial table with `shared_key`, optional `copy_id`,
  `head_row` and `head_col`. Rows are numbered bottom to top.

- label, fill:

  Optional source metadata columns. Defaults prefer nonempty
  `plot_label` and `plot_fill`, then gene names/IDs and grey fill.

## Value

The updated Syn object. Inspect it with
[`query_gene_lollipop()`](https://dongyaoliu.github.io/ggexon/reference/query_gene_lollipop.md).
