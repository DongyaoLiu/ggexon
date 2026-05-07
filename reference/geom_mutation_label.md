# Draw mutation labels as a dedicated text layer

`geom_mutation_label()` is the text companion to mutation lollipop
layers. With ordinary data frames it prepares label positions from
mutation positions. With `SynIndividual` or `SynSpecies` input it
dispatches mutation rows from attached `SynProteinMutationAnnotation`
layers.

## Usage

``` r
geom_mutation_label(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  mutations = NULL,
  annotation = NULL,
  individual = NULL,
  species = NULL,
  genes = NULL,
  event_type = NULL,
  min_sample_count = NULL,
  strains = NULL,
  mutation = NULL,
  mutation_position = "position",
  label = "mutation",
  ref = NULL,
  alt = NULL,
  spread_threshold = 7,
  mutation_y = 1,
  mutation_y_by = NULL,
  mutation_y_strategy = c("scaled", "bins"),
  mutation_y_range = c(0.85, 1.45),
  mutation_y_trans = "identity",
  mutation_y_breaks = NULL,
  mutation_y_values = NULL,
  label_nudge_y = 0.35,
  show_empty = FALSE,
  na.rm = FALSE,
  show.legend = FALSE,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard ggplot2 layer arguments.

- mutations:

  Optional mutation table. Used as `data` when `data` is `NULL`.

- annotation:

  Optional mutation annotation-layer name.

- individual:

  Optional individual selector for `SynSpecies` input.

- species:

  Alias for `individual`, matching other Syn-aware geoms.

- genes, event_type, min_sample_count, strains, mutation:

  Optional filters passed to
  [`query_protein_mutations()`](https://dongyaoliu.github.io/ggexon/reference/query_protein_mutations.md).

- mutation_position:

  Column containing mutation coordinates.

- label:

  Column used for text labels. Defaults to `"mutation"`.

- ref, alt:

  Optional residue/base columns used to build labels when `label` is
  `NULL`.

- spread_threshold:

  Minimum x-distance between adjacent labels.

- mutation_y:

  Fixed y coordinate of mutation heads when `mutation_y_by` is `NULL`.

- mutation_y_by:

  Optional numeric column used to align labels to multi-height lollipop
  heads.

- mutation_y_strategy, mutation_y_range, mutation_y_trans,
  mutation_y_breaks, mutation_y_values:

  Height-mapping controls shared with
  [`protein_lollipop_data()`](https://dongyaoliu.github.io/ggexon/reference/protein_lollipop_data.md).

- label_nudge_y:

  Vertical offset above mutation heads.

- show_empty:

  Logical; keep empty/`NA` labels when `TRUE`.

## Value

A ggplot layer.
