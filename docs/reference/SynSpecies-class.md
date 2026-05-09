# SynSpecies class

`SynSpecies` is the top-level comparative container in `ggexon`. It
groups named `SynIndividual` objects together with any stored pairwise
or multiple alignments, optional tree/tree plot objects, optional
metadata, and an optional reusable `SynLayout`.

## Slots

- `name`:

  Scalar species-collection label.

- `individuals`:

  Named list of `SynIndividual` objects.

- `pairwise_alignments`:

  Named list of `SynPairAlignment` objects.

- `multiple_alignments`:

  Named list of `SynMultiAlignment` objects.

- `tree`:

  Optional tree object, such as an
  [`ape::phylo`](https://rdrr.io/pkg/ape/man/read.tree.html).

- `tree_plot`:

  Optional tree plot object, such as a rectangular `ggtree` plot.

- `metadata`:

  Optional user or import metadata.

- `layout`:

  Optional stored `SynLayout` used by
  [`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md)
  and syn-aware plot building.

- `homology_annotations`:

  Named list of `HomologyAnnotation` objects storing cross-species gene
  homology mappings.

## Prototype defaults

- `individuals = list()`

- `pairwise_alignments = list()`

- `multiple_alignments = list()`

- `homology_annotations = list()`

- `tree = NULL`

- `tree_plot = NULL`

- `metadata = list()`

- `layout = NULL`

## Validity rules

- `name` must be one non-empty character value.

- `individuals` must contain only `SynIndividual` objects.

- `pairwise_alignments` must contain only `SynPairAlignment` objects.

- `multiple_alignments` must contain only `SynMultiAlignment` objects.

- `homology_annotations` must contain only `HomologyAnnotation` objects.

- `layout` must be either `NULL` or a `SynLayout`.
