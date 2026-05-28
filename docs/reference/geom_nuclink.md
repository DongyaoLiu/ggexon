# Draw cross-panel nucleotide links

`geom_nuclink()` draws filled polygons that connect one interval on a
query genome to one interval on a target genome. It can be used with an
ordinary data frame containing explicit query/target columns, or with a
`SynSpecies` object where link rows are resolved lazily from stored
alignments.

## Usage

``` r
geom_nuclink(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  alignment = NULL,
  reference = NULL,
  chr = NULL,
  subset = NULL,
  filter_by_len = NULL,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`ggplot2::aes()`](https://rdrr.io/pkg/ggplot2/man/aes.html).
  `colour`, `fill`, and `alpha` can be mapped in the standard ggplot2
  way.

- data:

  A data frame or a `SynSpecies` object.

- stat, position:

  Standard ggplot2 layer arguments.

- ...:

  Additional parameters passed to the layer, including fixed aesthetics
  such as `fill`, `colour`, and `alpha`.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are removed silently.

- show.legend:

  Logical. Should this layer be included in the legend?

- alignment:

  Optional alignment name when `data` is a `SynSpecies`. This can refer
  to a stored `SynPairAlignment` or an ODGI `SynMultiAlignment`.

- reference:

  Optional reference species used when deriving a comparative window
  from `chr` + `subset`. For ODGI multiple alignments, this also seeds
  the greedy comparison-panel order: starting from `reference`, ggexon
  repeatedly picks the remaining species that shares the most ODGI nodes
  with the most recently placed species.

- chr:

  Optional reference chromosome / seqname when subsetting Syn-backed
  links.

- subset:

  Optional numeric length-2 reference window. When supplied together
  with `reference` and `chr`, only links overlapping the derived
  comparative region are drawn. When omitted, `geom_nuclink()` uses the
  current annotation windows when available, otherwise the full
  alignment.

- filter_by_len:

  Optional ODGI node-length filter such as `"> 10"`, `"= 3"`, or
  `"<= 2"`. Applied only when link rows are being derived from an ODGI
  multiple alignment.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics rather than combining
  with them.

## Value

A ggplot2 layer using `GeomNucLink`.

## Details

For ordinary data, the layer expects columns or mappings for:

- `tspecies`, `tchr`, `tstart`, `tend`

- `qspecies`, `qchr`, `qstart`, `qend`

- `strand`

For Syn-backed plots, `alignment` can point to either a stored
`SynPairAlignment` or an ODGI-backed `SynMultiAlignment`. When an ODGI
multiple alignment is selected, ggexon derives adjacent pairwise link
tables for the plotted species order and dispatches them to the
corresponding middle link panels. In practice this layer is intended to
be used together with
[`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md),
which creates the annotation panels and middle link panels that
`geom_nuclink()` needs.
