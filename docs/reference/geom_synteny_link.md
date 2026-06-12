# Draw synteny interval links

`geom_synteny_link()` draws filled interval ribbons between genomic
tracks when the rows represent syntenic blocks, orthologous genes,
conserved exons, or other biologically matched intervals. It is a
semantic wrapper around
[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md)
for cases where the link is not necessarily a nucleotide alignment
fragment.

## Usage

``` r
geom_synteny_link(
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
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
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
  comparative region are drawn. When omitted,
  [`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md)
  uses the current annotation windows when available, otherwise the full
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

Use this layer when the input table already describes
interval-to-interval relationships. For ordinary data frames the
expected columns are the same as
[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md):

- `tspecies`, `tchr`, `tstart`, `tend`

- `qspecies`, `qchr`, `qstart`, `qend`

- `strand`

For `SynSpecies` input, `alignment`, `reference`, `chr`, and `subset`
are forwarded to
[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md)
so stored pairwise or multiple alignments can still be resolved lazily.

Internally, this wrapper creates the same `LayerSyn` and `GeomNucLink`
layer as
[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md).
During plot build,
[`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md)
creates annotation panels and middle link panels, then attaches
`target_anchor_y`, `query_anchor_y`, `t_panel`, and `q_panel` metadata.
`GeomNucLink` then melts `tstart`/`tend`/`qstart`/`qend` into four
polygon vertices, maps target and query x coordinates through their
source annotation panels, and draws the filled polygon in the link
panel.

## See also

[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md),
[`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md),
[`geom_genetag()`](https://dongyaoliu.github.io/ggexon/reference/geom_genetag.md)
