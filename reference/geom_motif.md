# Plot protein-domain motifs

When the plot data is a `SynIndividual` or `SynSpecies`, this layer
resolves an attached `SynProteinDomainAnnotation` and draws domain
intervals as motif blocks along protein-coordinate tracks.

## Usage

``` r
geom_motif(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  exon_height = NULL,
  y_scale = NULL,
  x_translation = NULL,
  species = NULL,
  chr = NULL,
  subset = NULL,
  annotation = NULL,
  ids = NULL,
  domains = NULL,
  model = "all",
  motif = NULL,
  y_offset = NULL,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard ggplot2 layer arguments.

- exon_height:

  Height of the motif blocks.

- y_scale:

  Vertical spacing between protein tracks.

- x_translation:

  Optional x-axis offset.

- species:

  Optional species/individual selector when plotting from a `SynSpecies`
  object.

- chr:

  Optional chromosome name used to define the genomic projection window.

- subset:

  Optional genomic window used to limit the projected motifs.

- annotation:

  Optional annotation-layer name. Defaults to the first attached
  `SynProteinDomainAnnotation`.

- ids:

  Optional identifiers to match against the protein-domain annotation
  key column.

- domains:

  Optional domain names/accessions to filter.

- model:

  InterProScan analysis model(s) to display. Accepts a single string, a
  character vector, or `"all"`. When multiple models are supplied, their
  order is used from top to bottom.

- motif:

  Optional motif name(s) used to filter the InterProScan table.

- y_offset:

  Vertical offset applied to the motif band so protein domains can be
  separated from exon rectangles on the same genomic track.

## Value

A ggplot layer.
