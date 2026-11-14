# Prepare shared genomic geometry on a Syn object

Computes a single exon-union intron map and bounded local head
positions. Stems, mutation heads, labels and exon layers can then use
the same layout. Geometry remains in original genomic units and the
stored map is applied once by
`scale_x_ggexon_genomic(genomic_layout = name)`.

## Usage

``` r
prepare_genomic_layout(
  x,
  annotation,
  name = "genomic",
  transcripts = NULL,
  chr = NULL,
  intron_factor = 10,
  head_spread = 0,
  mutation_y_by = NULL,
  mutation_y_trans = "identity",
  mutation_y_range = c(0.85, 1.45),
  track_ymin = 2,
  track_ymax = 2.8,
  stem_points = 30,
  curve_k = 1,
  base_edges = TRUE
)
```

## Arguments

- x:

  A SynIndividual with a stored mutation projection.

- annotation:

  Stored mutation-projection name.

- name:

  Name for the prepared layout in the object.

- transcripts:

  One reference transcript; inferred only when unambiguous.

- chr:

  Optional reference chromosome.

- intron_factor:

  Compression factor for the shared intron map.

- head_spread:

  Maximum sideways offset, in genomic units. Zero disables spreading.

- mutation_y_by, mutation_y_trans, mutation_y_range:

  Height controls passed to protein_lollipop_data().

- track_ymin, track_ymax:

  Bottom and top of the exon model.

- stem_points, curve_k:

  Curve controls shared by all stem consumers.

- base_edges:

  Use half-base exon edges to preserve inclusive base counts.

## Value

An updated SynIndividual. Original annotations are retained.
