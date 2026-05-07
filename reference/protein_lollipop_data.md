# Prepare lollipop annotation data on a linear feature track

`protein_lollipop_data()` prepares the small data frames needed to draw
a mutation lollipop plot. Coordinates are treated as positions on one
linear track, so the same structure can be used for amino-acid
positions, CDS positions, or nucleotide positions after upstream
coordinate conversion.

## Usage

``` r
protein_lollipop_data(
  mutations,
  domains = NULL,
  protein_length = NULL,
  position = "position",
  label = NULL,
  ref = NULL,
  alt = NULL,
  score = NULL,
  domain_start = "start",
  domain_end = "end",
  domain = NULL,
  domain_ymin = NULL,
  domain_ymax = NULL,
  spread_threshold = 7,
  protein_start = 0,
  track_ymin = 0.4,
  track_ymax = 0.6,
  domain_default_ymin = 0.35,
  domain_default_ymax = 0.65,
  mutation_y = 1,
  mutation_y_by = NULL,
  mutation_y_strategy = c("scaled", "bins"),
  mutation_y_range = c(0.85, 1.45),
  mutation_y_trans = "identity",
  mutation_y_breaks = NULL,
  mutation_y_values = NULL,
  stem_points = 30,
  curve_k = 1
)
```

## Arguments

- mutations:

  A data frame with one row per mutation.

- domains:

  Optional interval table,
  [`S4Vectors::DataFrame`](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html),
  or `SynProteinDomainAnnotation` object. Intervals must use the same
  coordinate system as `mutations`.

- protein_length:

  Optional track length. When `NULL`, the maximum mutation
  position/domain end is used.

- position:

  Column in `mutations` containing mutation positions.

- label:

  Optional label column in `mutations`. When omitted, a `mutation`
  column is used if present; otherwise labels are built from `ref`,
  `position`, and `alt` when both amino-acid columns are supplied.

- ref, alt:

  Optional reference and alternate residue/base columns used to generate
  labels.

- score:

  Optional column in `mutations` used for lollipop point colour, for
  example a BLOSUM score.

- domain_start, domain_end:

  Domain interval start/end columns.

- domain:

  Optional domain label column. When omitted, a suitable domain column
  is inferred from common InterProScan/domain-table names.

- domain_ymin, domain_ymax:

  Optional domain y-bound columns. If omitted, existing `ymin`/`ymax`
  columns are used when present.

- spread_threshold:

  Minimum x-distance between adjacent lollipop heads.

- protein_start:

  Start coordinate for the backbone rectangle.

- track_ymin, track_ymax:

  Backbone y-range.

- domain_default_ymin, domain_default_ymax:

  Default y-range for domain intervals when the interval table does not
  already contain y columns.

- mutation_y:

  Fixed y coordinate for lollipop heads when `mutation_y_by` is `NULL`.

- mutation_y_by:

  Optional numeric column in `mutations` used to map lollipop heads to
  multiple y levels, for example `"sample_count"`.

- mutation_y_strategy:

  Height-mapping strategy used when `mutation_y_by` is supplied.
  `"scaled"` rescales values into `mutation_y_range`; `"bins"` assigns
  values to tiered y levels.

- mutation_y_range:

  Numeric length-2 y range used by non-fixed height strategies.

- mutation_y_trans:

  Transformation applied before height mapping. One of `"identity"`,
  `"log10"`, `"log2"`, `"log"`, `"sqrt"`, or a function. Log and
  square-root transformations require non-negative values and logs use
  `value + 1`, which is convenient for count data.

- mutation_y_breaks:

  Optional numeric upper bounds for `mutation_y_strategy = "bins"`,
  expressed on the original `mutation_y_by` scale.

- mutation_y_values:

  Optional numeric y levels for `mutation_y_strategy = "bins"`. When
  `mutation_y_breaks` is supplied, this must have length
  `length(mutation_y_breaks) + 1`.

- stem_points:

  Number of points used per stem curve.

- curve_k:

  Sigmoid steepness for curved stems.

## Value

A list with `mutations`, `domains`, `backbone`, and `stems` data frames.
