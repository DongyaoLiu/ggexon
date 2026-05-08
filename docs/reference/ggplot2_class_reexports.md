# ggplot2 Class Objects Re-exported by ggexon

ggexon re-exports ggplot2's S7 and ggproto class objects so downstream
code can target the exact same class definitions without vendoring local
copies.

## Usage

``` r
class_gg()

class_ggproto

class_scale

class_guides

class_guide

class_coord

class_facet

class_layer

class_layout

class_scales_list

class_S3_gg

class_rel

class_zero_grob

class_waiver

class_derive

class_theme(elements = list(), ..., complete = FALSE, validate = TRUE)

class_labels(labels = list(), ...)

class_mapping(x = list(), ..., env = globalenv())

class_ggplot(
  data = waiver(),
  ...,
  layers = list(),
  scales = NULL,
  guides = NULL,
  mapping = aes(),
  theme = NULL,
  coordinates = coord_cartesian(default = TRUE),
  facet = facet_null(),
  layout = NULL,
  labels = labs(),
  meta = list(),
  plot_env = parent.frame()
)
```

## Format

An object of class `S7_S3_class` of length 3.

An object of class `S7_S3_class` of length 3.

An object of class `S7_S3_class` of length 3.

An object of class `S7_S3_class` of length 3.

An object of class `S7_S3_class` of length 3.

An object of class `S7_S3_class` of length 3.

An object of class `S7_S3_class` of length 3.

An object of class `S7_S3_class` of length 3.

An object of class `S7_S3_class` of length 3.

An object of class `S7_S3_class` of length 3.

An object of class `S7_S3_class` of length 3.

An object of class `S7_S3_class` of length 3.

An object of class `S7_S3_class` of length 3.

An object of class `S7_S3_class` of length 3.
