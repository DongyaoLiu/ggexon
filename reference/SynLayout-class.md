# SynLayout class

`SynLayout` stores panel placement information used by
[`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md)
together with shared plotting defaults that can be resolved by syn-aware
geoms. The `panels` table describes the panel arrangement, while the
numeric slots store layout-scoped defaults such as shared exon height or
x-axis translation.

## Slots

- `panels`:

  Layout data frame. It must contain `PANEL`, `ROW`, `COL`, and `track`,
  and may also contain comparative plotting columns such as
  `panel_type`, `species`, `alignment_name`, `tspecies`, `qspecies`,
  `t_panel`, `q_panel`, and optional panel-specific x-window columns
  `xlim_chr`, `xlim_min`, and `xlim_max`.

- `layout_type`:

  Scalar layout strategy label such as `"custom"` or `"chain"`.

- `free`:

  List with logical `x` and `y` entries describing whether scales should
  vary across panels.

- `exon_height`:

  Shared default exon or gene block height for layout-aware annotation
  geoms.

- `y_scale`:

  Shared default y scaling for layout-aware annotation geoms.

- `x_translation`:

  Shared default x-axis offset for layout-aware annotation geoms.

- `metadata`:

  Optional layout metadata.

## Prototype defaults

- `panels = data.frame()`

- `layout_type = "custom"`

- `free = list(x = FALSE, y = FALSE)`

- `exon_height = NA_real_`

- `y_scale = NA_real_`

- `x_translation = NA_real_`

- `metadata = list()`

## Validity rules

- `panels` must contain at least the columns `PANEL`, `ROW`, `COL`, and
  `track`.

- when `panels` contains any of `xlim_chr`, `xlim_min`, or `xlim_max`,
  it must contain all three columns.

- annotation panels with panel-specific x limits must provide complete
  `xlim_min` and `xlim_max` values. `xlim_chr` may be missing for
  numeric display-only windows that should not drive annotation
  filtering.

- when multiple annotation panels define different x windows, `free$x`
  must be `TRUE`.

- `layout_type` must be one non-empty character value.

- `free` must be a list with scalar logical `x` and `y` entries.

- `exon_height`, `y_scale`, and `x_translation` must each be scalar
  numeric values.
