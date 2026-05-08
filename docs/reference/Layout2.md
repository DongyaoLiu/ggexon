# ggexon layout runtime with Syn-aware panel setup

`Layout2` is ggexon's custom layout ggproto. It inherits from ggplot2's
`Layout` and overrides the parts of the build pipeline where Syn-aware
panel structure and link metadata need to be introduced.

## Usage

``` r
Layout2
```

## Format

An object of class `Layout2` (inherits from `Layout`, `ggproto`, `gg`)
of length 3.

## Details

Compared with the upstream layout, `Layout2` adds two main
responsibilities:

1.  `setup()` carries Syn plot data and optional stored `SynLayout`
    metadata into facet setup, lets
    [`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md)
    generate or reuse genomic panel layouts, and joins panel-level
    metadata such as `t_panel` and `q_panel` back onto layer data.

2.  `map_position()` maps x/y aesthetics panel-by-panel using the
    trained scales from the resolved layout while preserving the extra
    panel metadata introduced during setup.

This class is what makes stored `SynLayout` objects, link panels, and
cross-panel coordinate borrowing work inside the normal ggplot2 build
flow.

## Build flow

The high-level flow is:

- [`ggexon_build()`](https://dongyaoliu.github.io/ggexon/reference/ggexon_build.md)
  creates `Layout2` with
  [`create_layout2()`](https://dongyaoliu.github.io/ggexon/reference/create_layout2.md).

- `Layout2$setup()` asks the active facet for the panel table.

- [`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md)
  may return a stored `SynLayout`, derive a new chain layout, or fall
  back to a standard faceting layout.

- if link panels are present, link-direction metadata is added and
  source panel ids are propagated to layer data.

- `Layout2$map_position()` maps each layer's x/y aesthetics against the
  `SCALE_X` / `SCALE_Y` assignments in the final panel table.

## See also

[SynLayout](https://dongyaoliu.github.io/ggexon/reference/SynLayout.md)
