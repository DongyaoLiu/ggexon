# Cross-Panel Annotations Design Notes

## Goal

Build a small drawing system on top of `grid` for cross-panel annotations in faceted `ggplot2`/`ggexon` plots.

The focus is narrow on purpose:

- link objects that live in different panels
- compute connector coordinates after plot build
- inject the final grobs before render
- stay compatible with the existing `ggplot2` mental model

This should not become a lightweight replacement for `ggplot2`. It should be a focused post-build annotation layer that solves one difficult problem well.

## Primary Use Case

The motivating use case is to connect two objects in different panels when they share the same identity.

Examples:

- the same feature id appears in two facet panels
- two genomic regions in different panels should be visually linked
- a matched pair should be connected by a line, curve, or band

In all of these cases, normal `geom_*()` layers are awkward because each panel is drawn independently.

## Design Position

The right hook point is:

1. after `ggplot_build()`
2. after scales are trained and panel assignments are known
3. before the final `gtable` is rendered to the device

That timing gives access to:

- panel layout
- trained x/y scales
- facet assignments
- transformed layer data
- final panel placement in the `gtable`

This makes the system data-aware while still drawing with `grid`.

## Product Direction

The package should expose a post-build cross-panel annotation extension, not a separate plotting grammar.

That gives us:

- less scope
- easier adoption
- better compatibility with existing plots
- a clearer implementation path in `ggexon`

In practical terms, the user should still write a normal plot and then add a cross-panel annotation spec.

## Minimal V1 Scope

The first version should stay very small.

Supported:

- `facet_wrap()` and `facet_grid()`
- Cartesian coordinates
- explicit anchor coordinates from user data
- straight connector lines
- optional curved connectors if the implementation is still simple
- optional rectangles or bands spanning panel ranges

Not in V1:

- `coord_polar()`
- `coord_sf()`
- automatic extraction of anchor locations from arbitrary geoms
- deep support for every position adjustment edge case
- non-standard panel systems that do not behave like ordinary facet panels

## API Shape

The API should feel like a `ggplot2` add-on, even if the implementation is post-build.

Possible user-facing forms:

```r
p + annotate_cross_panel(
  data = links,
  from = aes(panel = panel1, x = x1, y = y1),
  to = aes(panel = panel2, x = x2, y = y2),
  colour = "red"
)
```

or a more task-specific helper:

```r
p + link_panels(
  data = links,
  id = "id",
  panel = "facet_value",
  x = "x",
  y = "y"
)
```

The helper form is likely the better user API for this package, especially if the main workflow is "same id across panels".

## Core Abstractions

Three concepts are enough for a useful first design.

### 1. Anchor

An anchor is a point attached to an object in a panel.

It needs:

- a panel identifier
- an x value
- a y value
- optionally an id
- optionally metadata for styling or grouping

Later we could allow named anchor positions such as:

- `"center"`
- `"top"`
- `"bottom"`
- `"left"`
- `"right"`

That would help if an object is represented by an interval or rectangle rather than a point.

### 2. Connector

A connector links one anchor to another.

It needs:

- a `from` anchor
- a `to` anchor
- graphical parameters such as colour, linewidth, alpha, linetype, arrow, curvature

### 3. Renderer

The renderer resolves anchors against the built plot and produces `grid` grobs spanning the combined panel region.

This separation is important:

- anchor resolution is about data and panels
- rendering is about grobs and layout

## Coordinate Strategy

The safest V1 design is to require explicit coordinates from the user.

Why this is the right first step:

- robust and easy to explain
- independent of layer internals
- avoids special handling for dodging, jittering, summaries, and custom geoms
- easy to test

Later we can add helpers that derive anchors from built layer data, but that should be a second phase.

## Coordinate Transformation Model

This is the technical heart of the system.

For each anchor:

1. identify the target panel
2. retrieve that panel's trained x and y scales from the built object
3. transform the anchor's data coordinates into panel-relative coordinates
4. resolve the panel's position inside the final `gtable`
5. convert the panel-relative coordinates into plot-level `grid` units

Once both endpoints are resolved, a connector grob can be drawn across the combined panel area.

This gives a true data-aware endpoint in each panel while still using `grid` for the final drawing.

## Fixed vs Free Scales

The system should support both `fixed` and free scales, but we need to be explicit about the interpretation.

With fixed scales:

- a connector aligned at the same vertical or horizontal position across panels has a stable data meaning
- visual comparison is straightforward

With `free_x` or `free_y`:

- anchor resolution still works panel by panel
- the connector can still be drawn correctly
- but a straight line spanning panels is no longer a shared global x or y value

In other words, with free scales the connector is still data-aware at its endpoints, but the segment between those endpoints is a visual bridge rather than a globally meaningful data trace.

## Relation to Existing `ggexon` Architecture

This package already has the right build/render split:

- [R/plot-build.R](/Users/liudongyao/Downloads/repository/ggexon/R/plot-build.R:1) defines `ggexon_build()`
- [R/plot-build.R](/Users/liudongyao/Downloads/repository/ggexon/R/plot-build.R:151) defines `ggexon_gtable()`
- [R/layout.R](/Users/liudongyao/Downloads/repository/ggexon/R/layout.R:1) defines `Layout2`

That makes this feature a natural fit for the current package.

There are also useful signs in the existing codebase:

- `GeomNucLink` already thinks about source and target panels
- `Layout2` already augments data with panel metadata
- the package already works directly with `grid` and `gtable`

So the cross-panel annotation system can be designed as a generalization of that direction rather than as an unrelated subsystem.

## Proposed Internal Pipeline

The implementation should be modular and testable.

Suggested steps:

1. collect panel metadata from the built plot and rendered `gtable`
2. resolve user-specified anchors against built panel data
3. transform each anchor into plot-level coordinates
4. create connector grobs from those coordinates
5. inject the grobs into the `gtable` with explicit clipping and z-order rules

Conceptually:

```r
plot -> build -> resolve anchors -> transform coordinates -> make grobs -> inject grobs -> draw
```

Suggested internal helpers:

```r
collect_panel_info(build, table)
resolve_anchors(build, spec, data)
transform_anchor_to_plot_coords(anchor, panel_info)
make_connector_grob(from, to, style)
inject_connector_grobs(table, grobs, z = Inf, clip = "off")
```

## Integration Strategy

The cleanest implementation path is to store a cross-panel annotation spec on the plot object and resolve it during gtable creation.

That likely means:

1. a user-facing constructor such as `link_panels(...)`
2. a place on the plot object to store cross-panel specs
3. a hook inside `ggexon_gtable()` that:
   - sees whether such specs exist
   - collects panel layout information
   - generates the final grobs
   - injects them into the table before returning

This keeps the final annotation logic close to rendering, which is where panel placement becomes concrete.

## Rendering Concerns

There are a few rendering choices we should expose or at least settle internally.

### Clipping

Cross-panel annotations should usually be drawn with clipping turned off at the injected grob level, otherwise the connector will be cut at panel boundaries.

### Z-order

We should decide whether connectors appear:

- behind panel data
- above panel data
- above background but below strips and axes

My instinct is to default to above panel data, but this may depend on the visual language of `ggexon`.

### Panel Span

For the injected grob, the drawing viewport should span all involved panels rather than be attached to only one panel.

## Why Not Just Use `annotation_custom()`?

Because `annotation_custom()` still operates within panel-oriented drawing logic.

The proposed system needs to understand:

- which panel each anchor belongs to
- how that panel transforms its data
- where that panel sits in the full plot layout
- how to draw a single grob across multiple panel regions

That is fundamentally a post-build, post-layout operation.

## V1 Data Model Example

Example connector data:

```r
links <- data.frame(
  id = c("a", "b"),
  panel1 = c("left", "left"),
  x1 = c(2, 4),
  y1 = c(5, 6),
  panel2 = c("right", "right"),
  x2 = c(3, 5),
  y2 = c(8, 2)
)
```

This is enough to support a large portion of the intended workflows.

For the "same id in two panels" case, another useful format would be one row per anchor:

```r
anchors <- data.frame(
  id = c("a", "a", "b", "b"),
  panel = c("left", "right", "left", "right"),
  x = c(2, 3, 4, 5),
  y = c(5, 8, 6, 2)
)
```

Then the helper can pair rows by `id`.

## Suggested First Deliverable

The first implementation target should be:

- explicit anchor coordinates
- one connector type: straight line
- one helper that links shared ids across panels
- integration into the existing build/render pipeline

If that works cleanly, we can extend to:

- curves
- rectangles or ribbons spanning panel intervals
- anchor extraction from built geoms
- richer attachment semantics

## Open Design Questions

Questions worth settling before implementation:

1. where should the annotation spec live on the plot object?
2. should V1 be generic for any faceted plot, or initially scoped to `ggexon` objects?
3. should connector styling be constant per layer in V1, or fully data-driven?
4. what is the default z-order policy?
5. do we want to support both pairwise data and shared-id data in the first API, or just one of them?

## Current Recommendation

My current recommendation is:

- explicit coordinates first
- straight lines first
- `ggplot`-style user API
- post-build grob injection internally
- implement it as a focused cross-panel annotation extension, not a new plotting system

That keeps the system small, composable, and well aligned with the current package architecture.
