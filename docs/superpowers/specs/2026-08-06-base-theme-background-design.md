# Shared ggexon Base Theme Design

## Goal

Provide one reusable ggexon theme object that removes decorative background rectangles and borders, then make every ggexon-derived theme include that shared behavior.

The change must preserve facet-label text and optional x-grid lines. In particular, the light vertical genomic coordinate grid remains controlled by each derived theme's existing `show_x_grid` argument.

## Public API

Add an exported function:

```r
theme_ggexon_base(
  base_size = 8,
  base_family = ""
)
```

The function returns a ggplot2 theme object. It is deliberately suitable both for direct use and for composition inside more specialized ggexon themes.

Internally, a private background-only modifier holds the shared blank-element contract. `theme_ggexon_base()` combines `theme_minimal()` with that modifier. Theme helpers that must remain incomplete modifiers, especially `theme_ggexon_side_strips()`, reuse the private modifier directly so they do not reset a complete theme that was added earlier.

## Base Theme Contract

`theme_ggexon_base()` starts with `ggplot2::theme_minimal()` for typography and coordinate-grid defaults, then removes these decorative elements:

- `plot.background`
- `panel.background`
- `panel.border`
- `strip.background`
- `legend.background`
- `legend.key`

All are set to `ggplot2::element_blank()`.

The base theme does not blank strip text, axis text, axis ticks, or grid lines. This keeps facet labels readable and lets derived themes control axes and grids for their particular layout.

## Derived Theme Hierarchy

`theme_ggexon_track()` will start from `theme_ggexon_base()` rather than calling `theme_minimal()` directly. Its current axis, grid, legend, spacing, and strip-text behavior stays intact.

`theme_ggexon_genomictree()` continues to build on `theme_ggexon_track()`, so it receives the base background contract transitively.

`theme_ggexon_side_strips()` remains a composable, incomplete theme modifier and includes the shared background-only modifier rather than another complete base theme. This prevents it from resetting track-specific axes or grids when added after `theme_ggexon_track()`. Its default `background` changes from `"grey96"` to `NA`, so side-strip labels no longer draw gray bars. Supplying an explicit non-missing color remains an intentional override and produces a strip rectangle.

`theme_ggexon_pairwise()` continues to combine the track and left-side-strip behavior. Because both components include the base contract, its annotation labels remain on the left without gray strip bars, and its x-grid remains visible when `show_x_grid = TRUE`.

## Compatibility

- Existing theme function names and main arguments remain available.
- `theme_ggexon_track()` keeps x-grid lines by default.
- Strip label text remains visible.
- The only intentional default visual change is that `theme_ggexon_side_strips()` and therefore `theme_ggexon_pairwise()` no longer use a gray strip background.
- Users can restore a colored side-strip background explicitly through `theme_ggexon_side_strips(background = "grey96")`.

## Documentation

Document `theme_ggexon_base()` as the shared foundation and update the theme-family documentation to describe the inheritance hierarchy. Add it to the pkgdown theme reference section and export it through roxygen.

The pairwise example should demonstrate left-side panel labels without background bars while retaining the vertical x-grid.

## Testing and Verification

Add testthat coverage that proves:

1. `theme_ggexon_base()` returns a theme and blanks every background element in its contract.
2. Base strip text and grid elements are not blanked.
3. `theme_ggexon_track()` inherits the blank backgrounds and keeps its default major x-grid line.
4. `theme_ggexon_genomictree()` inherits the same blank backgrounds.
5. `theme_ggexon_side_strips()` defaults to a blank strip background but keeps horizontal strip text; an explicit background color still overrides the blank default.
6. `theme_ggexon_pairwise()` inherits blank backgrounds, left-side strip text, and a visible default major x-grid.

Use a RED/GREEN test cycle, regenerate roxygen documentation, render the pairwise example for visual inspection, run the focused theme tests, and then run the full package test suite. Do not stage or commit changes.
