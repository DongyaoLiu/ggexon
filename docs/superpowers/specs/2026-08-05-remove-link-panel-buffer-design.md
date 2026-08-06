# Remove Link-Panel Buffer Rows

## Goal

When a faceted genomic plot contains annotation panels separated by a linkage
panel, remove the vertical `panel.spacing.y` buffers immediately above and below
the linkage panel. Keep spacing that separates annotation panels or belongs to a
mixed annotation/linkage row.

## Design

Extend the existing `apply_link_panel_layout()` gtable post-processing pass. It
already classifies annotation and linkage rows and removes linkage axes or
strips when requested. After resolving a linkage-only row, the same pass will
identify the unused gtable spacer row between that linkage panel and each
neighboring facet panel and set that spacer's height to zero.

The change is automatic whenever linkage panels are present. It will not add a
new public argument. A row shared with an annotation panel will not be
collapsed, because gtable row heights are shared across columns and collapsing
it would also alter annotation layout.

## Compatibility and Edge Cases

- Existing `link_panel_height`, `link_axis`, `link_strip`, and
  `annotation_axis` behavior remains unchanged.
- The operation is idempotent when a spacer already has zero height.
- Topmost or bottommost linkage panels collapse only the spacer that exists.
- Annotation-only plots and annotation-to-annotation spacing remain unchanged.
- Mixed rows retain their spacing to avoid affecting annotation panels in other
  columns.

## Tests and Visual Check

Add a regression test with two annotation panels and one linkage panel. Give
`panel.spacing.y` a visible nonzero value, render the gtable, and assert that
both linkage-adjacent spacer rows are zero while the annotation panel heights
and linkage panel height are preserved.

Render the same example to a PNG so the compact two-annotation/one-linkage
layout can be inspected directly.

## Scope

Only linkage-owned vertical gtable spacing is changed. No unrelated layout
refactoring, API changes, commit, or generated package-site rebuild is part of
this modification.
