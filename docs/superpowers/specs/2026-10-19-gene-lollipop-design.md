# Gene lollipop geom

Approved through the grill-me discussion and the user's final “yes, do it”.

Create `geom_gene_lollipop()` as a reusable annotation geom. Each gene keeps a true genomic midpoint anchor at the native gene row. A stem connects to an enlarged, fixed physical-size directional gene arrow, with the gene symbol inside. The common head width fits the longest label without reducing text size; its width is schematic and never a genomic length measurement. Unknown strands use a non-directional box. Labels, fill and pseudogene asterisks are supplied metadata, not biological inference.

Heads may shift horizontally within a configurable physical limit and occupy a compact number of rows above their anchors. Head centres preserve genomic order, and full head-plus-text boxes cannot overlap. Packing uses final viewport millimetres, so symbol size does not shrink over megabase x ranges. When a requested width/height cannot fit all genes, fail with an actionable size/spacing message instead of dropping genes or silently overlapping. Coordinate reversal reverses strand arrows exactly once. Source x, y, bounds, gene identifiers and track provenance remain unchanged.

The API supports ordinary data with x/y/strand/label mappings and Syn-backed gene annotations. Use the gene interval midpoint for Syn input, avoiding codon assumptions for pseudogenes. `headroom` reserves a numeric y-domain above the anchor; physical dimensions govern the actual arrows. An optional final-layout callback provides per-panel viewport, anchor, displaced centre, row and arrow bounds for audit. No backbone or transcript exon display is introduced.

Apply the geom to the 285 curated MHC genes, including pseudogenes, using source ortholog colors/status. Grey context genes stay at native scale; exclude overlapping MHC representative models from the grey layer. Remove the separate MHC points and standalone displaced labels. Keep native genomic axes, left labels, blank panel outlines and the shared VR lane beneath the native gene row.

Tests cover crowded anchors, order preservation, finite displacement, exact bounds, clear overcapacity failures, physical size at different genomic spans and panel widths, label fit, unknown/reversed strands, Syn selection and facet panel routing. Inspect the actual MHC PDF and PNG and audit all 285 heads and their native anchors.
