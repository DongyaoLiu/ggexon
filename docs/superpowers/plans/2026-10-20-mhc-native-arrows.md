# Restore native MHC arrows beneath enlarged heads

User approved one small ortholog-colored MHC arrow at each true curated span on all 13 haplotypes, preserving enlarged heads, shared head rows and common bp scale.

- Archive the current figure and all draw-time audit tables.
- Add a separate geom_gene layer from all 285 curated MHC spans; use true native starts/ends and strands, reflecting siamang once. No exon/transcript structure or new projection is inferred.
- Native arrow fills encode ortholog identity, including pseudogenes with a defined ortholog color. The 28 records with 18 pseudogene-only keys lacking a source color retain source pseudogene grey; assert no functional record needs this fallback. Enlarged grey heads and asterisks retain pseudogene status. Grey context and the existing legend remain unchanged.
- Keep native arrows in the existing gene row [-0.5,0], above VR annotations and beneath anchor stems. Draw over context for visibility; enlarge neither their genomic widths nor their endpoints.
- Record actual built geometry/fills and independently check all 285 arrows against sources. Require identical pre/post head layouts, stems, context, axes, panel heights and figure dimensions. Inspect vector-PDF detail crops and obtain independent review.
