# Alternate-haplotype MHC context

Keep the isolated worktree and the existing 285 curated heads, shared rows, native axes and nucleotide ribbons unchanged.

1. Search exact alternate assemblies in official annotation releases and browser sources. Save negative coverage evidence and source metadata. Completed: consensus annotations are primary-only; broad GenArk RNA alignments are unsuitable as complete context.
2. Transfer existing same-species primary **context** representatives to the alternate MHC sequence with Liftoff. Keep explicit projected provenance, exon coverage and sequence identity; require >=95% for both and exclude partial mappings. This is an inferred context layer, not a new validated gene catalog. Do not infer additional MHC copies or functional status.
3. Convert every output span/exon from regional sequence coordinates to native BED0, including reverse-oriented siamang. Save all failures and source-target assignments. Preserve primary models byte-for-byte for their original fields.
4. Render both haplotypes with context. Mark projected panels clearly. Preserve the previous figure and independently verify all geometry and source-coordinate contracts.
5. Review the parser and integration, inspect vector-PDF crops, and report incomplete transfers as annotation uncertainty, never gene absence.
