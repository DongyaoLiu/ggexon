# Dense HLA-A substitution lollipop example

The user approved separate heads for alternatives at one residue, then requested a denser, more polished summary and approved counting every eligible named allele separately.

## Data and coordinates

Use all 5,998 eligible named HLA-A alleles from IPD-IMGT/HLA 3.66.0, commit `d972ae287948dc2e5468a40ff3b3d27227541189`. Eligibility: 365-residue precursor, twenty standard amino acids only, name ending in a numeric field. Verify every candidate against the official protein alignment; all candidates share the reference's gap pattern. Preserve original FASTA records unchanged in a deterministically compressed archive and retain attribution and checksums.

Use A*02:01:01:01 as reference. Subtract the 24-residue signal peptide for mature positions 1–341. Structural region boundaries come from the bundled UniProt P04439 snapshot. No genomic projection is implied.

Named alleles count separately even when their mature proteins are identical. There are 3,188 distinct mature protein sequences. Counts reflect database representation, not population frequencies.

## Figure

Draw all 1,212 substitutions at 334 residues on one 240 by 135 mm overview. One head represents one (position, reference, alternative) combination. Heights are log10(count + 1), with original count tick labels. Six chemical groups colour alternatives. Label structural regions directly on a muted backbone, with a clear title and summary counts.

Fan heads symmetrically within 1.6 residue units of each true residue, restarting the fan at each position; do not accumulate global displacement. Keep true stem anchors and preserve exact count heights. Nearby heads can overlap at this density; do not filter or subsample substitutions. Add a labelled residue-156 close-up with five alternatives and corresponding exact counts.

## Delivery and validation

Bundle compressed sequences, allele manifest, annotation snapshot, source checksums, loader, and plotting helpers. Provide an offline R Markdown tutorial, built pkgdown article, PNG and vector PDF exports, and compact state/substitution TSVs with up to three example carrier names per state.

Verify every substitution against original sequences, per-position totals of 5,998, all 1,212 drawn heads, transformed count heights, bounded offsets, unique local head coordinates, true stem starts and matching ends. Verify source integrity, deterministic regeneration, article image paths, and visual layout. Keep the package API unchanged.

## Approved gene-model extension and site publication

The user requested public pkgdown visibility and selected three-fold intron compression. Use the exact A*02:01:01:01 release-3.66.0 genomic snapshot (HLA00005). Its eight coding exons and seven introns span ATG to stop. Add 24 to mature positions before `project_mutations_to_genome()`, retain split-codon fragments, and anchor on the longest exonic fragment. Draw exon base edges so widths equal actual bp counts. Apply `scale_x_ggexon_genomic(intron_factor=3, guide=guide_x_ggexon_piecewise())` once, disabling geom_exon2's independent compression. Include the genomic view in the tutorial, homepage, PNG/PDF exports, and published docs.

Verify the spliced reference translation, independently enumerate every genomic codon, test all anchors, and compare native/compressed exon widths and intron gaps. Publish only intended HLA-A source files and generated public pages while retaining unrelated local work and newer remote changes.
