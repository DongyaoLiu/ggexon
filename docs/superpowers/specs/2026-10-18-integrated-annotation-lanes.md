# Integrated annotation lanes

The user requested an isolated worktree and all non-nucleotide annotations inside each genome's gene-annotation panel, below the gene models. Nucleotide links remain separate intermediate panels.

Use existing ggexon annotation-panel routing with ordinary ggplot interval layers. The example renderer will reserve negative-y lanes for feature intervals, ATAC and methylation, while gene models remain above zero. Each signal lane explicitly labels its raw unit and range; matching assays share ranges across genomes. Original intervals and scores remain unchanged. Missing assays remain omitted. No core package behavior changes are necessary.

Verification requires exactly one annotation panel per genome, only intermediate nucleotide-link panels, no coverage-role panels, every signal interval routed to its owning genome below its gene models, unchanged raw score vectors, unchanged genomic windows, and visual inspection of all three exported figures. Existing source/checksum/alignment audits must still pass. The primary checkout and its previous figures are preserved.
