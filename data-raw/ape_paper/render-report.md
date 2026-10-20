# Integrated annotation render report

Run: 2026-10-18 22:43:42

mhc OK: 3 genomes; 116 candidate links; 13 displayed links; 2 in-panel signal lanes

All feature intervals and signal lanes share their owning genome annotation panel, below its gene models. Only nucleotide links have separate panels.
Signal lane headings give original units and raw-value ranges. Matching assays share the same range across genomes; this affine display placement does not change source scores.
Signal-lane QC asserts every interval/score survives unchanged and is routed to its owning annotation panel below zero. Missing chimp_h2 methylation is omitted.
Native genomic axis labels retain the documented gtable workaround. scale_x_continuous(oob=scales::oob_keep) preserves boundary intervals for clipping.
All changes are example-local; package source files and the primary checkout remain unchanged.
