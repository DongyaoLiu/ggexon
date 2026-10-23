# Native GenArk Augustus audit of 14 unresolved context transfers

All coordinates are native, zero-based, half-open (BED0); all exon arrays are sorted on the native chromosome. These are QC evidence, not accepted gene models. No curated MHC model, context model, shared row or plot scale was changed.

The source models are the original same-species CatRef representatives in `results/mhc_continuous/figures/before-alt-context/representative-models.tsv`. Exons were reconstructed from the representative BED12 fields. The existing full-region same-species PAF CIGAR was used to identify collinear aligned source exon segments and their target intervals. SSY display coordinates were explicitly reversed back into native coordinates. Minor inverted alignments were excluded from the syntenic mapping. Exact (`=`) CIGAR base counts and aligned base counts are divided by the complete source exonic length. These measures support sequence homology at the source-syntenic locus; they do not prove transcript expression, precise splice boundaries, gene identity against all paralogs, or pseudogene status.

The native Augustus bigBed URLs and assembly identifiers come from the existing GenArk probe metadata. Direct queries of all five cached native whole-assembly bigBeds reconfirmed the regional BED contents; all native chromosome lengths match the corresponding NCBI assembly reports. The native accession/browser chromosome mapping is recorded in `native_bigbed_query_provenance.json`. In PABh2 and PPYh2, all available regional Augustus models start above 33 Mb. Their lack of predictions at the requested loci is therefore a property of the available prediction track, not proof of gene absence.

`native_recovery_summary.tsv` contains all 14 requested entries, including entries without a candidate. `native_candidate_evidence.json` contains the complete source exons, all target aligned segments, candidate exon arrays, source URLs, strand checks, and overlap of candidate exons with all named source-model exons. `native_candidate_evidence.tsv` is the flattened candidate table. `*.expanded.augustus.bed` contains directly re-queried regional BED records. The scripts regenerate the analysis without writing to annotation/model/figure tables.

No native Augustus prediction can safely be accepted as a complete requested gene model:

- SSYh2 MUC21 overlaps g12924.t1 (+), opposite the source gene (-). The prediction also contains sequence aligned to MUC22.
- GGOh2 MUC21 overlaps g4607.t1 (-), opposite the source gene (+).
- GGOh2 MUC22 overlaps g4608.t1 (-), opposite the source gene (+), and includes exons aligned to CDSN and PSORS1C1.
- GGOh2 MUCL3 has useful partial corroboration from g4606.t1 (+): the complete native exons 41047819-41047934 and 41058098-41062330 exactly coincide with aligned MUCL3 exons. However, the full 58-exon model extends 40988503-41088760 and also joins sequence aligned to DDR1, VARS2, GTF2H4 and SFTA2. It cannot be accepted as a MUCL3 transcript or used to assign its extra exons.
- PPAh1 C6orf15 has no overlapping native prediction exons despite a broad neighboring g4389.t3 model crossing the interval.
- PPAh1 OR2J2 lies at a locus bounded by collinear source-alignment flanks, but its source exons are unaligned in the whole-region PAF. The broad g4360.t1 spans multiple neighboring olfactory receptor loci; a span crossing OR2J2's search locus does not identify it as OR2J2.
- PABh2 LY6G6E, OR12D3, OR5V1, MUC21, MUCL3 and OR12D2, and PPYh2 MUC21 and MUCL3, have no native Augustus model at the syntenic locus. OR12D3 and OR12D2 share the same closest aligned target flanks because their source segment lacks collinear target alignment in the existing PAF. The two-base search bracket is a coordinate anchor, not a gene-sized interval or evidence of deletion.

Sequence-aware transfer or expression evidence must resolve these cases. No conclusion about functional loss or pseudogenization is warranted from this audit.
