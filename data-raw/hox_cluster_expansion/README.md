# Building the HOX cluster expansion data

`build-hox-cluster-demo.R` creates the compact, plot-ready data under
`inst/extdata/hox_cluster_expansion/`.

Run it from anywhere in the repository:

```sh
/Library/Frameworks/R.framework/Resources/bin/Rscript \
  data-raw/hox_cluster_expansion/build-hox-cluster-demo.R
```

The script downloads the six pinned GTF files into the ignored `downloads/`
directory. To reuse another cache, set `HOX_GTF_CACHE` to a directory with
files named `human.gtf.gz`, `mouse.gtf.gz`, `chicken.gtf.gz`, `gar.gtf.gz`,
`zebrafish.gtf.gz`, and `amphioxus.gtf.gz`. Every input is checked against the
SHA-256 value recorded in the script before parsing.

## Pinned annotation sources

| Species | Database and release | Assembly | GTF |
|---|---|---|---|
| *Homo sapiens* | Ensembl 116 | GRCh38 | `Homo_sapiens.GRCh38.116.chr.gtf.gz` |
| *Mus musculus* | Ensembl 116 | GRCm39 | `Mus_musculus.GRCm39.116.chr.gtf.gz` |
| *Gallus gallus* | Ensembl 116 | bGalGal1.mat.broiler.GRCg7b | `Gallus_gallus.bGalGal1.mat.broiler.GRCg7b.116.chr.gtf.gz` |
| *Lepisosteus oculatus* | Ensembl 116 | LepOcu1 | `Lepisosteus_oculatus.LepOcu1.116.chr.gtf.gz` |
| *Danio rerio* | Ensembl 116 | GRCz11 | `Danio_rerio.GRCz11.116.chr.gtf.gz` |
| *Branchiostoma lanceolatum* | Ensembl Metazoa 63 | BraLan2 | `Branchiostoma_lanceolatum.BraLan2.63.gtf.gz` |

The exact HTTPS URLs, assembly accessions, retrieval date, and checksums are
written to `hox_species.tsv`.

## Selection and anchor rules

For every source protein-coding Hox gene, the builder selects the coding
transcript with the greatest genomic span among transcripts with a usable CDS
and a known `+` or `-` strand. Equal-span ties prefer an
`Ensembl_canonical` transcript and then the lexical transcript stable ID.
The three source-backed merged gar transcripts listed in
`curated_transcript_exclusions.tsv` are removed before that rule is applied;
affected selected rows use an explicit override value in
`transcript_selection_rule`.

The plot anchors mirror `geom_genebox()`:

- initiation: middle base of a complete explicit `start_codon`; otherwise the
  second CDS base in transcription order;
- middle: arithmetic genomic midpoint between the initiation and stop
  anchors, including introns but excluding UTRs;
- stop: middle base of a complete explicit `stop_codon`; otherwise the
  penultimate CDS base in transcription order.

The two fallbacks are plotting anchors. In particular, a terminal-CDS fallback
does not claim that the underlying three bases were sequence-verified as an
ATG or stop codon. `hox_genes.tsv` records the source and fallback flag for
both anchors; the fallback source token is `terminal_CDS_positional_proxy`.

## Curated complements and release audit

The builder validates the pinned annotations against a separate
`hox_expected_complement.tsv`, rather than treating a failed `gene_name`
regular-expression match as evidence of loss. The expected functional totals
are 39 for human, 39 for mouse, 39 for chicken, 43 for spotted gar, 49 for
zebrafish, and 15 for amphioxus. Every expected member must resolve to exactly
one plotted model or one explicit source-annotation gap.

The zebrafish complement has seven clusters: HoxAa, HoxAb, HoxBa, HoxBb,
HoxCa, HoxCb, and HoxDa. It contains HoxCb and no HoxDb cluster, so the matrix
marks **HoxDb** as `cluster_not_retained`; it does not mark HoxCb absent.
The unnamed Ensembl-116 Havana model ENSDARG00000100358 is rescued as HoxAa4
from its stable-ID history and collinear position. This brings the plotted
zebrafish complement to the published 49 protein-coding genes.

The Ensembl-116 chicken annotation supplies 35 safe coding models. Four
published members (HoxC4, HoxC5, HoxC6, and HoxD1) lack safe coding models in
GRCg7b and are retained as annotation gaps. The corresponding source inventory
is the published 39-gene avian complement:

- Liang et al. (2011), <https://pmc.ncbi.nlm.nih.gov/articles/PMC3038165/>

The spotted-gar inventory follows Supplementary Figure 12 of Braasch et al.
(2016): 43 protein-coding Hox genes in four clusters. Seven unnamed LepOcu1
models are rescued using Ensembl external xrefs plus physical collinearity;
12 expected members still lack a safe source model and remain annotation gaps.
The long model ENSLOCG00000011824 is deliberately not assigned: its 13 CDS
pieces and 673-aa prediction span several expected HoxA positions and make a
one-slot assignment unsafe.

Three other longest gar transcripts are explicitly excluded before applying
the transcript-span rule. ENSLOCT00000014539 and ENSLOCT00000016594 are
three-homeodomain merges spanning Hox4/Hox3/Hox2. Their shorter coherent
isoforms ENSLOCT00000014553 and ENSLOCT00000016597 are selected for HoxA3 and
HoxB3, while Hox2 and Hox4 remain annotation gaps. ENSLOCT00000007673 merges
HoxC9-like and HoxC6-like proteins and has no coherent alternative, so both
HoxC9 and HoxC6 remain gaps. These exceptions and their protein IDs are in
`curated_transcript_exclusions.tsv` and on the affected gene/gap rows.

The selected HoxA3 translation also preserves an external-xref disagreement:
Ensembl, ZFIN, RefSeq, collinearity, and its Hox3-like homeodomain support
HoxA3, while UniProt/TrEMBL W5N1L5.67 labels it Homeobox A4a. The retained
assignment and conflict are recorded in `hox_xref_conflicts.tsv` and in the
selected-gene provenance columns.

Gar HoxA14 is recorded as a lineage absence. HoxD14 is instead a recognizable
pseudogene, so it is documented but excluded from the 43 protein-coding
members and from plotted gene boxes:

- Braasch et al. (2016), <https://pmc.ncbi.nlm.nih.gov/articles/PMC4817229/>

These distinctions are machine-readable in `hox_slot_states.tsv`.

## Manual BraLan2 mapping and Hox13 gap

The BraLan2 GTF uses `BL` stable IDs without Hox gene symbols. The mapping is
therefore explicit and reviewable in `amphioxus_hox_mapping.tsv`.

The GTF contains a collinear run of 14 protein-coding models on the minus
strand of `Sc0000000`. From high to low genomic coordinate, the models map to
Hox1 through Hox12, followed by Hox14 and Hox15. Ensembl Metazoa release-63
BioMart assigns the Homeodomain entry IPR001356 to 12 of those 14 models;
BL02747 (mapped Hox7) and BL14546 (mapped Hox12) are retained by their
unambiguous positions in the same-strand collinear run.

There is no BraLan2 gene model between BL14546 (Hox12) and BL01409 (Hox14).
Hox13 is consequently represented as an `annotation_gap`, not as a biological
loss and not as a fabricated gene box. This interpretation is consistent with
the primary *B. lanceolatum* study reporting that Hox13 was not recovered by
RT-PCR or in the embryonic transcriptome, while the lancelet complement was
described as 15 Hox genes:

- Pascual-Anaya, D'Aniello, and Garcia-Fernandez (2008),
  <https://doi.org/10.1007/s00427-008-0246-8>
- Pascual-Anaya et al. (2012),
  <https://pmc.ncbi.nlm.nih.gov/articles/PMC3534614/>

The assembly and annotation origin are documented by
[Ensembl Metazoa](https://metazoa.ensembl.org/Branchiostoma_lanceolatum/Info/Index)
and [Amphiencode](https://amphiencode.github.io/Data/). Posterior amphioxus
numbers, especially Hox14 and Hox15, should not be read as strict one-to-one
orthology to vertebrate paralog groups.

## Generated files

- `hox_genes.tsv`: one row per plotted gene and its selected transcript;
- `hox_cds.tsv`: CDS pieces for selected transcripts in transcription order;
- `hox_clusters.tsv`: the complete 7-row by 4-column matrix with panel states;
- `hox_annotation_gaps.tsv`: source-backed slots without a usable gene model;
- `hox_expected_complement.tsv`: literature/curation-defined functional
  inventory used for completeness QA;
- `hox_slot_states.tsv`: every retained cluster's Hox15-Hox1 slots, explicitly
  distinguishing plotted models, annotation gaps, non-retention, gar HoxA14
  absence, and the unplotted gar HoxD14 pseudogene;
- `manual_hox_mapping.tsv`: the eight unnamed source models rescued by stable
  ID, xref evidence, and cluster collinearity;
- `curated_transcript_exclusions.tsv`: merged gar transcript models excluded
  before longest-safe-transcript selection;
- `hox_xref_conflicts.tsv`: the preserved HoxA3 UniProt label disagreement and
  evidence supporting the retained HoxA3 assignment;
- `hox_species.tsv`: pinned source and assembly provenance;
- `amphioxus_hox_mapping.tsv`: all Hox1-Hox15 slots and manual mapping status;
- `annotations/*.gff3`: compact selected gene, mRNA, CDS, and source codon
  records for Syn-backed examples. Every mRNA carries `slot`, Hox cluster,
  mapping method, and anchor provenance; the builder asserts transcript-slot
  round-trip after writing.
