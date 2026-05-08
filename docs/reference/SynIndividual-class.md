# SynIndividual class

`SynIndividual` stores the per-individual data needed to build synteny
plots. Only `id` is required at the class level; genome and annotation
file paths can be attached later. Parsed annotations, nucleotide/protein
sequences, and plotting caches can also be attached through accessor
methods.

## Slots

- `id`:

  Scalar identifier for the species, genome, or plotting track.

- `genome_file`:

  Path to the genome FASTA file, or `NA_character_` when the genome was
  waived during construction.

- `annotation_file`:

  Path or paths to the corresponding GFF or GTF file(s).

- `annotation_format`:

  One of `"gff"`, `"gtf"`, or `"auto"`, or a vector matching
  `annotation_file`.

- `annotation`:

  Parsed annotation container used for plotting as a
  [`GenomicRanges::GRanges`](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  object.

- `nucleotide_seq`:

  Nucleotide sequences extracted from the genome as a
  [`Biostrings::DNAStringSet`](https://rdrr.io/pkg/Biostrings/man/XStringSet-class.html).

- `protein_seq`:

  Protein sequences translated from CDS annotations as a
  [`Biostrings::AAStringSet`](https://rdrr.io/pkg/Biostrings/man/XStringSet-class.html).

- `seqinfo`:

  Sequence-level metadata such as chromosome names and lengths stored as
  a
  [`GenomeInfoDb::Seqinfo`](https://rdrr.io/pkg/GenomeInfoDb/man/Seqinfo-class.html)
  object.

- `feature_index`:

  Fast lookup structure for genes, transcripts, or exons.

- `annotations`:

  Named list of `SynAnnotation` objects attached to this genome.

- `active_annotation`:

  Name of the default feature annotation layer to use.

- `metadata`:

  User or import metadata describing the individual.

- `plot_cache`:

  Derived plotting tables cached for reuse.

- `projected_domains`:

  Named list of projected protein-domain tables stored for inspection.

## Prototype defaults

- `annotation_format = "auto"`

- `annotation = NULL`

- `nucleotide_seq = NULL`

- `protein_seq = NULL`

- `seqinfo = NULL`

- `feature_index = NULL`

- `annotations = list()`

- `active_annotation = "default"`

- `metadata = list()`

- `plot_cache = list()`

- `projected_domains = list()`

The raw class prototype leaves `annotations` empty. The high-level
[`SynIndividual()`](https://dongyaoliu.github.io/ggexon/reference/SynIndividual.md)
constructor usually populates that slot with a `"default"`
[`SynFeatureAnnotation()`](https://dongyaoliu.github.io/ggexon/reference/SynFeatureAnnotation.md)
immediately.

## Validity rules

- `id` and `active_annotation` must be scalar non-empty character
  values.

- `genome_file` must be a length-one character vector or
  `NA_character_`.

- When present, `annotation_file` must be a non-empty character vector
  with no empty entries.

- `annotation_format` must be one of `"auto"`, `"gff"`, or `"gtf"`. When
  `annotation_file` is present, it must be length one or the same length
  as `annotation_file`.

- When `annotations` is non-empty, every entry must inherit from
  `SynAnnotation`. When feature annotations are present,
  `active_annotation` must name one of those feature layers.

- `projected_domains` must be a list of data-frame-like objects.
