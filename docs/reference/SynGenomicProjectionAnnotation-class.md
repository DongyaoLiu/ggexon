# Stored genomic projection of a protein annotation

Created by
[`project_mutations_to_genome()`](https://dongyaoliu.github.io/ggexon/reference/project_mutations_to_genome.md)
or
[`project_domains_to_genome()`](https://dongyaoliu.github.io/ggexon/reference/project_domains_to_genome.md)
with `store = TRUE`. Original protein annotations remain attached.
Projected fragments and mutation anchors retain their source row and
transcript identity.

## Slots

- `projection_data`:

  Genomic fragments and source metadata.

- `anchors`:

  One exonic anchor per source mutation and transcript.

- `source_annotation`:

  Name of the original protein annotation.

- `feature_kind`:

  Either `"mutation"` or `"domain"`.

- `recipe`:

  Projection selectors and reference annotation identity.

- `signature`:

  Fingerprint used to reject stale projections.
