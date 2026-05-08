# SynProteinDomainAnnotation class

`SynProteinDomainAnnotation` stores protein-domain calls keyed by a
protein-, transcript-, or gene-level identifier.

## Details

In addition to the slots listed below, the class inherits `name`,
`source_file`, `annotation_scope`, `lazy`, `loaded`, `metadata`, and
`plot_cache` from `SynAnnotation`.

## Slots

- `data_format`:

  Domain file format label. Currently `"domain"`.

- `domain_data`:

  Optional cached parsed domain table.

- `keytype`:

  Identifier type used to join domain rows to package objects.

- `source_db`:

  Optional source database label such as `"Pfam"` or `"InterPro"`.

## Prototype defaults

- `data_format = "domain"`

- `annotation_scope = "protein"`

- `domain_data = NULL`

- `keytype = "protein_id"`

- `source_db = NA_character_`

## Validity rules

No additional custom validity checks are defined beyond the inherited
slot classes and parent-class rules.
