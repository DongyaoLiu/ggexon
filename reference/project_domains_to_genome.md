# Project protein-domain coordinates onto genomic coordinates

Uses transcript CDS structure from a `SynIndividual` to convert
protein-domain intervals into one or more genomic intervals.

## Usage

``` r
project_domains_to_genome(
  x,
  annotation = NULL,
  ids = NULL,
  domains = NULL,
  model = "all",
  motif = NULL,
  genes = NULL,
  transcripts = NULL,
  chr = NULL,
  start = NULL,
  end = NULL
)
```

## Arguments

- x:

  A `SynIndividual` object.

- annotation:

  Optional name of an attached `SynProteinDomainAnnotation` layer.
  Defaults to the first available protein-domain annotation.

- ids:

  Optional explicit identifier vector matched against the domain
  annotation key column.

- domains:

  Optional domain names/accessions to filter.

- model:

  InterProScan analysis model(s) to keep. Accepts a single string, a
  character vector, or `"all"`.

- motif:

  Optional motif name(s) used to filter the InterProScan table.

- genes:

  Optional gene identifiers used to limit the projected proteins.

- transcripts:

  Optional transcript identifiers used to limit the projected proteins.

- chr:

  Optional chromosome name used to define the genomic window.

- start, end:

  Optional genomic window bounds.

## Value

A data frame with projected genomic motif segments.
