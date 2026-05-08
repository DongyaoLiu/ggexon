# Constructor for SynSpecies

Constructor for SynSpecies

## Usage

``` r
SynSpecies(
  name = NULL,
  annotation_folder = NULL,
  annotation_format = c("auto", "gff", "gtf"),
  recursive = FALSE,
  tree = NULL,
  tree_plot = NULL,
  metadata = list()
)
```

## Arguments

- name:

  Species collection label. If omitted and `annotation_folder` is
  supplied, the folder basename is used.

- annotation_folder:

  Optional directory containing `.gff`, `.gff3`, or `.gtf` files to
  import immediately as annotation-only `SynIndividual` objects.

- annotation_format:

  One of `"auto"`, `"gff"`, or `"gtf"`. Used only when
  `annotation_folder` is supplied.

- recursive:

  Logical; should annotation discovery recurse into subfolders? Used
  only when `annotation_folder` is supplied.

- tree:

  Optional tree object, such as an
  [`ape::phylo`](https://rdrr.io/pkg/ape/man/read.tree.html), to reuse
  for tree-aligned genomic plots.

- tree_plot:

  Optional rectangular `ggtree` plot to reuse for tree-aligned genomic
  plots.

- metadata:

  Optional metadata list.

## Value

A `SynSpecies` object. When `annotation_folder` is provided, the object
is initialized with one annotation-only `SynIndividual` per supported
annotation file found in that folder.
