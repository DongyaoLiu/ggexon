# Subset a comparative window from a `SynSpecies` object

Uses a reference species and genomic window to find overlapping pairwise
alignments, derives the linked window on the partner genome from the
dominant PAF cluster, and trims both annotation layers plus the retained
link rows to the matched comparative region.

## Usage

``` r
subset_synspecies_window(
  x,
  reference_species,
  chr,
  start,
  end,
  alignment = NULL,
  selected_species = NULL,
  filter_by_len = NULL,
  max_target_gap = NULL
)

# S4 method for class 'SynSpecies'
subset_synspecies_window(
  x,
  reference_species,
  chr,
  start,
  end,
  alignment = NULL,
  selected_species = NULL,
  filter_by_len = NULL,
  max_target_gap = NULL
)
```

## Arguments

- x:

  A `SynSpecies` object.

- reference_species:

  Individual name used as the starting coordinate system.

- chr:

  Chromosome/seqname on the reference species.

- start:

  Start coordinate on the reference species.

- end:

  End coordinate on the reference species.

- alignment:

  Optional pairwise alignment name. Required when multiple pairwise
  alignments exist and you want to choose a specific pair.

- selected_species:

  Optional character vector giving the plotted species order to retain
  when `alignment` points to an ODGI multiple alignment. When
  `reference_species` is supplied for an ODGI multiple alignment, ggexon
  reorders this set greedily from the reference by choosing the next
  species with the largest shared-node count against the most recently
  chosen species. Adjacent species in the resulting order are linked
  pairwise.

- filter_by_len:

  Optional ODGI node-length filter such as `"> 10"` or `"<= 3"`. Applied
  only when `alignment` resolves to an ODGI multiple alignment.

- max_target_gap:

  Optional maximum gap used when chaining nearby PAF hits on the partner
  genome. Defaults to `max(50000, 2 * window_width)`.

## Value

A list with `windows`, `annotations`, and `links`.
