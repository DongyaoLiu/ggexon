# Subsetting annotation and comparative windows

ggexon supports three closely related kinds of subsetting. They differ
in when the subsetting happens and what kind of object they return.

## Details

**1. Plot-time annotation subsetting**

Syn-aware annotation geoms subset lazily at build time by combining
`chr =` with `subset = c(start, end)`. This is the most common plotting
workflow when you want to display only one genomic window from a larger
GFF/GTF file.


    ggexon(sp) +
      geom_exon(
        species = "XZ1516",
        chr = "RagTag_V",
        subset = c(21558028, 21620381)
      )

The whole annotation file stays attached to the `SynIndividual`; only
the requested window is resolved for the layer.

**2. Direct feature queries**

Use
[`query_features()`](https://dongyaoliu.github.io/ggexon/reference/query_features.md)
when you want the subsetted annotation ranges as a `GRanges` object
instead of a plot layer. This is useful for inspection, downstream
computation, or building a custom plotting table.


    ind <- individuals(sp)[["XZ1516"]]
    gr <- query_features(
      ind,
      chr = "RagTag_V",
      start = 21558028,
      end = 21620381,
      feature_type = "exon"
    )

**3. Object-level window subsetting**

Use
[`subset_feature_annotation()`](https://dongyaoliu.github.io/ggexon/reference/subset_feature_annotation.md)
or
[`subset_individual()`](https://dongyaoliu.github.io/ggexon/reference/subset_individual.md)
when you want a reusable windowed object rather than a one-off `GRanges`
query. These helpers return trimmed snapshots that are ready to pass
back into
[`ggexon()`](https://dongyaoliu.github.io/ggexon/reference/ggexon.md) or
to inspect directly.


    ind <- individuals(sp)[["XZ1516"]]
    ann <- get_annotation(ind, "default")

    small_ann <- subset_feature_annotation(
      ann,
      coords = "RagTag_V:21558028-21620381"
    )

    ind <- individuals(sp)[["XZ1516"]]
    small_ind <- subset_individual(
      ind,
      coords = "RagTag_V:21558028-21620381"
    )

    small_sp <- subset_species(
      sp,
      coords = c(
        "XZ1516#RagTag_V:21558028-21620381",
        "N2#V:20454111-20491853"
      )
    )

[`subset_species()`](https://dongyaoliu.github.io/ggexon/reference/subset_species.md)
is useful when you already know the species-specific windows you want to
keep and would like a reusable trimmed `SynSpecies` object rather than a
comparative window query result.

**4. Comparative window subsetting**

Use
[`subset_synspecies_window()`](https://dongyaoliu.github.io/ggexon/reference/subset_synspecies_window.md)
when you want one reference window and the corresponding linked window
on the partner genome. This helper returns both annotation subsets and
the retained link rows.


    out <- subset_synspecies_window(
      sp,
      reference_species = "XZ1516",
      chr = "RagTag_V",
      start = 21574445,
      end = 21584356,
      alignment = "XZ1516_vs_N2"
    )

The return value is a list with:

- `windows`: the resolved genomic window for each species

- `annotations`: subsetted `GRanges` objects per species

- `links`: the retained pairwise alignment rows

**5. Alignment-only subsetting**

If you only want the pairwise alignment rows and not the annotation
ranges, use
[`pairwise_alignment_data()`](https://dongyaoliu.github.io/ggexon/reference/pairwise_alignment_data.md)
with `subset =`. Use
[`subset_pairwise_alignment()`](https://dongyaoliu.github.io/ggexon/reference/subset_pairwise_alignment.md)
when you want an updated alignment object or updated `SynSpecies`.


    paf <- pairwise_alignment_data(
      sp,
      alignment = "XZ1516_vs_N2",
      subset = c(
        XZ1516 = "RagTag_V:21574445-21584356",
        N2 = "V:20456000-20465040"
      )
    )

    paf_query_only <- pairwise_alignment_data(
      sp,
      alignment = "XZ1516_vs_N2",
      subset = c(XZ1516 = "RagTag_V")
    )

## Which function should I use?

- Use `chr` + `subset` in syn-aware geoms when you are plotting one
  window.

- Use
  [`query_features()`](https://dongyaoliu.github.io/ggexon/reference/query_features.md)
  when you want a subsetted annotation `GRanges`.

- Use
  [`subset_feature_annotation()`](https://dongyaoliu.github.io/ggexon/reference/subset_feature_annotation.md)
  or
  [`subset_individual()`](https://dongyaoliu.github.io/ggexon/reference/subset_individual.md)
  when you want a reusable windowed object.

- Use
  [`subset_species()`](https://dongyaoliu.github.io/ggexon/reference/subset_species.md)
  when you want to trim one or more individuals inside a `SynSpecies`
  with explicit species-tagged coordinate strings.

- Use
  [`subset_synspecies_window()`](https://dongyaoliu.github.io/ggexon/reference/subset_synspecies_window.md)
  when you want linked windows across species together with the retained
  annotation and link data.

- Use
  [`pairwise_alignment_data()`](https://dongyaoliu.github.io/ggexon/reference/pairwise_alignment_data.md)
  when you only need the alignment rows.

- Use
  [`subset_pairwise_alignment()`](https://dongyaoliu.github.io/ggexon/reference/subset_pairwise_alignment.md)
  when you want to keep the subsetted pairwise alignment inside a
  Syn-aware object.

## See also

[`query_features()`](https://dongyaoliu.github.io/ggexon/reference/query_features.md),
[`subset_synspecies_window()`](https://dongyaoliu.github.io/ggexon/reference/subset_synspecies_window.md),
[`subset_feature_annotation()`](https://dongyaoliu.github.io/ggexon/reference/subset_feature_annotation.md),
[`subset_individual()`](https://dongyaoliu.github.io/ggexon/reference/subset_individual.md),
[`subset_species()`](https://dongyaoliu.github.io/ggexon/reference/subset_species.md),
[`subset_pairwise_alignment()`](https://dongyaoliu.github.io/ggexon/reference/subset_pairwise_alignment.md),
[`add_individuals_from_folder()`](https://dongyaoliu.github.io/ggexon/reference/add_individuals_from_folder.md)
