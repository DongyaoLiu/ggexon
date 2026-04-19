#' Subsetting annotation and comparative windows
#'
#' ggexon supports three closely related kinds of subsetting. They differ in
#' when the subsetting happens and what kind of object they return.
#'
#' \strong{1. Plot-time annotation subsetting}
#'
#' Syn-aware annotation geoms subset lazily at build time by combining
#' `chr =` with `subset = c(start, end)`. This is the most common plotting
#' workflow when you want to display only one genomic window from a larger
#' GFF/GTF file.
#'
#' \preformatted{
#' ggexon(sp) +
#'   geom_exon(
#'     species = "XZ1516",
#'     chr = "RagTag_V",
#'     subset = c(21558028, 21620381)
#'   )
#' }
#'
#' The whole annotation file stays attached to the `SynIndividual`; only the
#' requested window is resolved for the layer.
#'
#' \strong{2. Direct feature queries}
#'
#' Use [query_features()] when you want the subsetted annotation ranges as a
#' `GRanges` object instead of a plot layer. This is useful for inspection,
#' downstream computation, or building a custom plotting table.
#'
#' \preformatted{
#' ind <- individuals(sp)[["XZ1516"]]
#' gr <- query_features(
#'   ind,
#'   chr = "RagTag_V",
#'   start = 21558028,
#'   end = 21620381,
#'   feature_type = "exon"
#' )
#' }
#'
#' \strong{3. Object-level window subsetting}
#'
#' Use [subset_feature_annotation()] or [subset_individual()] when you want a
#' reusable windowed object rather than a one-off `GRanges` query. These
#' helpers return trimmed snapshots that are ready to pass back into `ggexon()`
#' or to inspect directly.
#'
#' \preformatted{
#' ind <- individuals(sp)[["XZ1516"]]
#' ann <- get_annotation(ind, "default")
#'
#' small_ann <- subset_feature_annotation(
#'   ann,
#'   coords = "RagTag_V:21558028-21620381"
#' )
#'
#' ind <- individuals(sp)[["XZ1516"]]
#' small_ind <- subset_individual(
#'   ind,
#'   coords = "RagTag_V:21558028-21620381"
#' )
#'
#' small_sp <- subset_species(
#'   sp,
#'   coords = c(
#'     "XZ1516#RagTag_V:21558028-21620381",
#'     "N2#V:20454111-20491853"
#'   )
#' )
#' }
#'
#' `subset_species()` is useful when you already know the species-specific
#' windows you want to keep and would like a reusable trimmed `SynSpecies`
#' object rather than a comparative window query result.
#'
#' \strong{4. Comparative window subsetting}
#'
#' Use [subset_synspecies_window()] when you want one reference window and the
#' corresponding linked window on the partner genome. This helper returns both
#' annotation subsets and the retained link rows.
#'
#' \preformatted{
#' out <- subset_synspecies_window(
#'   sp,
#'   reference_species = "XZ1516",
#'   chr = "RagTag_V",
#'   start = 21574445,
#'   end = 21584356,
#'   alignment = "XZ1516_vs_N2"
#' )
#' }
#'
#' The return value is a list with:
#'
#' - `windows`: the resolved genomic window for each species
#' - `annotations`: subsetted `GRanges` objects per species
#' - `links`: the retained pairwise alignment rows
#'
#' \strong{5. Alignment-only subsetting}
#'
#' If you only want the pairwise alignment rows and not the annotation ranges,
#' use `pairwise_alignment_data()` with `subset =` or the wrapper
#' [subset_pairwise_alignment()].
#'
#' \preformatted{
#' paf <- pairwise_alignment_data(
#'   sp,
#'   alignment = "XZ1516_vs_N2",
#'   subset = c(
#'     XZ1516 = "RagTag_V:21574445-21584356",
#'     N2 = "V:20456000-20465040"
#'   )
#' )
#' }
#'
#' @section Which function should I use?:
#' - Use `chr` + `subset` in syn-aware geoms when you are plotting one window.
#' - Use [query_features()] when you want a subsetted annotation `GRanges`.
#' - Use [subset_feature_annotation()] or [subset_individual()] when you want a
#'   reusable windowed object.
#' - Use [subset_species()] when you want to trim one or more individuals inside
#'   a `SynSpecies` with explicit species-tagged coordinate strings.
#' - Use [subset_synspecies_window()] when you want linked windows across
#'   species together with the retained annotation and link data.
#' - Use `pairwise_alignment_data()` or [subset_pairwise_alignment()] when you
#'   only need the alignment rows.
#'
#' @name subsetting_operations
#' @seealso [query_features()], [subset_synspecies_window()],
#'   [subset_feature_annotation()], [subset_individual()], [subset_species()],
#'   [subset_pairwise_alignment()], [add_individuals_from_folder()]
NULL
