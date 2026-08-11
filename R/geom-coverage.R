#' Coverage geom ggproto
#'
#' `GeomCoverage` draws interval-native coverage rectangles. For Syn object
#' input, signal intervals are queried from an attached
#' `SynBigWigAnnotation`; plain data frames can supply the required aesthetics
#' directly.
#'
#' @format NULL
#' @usage NULL
GeomCoverage <- ggplot2::ggproto(
  "GeomCoverage",
  ggplot2::GeomRect,
  required_aes = c("xmin", "xmax", "coverage", "track"),
  extra_params = c("na.rm", "annotation", "species"),
  default_aes = ggplot2::aes(
    fill = "grey45",
    colour = NA,
    linewidth = 0,
    linetype = 1,
    alpha = 0.8,
    interval_start = NA_real_,
    interval_end = NA_real_,
    genomic_xmin = NA_real_,
    genomic_xmax = NA_real_
  ),
  setup_data = function(data, params) {
    coverage <- suppressWarnings(as.numeric(data$coverage))
    invalid <- !is.finite(coverage) | coverage < 0
    if (any(invalid)) {
      cli::cli_abort(
        "{.field coverage} must contain only finite, non-negative scores; invalid value at row {which(invalid)[[1L]]}."
      )
    }

    raw_xmin <- suppressWarnings(as.numeric(data$xmin))
    raw_xmax <- suppressWarnings(as.numeric(data$xmax))
    interval_start <- suppressWarnings(as.numeric(data$interval_start))
    interval_end <- suppressWarnings(as.numeric(data$interval_end))
    use_interval <- is.finite(interval_start) & is.finite(interval_end)
    raw_xmin[use_interval] <- interval_start[use_interval]
    raw_xmax[use_interval] <- interval_end[use_interval]

    data$interval_start <- raw_xmin
    data$interval_end <- raw_xmax
    data$genomic_xmin <- raw_xmin
    data$genomic_xmax <- raw_xmax
    data$xmin <- raw_xmin - 0.5
    data$xmax <- raw_xmax + 0.5
    data$coverage <- coverage
    data$ymin <- 0
    data$ymax <- coverage
    data$.ggexon_panel_role <- "coverage"
    data
  },
  syn_data = function(x, layer) {
    params <- syn_layer_params(layer)
    syn_to_coverage_df(
      x,
      species = params$species,
      annotation = params$annotation,
      context = layer$syn_plot_context %||% NULL
    )
  },
  syn_default_aes = c(
    "xmin", "xmax", "coverage", "track", "group", "interval_start",
    "interval_end", "genomic_xmin", "genomic_xmax"
  ),
  ggexon_panel_role = "coverage",
  draw_key = ggplot2::draw_key_rect
)

#' Plot interval-native BigWig coverage
#'
#' `geom_coverage()` draws raw coverage values as rectangles spanning each
#' stored BigWig interval. With a `SynIndividual` or `SynSpecies` data source,
#' the layer resolves an attached [SynBigWigAnnotation-class] and queries the
#' effective panel window. A plain data frame can instead provide `xmin`,
#' `xmax`, `coverage`, and `track` aesthetics directly.
#'
#' Coverage scores are plotted unchanged. The layer does not normalize,
#' smooth, threshold, or expand signal records to one row per base.
#' In a Syn-aware [`facet_genomics()`] build, every requested BigWig track owns
#' a first-class coverage panel separate from gene annotation. Coverage starts
#' at zero and is never moved into a synthetic negative annotation band. Use
#' [`scale_panel_coverage()`] to share one raw-depth y scale or give each
#' coverage panel an independent scale.
#' Explicit coverage data on a `SynIndividual` or `SynSpecies` plot can also
#' form standalone first-class coverage panels without an annotation layer.
#' Ordinary non-Syn plots retain the legacy composite-layer behavior.
#'
#' Continuous coverage panels use ordinary genomic x coordinates. They cannot
#' currently be combined with [`scale_x_ggexon_genomic()`] exon/intron
#' compression or [`strip_scale_x()`]; those mixed builds fail before
#' transforming the signal.
#' Inclusive genomic interval endpoints are retained in `interval_start`,
#' `interval_end`, `genomic_xmin`, and `genomic_xmax` in the built layer data;
#' rectangle edges are shifted by half a base so adjacent intervals meet
#' without overlap or gaps.
#'
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard
#'   ggplot2 layer arguments.
#' @param annotation Optional name of an attached `SynBigWigAnnotation`.
#'   When omitted, each individual must have exactly one attached BigWig
#'   annotation.
#' @param species Optional individual selector when plotting a `SynSpecies`.
#' @param bigwig,ref_chr,subset,annotation_type,y_threshold,x_threshold
#'   Deprecated file-driven coverage arguments. Attach a
#'   `SynBigWigAnnotation` and use effective panel windows instead.
#'
#' @return A ggplot2 layer.
#' @seealso [`SynBigWigAnnotation()`], [`query_signal()`],
#'   [`scale_panel_coverage()`], [`center_panel_annotation()`]
#' @export
geom_coverage <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          annotation = NULL,
                          species = NULL,
                          bigwig = NULL,
                          ref_chr = NULL,
                          subset = NULL,
                          annotation_type = NULL,
                          y_threshold = NULL,
                          x_threshold = NULL,
                          inherit.aes = TRUE) {
  if (!missing(bigwig)) {
    lifecycle::deprecate_warn(
      "0.1.0",
      "geom_coverage(bigwig)",
      "geom_coverage(annotation)"
    )
  }
  if (!missing(ref_chr)) {
    lifecycle::deprecate_warn(
      "0.1.0",
      "geom_coverage(ref_chr)",
      "effective_panel_windows()"
    )
  }
  if (!missing(subset)) {
    lifecycle::deprecate_warn(
      "0.1.0",
      "geom_coverage(subset)",
      "effective_panel_windows()"
    )
  }
  if (!missing(annotation_type)) {
    lifecycle::deprecate_warn("0.1.0", "geom_coverage(annotation_type)")
  }
  if (!missing(y_threshold)) {
    lifecycle::deprecate_warn("0.1.0", "geom_coverage(y_threshold)")
  }
  if (!missing(x_threshold)) {
    lifecycle::deprecate_warn("0.1.0", "geom_coverage(x_threshold)")
  }

  params <- Filter(Negate(is.null), c(list(
    ...,
    na.rm = na.rm,
    annotation = annotation,
    species = species
  )))
  ggplot2::layer(
    data = data,
    mapping = mapping,
    geom = GeomCoverage,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    layer_class = LayerSyn,
    params = params
  )
}
