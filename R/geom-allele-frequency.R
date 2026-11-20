#' Draw population allele frequencies inside a linkage panel
#'
#' Each SNP appears a fraction `af` of the way from its reference allele to
#' its alternate allele. Both endpoints follow their annotation panels' x
#' scales, including panel reversal and genomic intron compression. Frequencies
#' of zero and one fall on the reference and alternate edges respectively.
#'
#' @param mapping Aesthetic mappings from [ggplot2::aes()]. Colour, shape,
#'   size, fill, alpha and stroke follow [ggplot2::geom_point()].
#' @param data A [SynSpecies()] with a stored preparation, or a table with
#'   the columns of [query_allele_frequency()]. For a table, `track` identifies
#'   the link panel, `tspecies` and `qspecies` its annotation panels, and
#'   `tstart = tend` and `qstart = qend` the two native genomic positions.
#'   Supply `tchr`, `qchr`, `strand`, numeric `af` and logical
#'   `reference_is_target`. Canonically named columns are mapped automatically.
#' @param preparation Stored preparation name. Optional with a single preparation.
#' @param ... Additional arguments passed to the point layer.
#' @inheritParams geom_nuclink
#' @return A ggplot2 point layer for use with [facet_genomics()].
#' @seealso [prepare_allele_frequency()], [SynSNPAnnotation()]
#' @include geom-nuclink.R
#' @export
geom_allele_frequency <- function(mapping = NULL, data = NULL, preparation = NULL,
                                  ..., na.rm = FALSE, show.legend = NA,
                                  inherit.aes = FALSE) {
  layer(mapping = mapping, data = data, geom = GeomAlleleFrequency,
        stat = "identity", position = "identity", layer_class = LayerAlleleFrequency,
        inherit.aes = inherit.aes, show.legend = show.legend,
        params = list(preparation = preparation, na.rm = na.rm, ...))
}

# The points already use the panel's normalized coordinate system.
.af_display_coord <- ggproto(NULL, ggplot2::CoordCartesian,
  transform = function(data, panel_params) data
)

GeomAlleleFrequency <- ggproto("GeomAlleleFrequency", ggplot2::GeomPoint,
  ggexon_panel_role = "link",
  required_aes = c(GeomNucLink$required_aes, "af", "reference_is_target"),
  optional_aes = c(GeomNucLink$optional_aes, "track"),
  extra_params = c("na.rm", "preparation"),
  syn_default_aes = c(GeomNucLink$syn_default_aes, "af", "reference_is_target"),

  syn_data = function(x, layer) {
    entry <- .allele_frequency_entry(x, syn_layer_params(layer)$preparation)
    rows <- .af_index_style_rows(entry$variants, layer$aes_params)
    if (!nrow(rows)) return(rows)
    windows <- layer$syn_plot_context$windows
    for (side in c("t", "q")) {
      species <- rows[[paste0(side, "species")]][1L]
      window <- windows[[species]]
      pos <- rows[[paste0(side, "start")]]
      chr <- rows[[paste0(side, "chr")]]
      keep <- rep(TRUE, nrow(rows))
      if (!is.null(window$chr)) keep <- keep & chr == window$chr
      if (!is.null(window$start)) keep <- keep & pos >= window$start
      if (!is.null(window$end)) keep <- keep & pos <= window$end
      rows <- rows[keep, , drop = FALSE]
      if (!nrow(rows)) break
    }
    rows
  },

  setup_data = function(data, params) {
    if (!is.numeric(data$af) || any(!is.finite(data$af) | data$af < 0 | data$af > 1))
      cli::cli_abort("Allele frequency must be numeric and within [0, 1].")
    if (!is.logical(data$reference_is_target) || anyNA(data$reference_is_target))
      cli::cli_abort("{.field reference_is_target} must be TRUE or FALSE for each SNP.")
    if (anyNA(data$tstart) || anyNA(data$tend) || anyNA(data$qstart) || anyNA(data$qend) ||
        any(data$tstart != data$tend | data$qstart != data$qend))
      cli::cli_abort("Each allele-frequency endpoint must identify one genomic position.")
    metadata <- c("PANEL", "t_panel", "q_panel", "target_anchor_y", "query_anchor_y")
    if (!all(metadata %in% names(data)) || anyNA(data[metadata]))
      cli::cli_abort("{.fn geom_allele_frequency} requires a link panel and both source annotation panels from {.fn facet_genomics}.")
    vertices <- GeomNucLink$setup_data(data, params)
    vertices$af <- data$af[vertices$id]
    vertices$reference_is_target <- data$reference_is_target[vertices$id]
    if (".ggexon_af_row" %in% names(data)) {
      vertices$.ggexon_af_row <- data$.ggexon_af_row[vertices$id]
    }
    vertices
  },

  draw_layer = draw_link_layer,
  draw_panel = function(data, panel_params, coord, panel, na.rm = FALSE) {
    # Borrow the two source x scales and the link panel's vertical scale.
    endpoints <- .transform_link_x_by_source_panel(data, panel_params, CoordCartesianGenomic)
    endpoints <- CoordCartesianGenomic$transform_y(endpoints, panel_params[[panel]])
    target <- endpoints[endpoints$x_variable == "tstart", , drop = FALSE]
    query <- endpoints[endpoints$x_variable == "qstart", , drop = FALSE]
    query <- query[match(target$group, query$group), , drop = FALSE]
    weight <- ifelse(target$reference_is_target, target$af, 1 - target$af)
    target$x <- target$x + weight * (query$x - target$x)
    target$y <- target$y + weight * (query$y - target$y)
    ggplot2::GeomPoint$draw_panel(target, panel_params[[panel]], .af_display_coord, na.rm)
  }
)

# Some facet windows specify limits without selecting a chromosome. Their
# final panel ranges are available only after facet layout resolution.
.af_filter_panel_windows <- function(data, layout) {
  if (!nrow(data) || !all(c("xlim_min", "xlim_max") %in% names(layout))) return(data)
  keep <- rep(TRUE, nrow(data))
  for (side in c("t", "q")) {
    source <- data[[paste0(side, "_panel")]]
    if (is.null(source)) next
    panel <- match(source, layout$PANEL)
    lower <- layout$xlim_min[panel]
    upper <- layout$xlim_max[panel]
    pos <- data[[paste0(side, "start")]]
    keep <- keep & (is.na(lower) | pos >= lower) & (is.na(upper) | pos <= upper)
    if ("xlim_chr" %in% names(layout)) {
      chr <- layout$xlim_chr[panel]
      keep <- keep & (is.na(chr) | data[[paste0(side, "chr")]] == chr)
    }
  }
  data[which(keep), , drop = FALSE]
}
