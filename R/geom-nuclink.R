#' Draw cross-panel nucleotide links
#'
#' `geom_nuclink()` draws filled polygons that connect one interval on a query
#' genome to one interval on a target genome. It can be used with an ordinary
#' data frame containing explicit query/target columns, or with a `SynSpecies`
#' object where link rows are resolved lazily from stored alignments.
#'
#' For ordinary data, the layer expects columns or mappings for:
#'
#' - `tspecies`, `tchr`, `tstart`, `tend`
#' - `qspecies`, `qchr`, `qstart`, `qend`
#' - `strand`
#'
#' For Syn-backed plots, `alignment` can point to either a stored
#' `SynPairAlignment` or an ODGI-backed `SynMultiAlignment`. When an ODGI
#' multiple alignment is selected, ggexon derives adjacent pairwise link tables
#' for the plotted species order and dispatches them to the corresponding
#' middle link panels. In practice this layer is intended to be used together
#' with [facet_genomics()], which creates the annotation panels and middle link
#' panels that `geom_nuclink()` needs.
#'
#' @param mapping Set of aesthetic mappings created by [`ggplot2::aes()`].
#'   `colour`, `fill`, and `alpha` can be mapped in the standard ggplot2 way.
#' @param data A data frame or a `SynSpecies` object.
#' @param stat,position Standard ggplot2 layer arguments.
#' @param ... Additional parameters passed to the layer, including fixed
#'   aesthetics such as `fill`, `colour`, and `alpha`.
#' @param na.rm If `FALSE`, the default, missing values are removed with a
#'   warning. If `TRUE`, missing values are removed silently.
#' @param show.legend Logical. Should this layer be included in the legend?
#' @param alignment Optional alignment name when `data` is a `SynSpecies`.
#'   This can refer to a stored `SynPairAlignment` or an ODGI
#'   `SynMultiAlignment`.
#' @param reference Optional reference species used when deriving a comparative
#'   window from `chr` + `subset`. For ODGI multiple alignments, this also seeds
#'   the greedy comparison-panel order: starting from `reference`, ggexon
#'   repeatedly picks the remaining species that shares the most ODGI nodes with
#'   the most recently placed species.
#' @param chr Optional reference chromosome / seqname when subsetting Syn-backed
#'   links.
#' @param subset Optional numeric length-2 reference window. When supplied
#'   together with `reference` and `chr`, only links overlapping the derived
#'   comparative region are drawn. When omitted, `geom_nuclink()` uses the
#'   current annotation windows when available, otherwise the full alignment.
#' @param filter_by_len Optional ODGI node-length filter such as `"> 10"`,
#'   `"= 3"`, or `"<= 2"`. Applied only when link rows are being derived from an
#'   ODGI multiple alignment.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics rather than
#'   combining with them.
#'
#' @return A ggplot2 layer using `GeomNucLink`.
#' @export
geom_nuclink <- function(mapping = NULL, data = NULL,
                                stat = "identity", position = "identity",
                                ...,
                                na.rm = FALSE, show.legend = NA,
                                alignment = NULL,
                                reference = NULL,
                                chr = NULL,
                                subset = NULL,
                                filter_by_len = NULL,
                                inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    geom = GeomNucLink,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    layer_class = LayerSyn,
    params = c(list(
      ...,
      na.rm = na.rm,
      alignment = alignment,
      reference = reference,
      chr = chr,
      subset = subset,
      filter_by_len = filter_by_len
    ))
  )
}

#' Draw synteny interval links
#'
#' `geom_synteny_link()` draws filled interval ribbons between genomic tracks
#' when the rows represent syntenic blocks, orthologous genes, conserved exons,
#' or other biologically matched intervals. It is a semantic wrapper around
#' [geom_nuclink()] for cases where the link is not necessarily a nucleotide
#' alignment fragment.
#'
#' @details
#' Use this layer when the input table already describes interval-to-interval
#' relationships. For ordinary data frames the expected columns are the same as
#' [geom_nuclink()]:
#'
#' - `tspecies`, `tchr`, `tstart`, `tend`
#' - `qspecies`, `qchr`, `qstart`, `qend`
#' - `strand`
#'
#' For `SynSpecies` input, `alignment`, `reference`, `chr`, and `subset` are
#' forwarded to [geom_nuclink()] so stored pairwise or multiple alignments can
#' still be resolved lazily.
#'
#' Internally, this wrapper creates the same `LayerSyn` and `GeomNucLink` layer
#' as [geom_nuclink()]. During plot build, [facet_genomics()] creates annotation
#' panels and middle link panels, then attaches `target_anchor_y`,
#' `query_anchor_y`, `t_panel`, and `q_panel` metadata. `GeomNucLink` then melts
#' `tstart`/`tend`/`qstart`/`qend` into four polygon vertices, maps target and
#' query x coordinates through their source annotation panels, and draws the
#' filled polygon in the link panel.
#'
#' @inheritParams geom_nuclink
#'
#' @return A ggplot2 layer using `GeomNucLink`.
#' @seealso [geom_nuclink()], [facet_genomics()], [geom_genetag()]
#' @export
geom_synteny_link <- function(mapping = NULL, data = NULL,
                              stat = "identity", position = "identity",
                              ...,
                              na.rm = FALSE, show.legend = NA,
                              alignment = NULL,
                              reference = NULL,
                              chr = NULL,
                              subset = NULL,
                              filter_by_len = NULL,
                              inherit.aes = TRUE) {
  geom_nuclink(
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    ...,
    na.rm = na.rm,
    show.legend = show.legend,
    alignment = alignment,
    reference = reference,
    chr = chr,
    subset = subset,
    filter_by_len = filter_by_len,
    inherit.aes = inherit.aes
  )
}

GeomNucLink <- ggproto("GeomPanel", Geom,
                             required_aes = c("tspecies", "tchr", "tstart", "tend", "strand",
                                              "qspecies", "qchr", "qstart", "qend"),
                             optional_aes = c("target_anchor_y", "query_anchor_y", "t_panel", "q_panel"),
                             non_missing_aes = c("linetype", "linewidth", "shape"),
                             extra_params = c("na.rm", "alignment", "reference", "chr", "subset", "filter_by_len"),
                             draw_key = draw_key_polygon,
                             default_aes = aes(linewidth = 0,
                                               linejoin = "mitre",
                                               colour = "black",
                                               size = 15,
                                               linetype = 1,
                                               shape = 19,
                                               alpha = 0.5,
                                               stroke = 1,
                                               fill = "grey50",
                                               target_anchor_y = NA_real_,
                                               query_anchor_y = NA_real_
                             ),
                             setup_data = function(data, params) {
                               required_layout_cols <- c(
                                 "target_anchor_y", "query_anchor_y", "t_panel", "q_panel"
                               )
                               missing_layout_cols <- setdiff(required_layout_cols, names(data))
                               if (length(missing_layout_cols) > 0L) {
                                 cli::cli_abort(c(
                                   "{.fn geom_nuclink} requires link-panel layout metadata before drawing.",
                                   "i" = "Add {.fn facet_genomics} so ggexon can create annotation and middle link panels.",
                                   "i" = "Missing columns: {.val {missing_layout_cols}}."
                                 ))
                               }

                               # extract the y layout information
                               link_y_out = data %>% select(PANEL, group, target_anchor_y, query_anchor_y) %>% unique() %>%
                                 melt(id = c("PANEL", "group"), variable.name = "y_variable", value.name = "y") %>%
                                 mutate(y = as.numeric(y))


                               # each row are a group will have a same id after melting
                               data$id <- seq_len(nrow(data))
                               id_cols <- intersect(
                                 c("id", "strand", "PANEL", "group", "t_panel", "q_panel",
                                   "tspecies", "qspecies", "track", "tchr", "qchr",
                                   "fill", "colour", "alpha", "linewidth", "linetype",
                                   "size", "shape", "stroke"),
                                 names(data)
                               )
                               x_cols <- intersect(c("tstart", "tend", "qstart", "qend"), names(data))
                               melt_data <- data %>%
                                 select(any_of(c(id_cols, x_cols))) %>%
                                 melt(id = id_cols, measure.vars = x_cols,
                                      variable.name = "x_variable", value.name = "x") %>%
                                 mutate(y_variable = if_else(stringr::str_detect(x_variable,"^t"), "target_anchor_y", "query_anchor_y")) %>%
                                 left_join(link_y_out, join_by(PANEL == PANEL, group == group, y_variable == y_variable)) %>%
                                 mutate(source_panel = if_else(stringr::str_detect(x_variable, "^t"), t_panel, q_panel)) %>%
                                 arrange(id, x_variable) %>% rowwise() %>%
                                 mutate(draw_order =
                                          case_when(strand == "+" && x_variable == "tstart" ~ 1,
                                                    strand == "+" && x_variable == "tend" ~ 2,
                                                    strand == "+" && x_variable == "qstart" ~ 4,
                                                    strand == "+" && x_variable == "qend" ~ 3,
                                                    strand == "-" && x_variable == "tstart" ~ 1,
                                                    strand == "-" && x_variable == "tend" ~ 2,
                                                    strand == "-" && x_variable == "qstart" ~ 3,
                                                    strand == "-" && x_variable == "qend" ~ 4)) %>%
                                 arrange(id, draw_order) %>% select(-group)
                               melt_data$group = melt_data$id
                               #print(melt_data)
                               melt_data
                             },

                         draw_layer = function(self, data, params, layout, coord) {
                         if (empty(data)) {
                           n <- if (is.factor(data$PANEL)) nlevels(data$PANEL) else 1L
                           return(rep(list(zeroGrob()), n))
                         }

                         # Trim off extra parameters
                         params <- params[intersect(names(params), self$parameters())]

                         if (nlevels(as.factor(data$PANEL)) > 1L) {
                           data_panels <- split(data, data$PANEL)
                         } else {
                           data_panels <- list(data)
                         }


                         lapply(data_panels, function(data) {
                           if (empty(data)) return(zeroGrob())
                           panel = data$PANEL[1]
                           inject(self$draw_panel(data, layout$panel_params, coord, panel, !!!params))
                         })
                       },
                       draw_panel = function(data, panel_params, coord, panel, rule = "evenodd",
                                             lineend = "butt", linejoin = "round", linemitre = 10){
                       data <- ggplot2:::fix_linewidth(data, snake_class(self))
                       n <- nrow(data)
                       if (n == 1) return(zeroGrob())

                       munched <- .transform_link_x_by_source_panel(data, panel_params, coord) %>%
                         arrange(PANEL, group, draw_order)
                       munched = coord$transform_y(munched, panel_params[[panel]])

                       if (!"fill" %in% names(munched)) {
                         munched$fill <- "grey50"
                       }
                       if (!"linetype" %in% names(munched)) {
                         munched$linetype <- 1
                       }
                       if (!"linewidth" %in% names(munched)) {
                         munched$linewidth <- 0
                       }
                       if (!"alpha" %in% names(munched)) {
                         munched$alpha <- 0.5
                       }
                       if (!"colour" %in% names(munched)) {
                         munched$colour <- "black"
                       }

                       first_idx <- !duplicated(munched$group)
                       first_rows <- munched[first_idx, ]

                       ggname(
                         "geom_link_polygon",
                         polygonGrob(
                           munched$x, munched$y, default.units = "native",
                           id = munched$group,
                           gp = gg_par(
                             col = first_rows$colour,
                             fill = fill_alpha(first_rows$fill, first_rows$alpha),
                             lwd = first_rows$linewidth,
                             lty = first_rows$linetype,
                             lineend = lineend,
                             linejoin = linejoin,
                             linemitre = linemitre
                           )
                         )
                       )
                       },
                       syn_data = function(x, layer) {
                         params <- syn_layer_params(layer)
                         context <- layer$syn_plot_context %||% NULL
                         syn_to_nuclink_df(
                           x = x,
                           alignment = params$alignment,
                           reference = params$reference,
                           chr = params$chr,
                           subset = params$subset,
                           filter_by_len = params$filter_by_len,
                           context = context
                         )
                       },
                       syn_default_aes = c(
                         "tspecies", "tchr", "tstart", "tend", "strand",
                         "qspecies", "qchr", "qstart", "qend", "group", "track",
                         "target_anchor_y", "query_anchor_y"
                       )
)

.transform_link_x_by_source_panel <- function(data, panel_params, coord) {
  if (!"source_panel" %in% names(data)) {
    cli::cli_abort("Link data must contain a {.field source_panel} column for x transformation.")
  }

  panel_ids <- unique(stats::na.omit(as.integer(data$source_panel)))
  if (length(panel_ids) == 0L) {
    cli::cli_abort("Link data does not define any source x panels.")
  }

  transformed <- lapply(panel_ids, function(panel_id) {
    panel_data <- data[as.integer(data$source_panel) == panel_id, , drop = FALSE]
    if (nrow(panel_data) == 0L) {
      return(NULL)
    }
    if (panel_id < 1L || panel_id > length(panel_params)) {
      cli::cli_abort("Link source panel {.val {panel_id}} is out of bounds for the current layout.")
    }

    coord$transform_x(panel_data, panel_params[[panel_id]])
  })

  transformed <- Filter(Negate(is.null), transformed)
  if (length(transformed) == 0L) {
    return(data[0, , drop = FALSE])
  }

  dplyr::bind_rows(transformed)
}
