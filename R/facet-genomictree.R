#' Facet genomic panels by tree-tip order
#'
#' `facet_genomictree()` lays out one genomic panel per tree-matched
#' individual, ordered by the y positions in [geom_genomic_tree()]. During
#' rendering, ggexon injects a single tree panel that spans all genomic rows.
#'
#' @param facets Faceting variables. Defaults to `ggplot2::vars(track)`.
#' @param scales One of `"fixed"`, `"free_x"`, `"free_y"`, or `"free"`.
#' @param shrink Passed through to the facet.
#' @param labeller A labeller specification.
#' @param as.table Logical; whether panels are laid out like a table.
#' @param switch Deprecated ggplot2 argument.
#' @param drop Logical; drop unused facet levels?
#' @param strip.position Position of facet strips.
#' @param axes Which axes to draw.
#' @param axis.labels Which axis labels to draw.
#' @param show_tree_x_axis Logical; draw the tree branch-length x axis.
#' @param label_position Position for tree-tip labels. One of `"left"`,
#'   `"right"`, or `"none"`. `"left"` places labels between the tree and
#'   genomic panels.
#' @param label_width Grid unit width for the tip-label column.
#'
#' @return A `FacetGenomicTree` ggproto object.
#' @export
facet_genomictree <- function(facets = ggplot2::vars(track),
                              scales = "free_x",
                              shrink = TRUE,
                              labeller = "label_value",
                              as.table = TRUE,
                              switch = deprecated(),
                              drop = TRUE,
                              strip.position = "top",
                              axes = "margins",
                              axis.labels = "all",
                              show_tree_x_axis = TRUE,
                              label_position = c("left", "right", "none"),
                              label_width = grid::unit(0.7, "in")) {
  scales <- arg_match0(scales %||% "free_x", c("fixed", "free_x", "free_y", "free"))
  label_position <- match.arg(label_position)
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )

  draw_axes <- arg_match0(axes, c("margins", "all_x", "all_y", "all"))
  draw_axes <- list(
    x = free$x || any(draw_axes %in% c("all_x", "all")),
    y = free$y || any(draw_axes %in% c("all_y", "all"))
  )
  axis_labels <- arg_match0(axis.labels, c("margins", "all_x", "all_y", "all"))
  axis_labels <- list(
    x = free$x || !draw_axes$x || any(axis_labels %in% c("all_x", "all")),
    y = free$y || !draw_axes$y || any(axis_labels %in% c("all_y", "all"))
  )
  labeller <- ggplot2:::validate_labeller(labeller)
  facets <- ggplot2:::compact_facets(facets)
  strip.position <- arg_match0(strip.position, c("top", "bottom", "left", "right"))

  ggproto(NULL, FacetGenomicTree,
    shrink = shrink,
    params = list(
      facets = facets,
      free = free,
      as.table = as.table,
      strip.position = strip.position,
      drop = drop,
      ncol = 1L,
      nrow = NULL,
      labeller = labeller,
      dir = "tl",
      draw_axes = draw_axes,
      axis_labels = axis_labels,
      show_tree_x_axis = show_tree_x_axis,
      label_position = label_position,
      label_width = label_width
    )
  )
}

#' ggproto backend for `facet_genomictree()`
#'
#' @export
FacetGenomicTree <- ggproto("FacetGenomicTree", FacetGenomics,
  compute_layout = function(self, data, params) {
    if (methods::is(params$plot_data, "SynSpecies") && !is.null(params$genomic_tree)) {
      return(.compute_genomictree_layout(data, params))
    }
    layout <- ggplot2::ggproto_parent(FacetGenomics, self)$compute_layout(data, params)
    if (!is.null(params$genomic_tree)) {
      layout <- .order_manual_genomictree_layout(layout, params)
    }
    layout
  }
)

.compute_genomictree_layout <- function(data, params) {
  tree_spec <- params$genomic_tree
  tree_data <- .ggtree_rectangular_plot_data(
    tree = tree_spec$tree,
    tree_plot = tree_spec$tree_plot,
    layout = tree_spec$layout
  )
  tip_data <- .ggtree_rectangular_tip_data_from_data(tree_data)
  tip_map <- .genetag_tip_individual_map(
    tip_labels = tip_data$label,
    available_individuals = names(individuals(params$plot_data)),
    individual = tree_spec$individual
  )
  if (nrow(tip_map) == 0L) {
    stop("No tree tips match SynSpecies individuals.", call. = FALSE)
  }

  annotation_species <- .annotation_species_from_layers(data)
  if (length(annotation_species) > 0L) {
    tip_map <- tip_map[tip_map$individual %in% annotation_species, , drop = FALSE]
  }
  if (nrow(tip_map) == 0L) {
    stop("No tree tips match the genomic layer tracks.", call. = FALSE)
  }

  tip_rows <- tip_data[match(tip_map$id, tip_data$label), , drop = FALSE]
  panels <- data.frame(
    PANEL = seq_len(nrow(tip_map)),
    ROW = seq_len(nrow(tip_map)),
    COL = 1L,
    track = tip_map$individual,
    panel_type = "annotation",
    species = tip_map$individual,
    tree_id = tip_map$id,
    tree_node = tip_rows$node,
    tree_x = tip_rows$x,
    tree_y = tip_rows$y,
    stringsAsFactors = FALSE
  )
  panels <- panels[order(-panels$tree_y), , drop = FALSE]
  panels$PANEL <- seq_len(nrow(panels))
  panels$ROW <- seq_len(nrow(panels))
  panels$COL <- 1L
  panels$SCALE_X <- if (params$free$x) seq_len(nrow(panels)) else 1L
  panels$SCALE_Y <- if (params$free$y) seq_len(nrow(panels)) else 1L
  rownames(panels) <- NULL
  panels
}

.order_manual_genomictree_layout <- function(layout, params) {
  if (nrow(layout) == 0L) {
    return(layout)
  }

  track_column <- .genomictree_layout_track_column(layout, params)
  if (is.null(track_column)) {
    return(layout)
  }

  tree_spec <- params$genomic_tree
  tree_data <- .ggtree_rectangular_plot_data(
    tree = tree_spec$tree,
    tree_plot = tree_spec$tree_plot,
    layout = tree_spec$layout
  )
  tip_data <- .ggtree_rectangular_tip_data_from_data(tree_data)
  track_values <- as.character(layout[[track_column]])
  tip_idx <- match(track_values, tip_data$label)
  tree_y <- tip_data$y[tip_idx]
  if (!any(is.finite(tree_y))) {
    return(layout)
  }

  if (!"track" %in% names(layout)) {
    layout$track <- track_values
  }
  layout$tree_id <- tip_data$label[tip_idx]
  layout$tree_node <- tip_data$node[tip_idx]
  layout$tree_x <- tip_data$x[tip_idx]
  layout$tree_y <- tree_y

  matched <- is.finite(layout$tree_y)
  sort_key <- ifelse(matched, -layout$tree_y, Inf)
  layout <- layout[order(sort_key, layout$PANEL), , drop = FALSE]
  layout$PANEL <- seq_len(nrow(layout))
  layout$ROW <- seq_len(nrow(layout))
  layout$COL <- 1L
  layout$SCALE_X <- if (params$free$x) seq_len(nrow(layout)) else 1L
  layout$SCALE_Y <- if (params$free$y) seq_len(nrow(layout)) else 1L
  rownames(layout) <- NULL
  layout
}

.genomictree_layout_track_column <- function(layout, params) {
  if ("track" %in% names(layout)) {
    return("track")
  }

  facet_names <- unique(c(
    names(params$facets),
    vapply(params$facets, rlang::as_name, character(1))
  ))
  facet_names <- facet_names[nzchar(facet_names)]
  facet_names <- facet_names[facet_names %in% names(layout)]
  if (length(facet_names) == 0L) {
    return(NULL)
  }
  facet_names[[1L]]
}
