#' Compile a tree-aligned genomic track layout
#'
#' `compile_ggtree_genomic_alignment()` prepares a ggalign-style layout where a
#' rectangular tree provides the shared tip y positions and each matched
#' `SynSpecies` individual keeps its own genomic x coordinate system.
#'
#' @param x A `SynSpecies` object.
#' @param tree Optional tree object accepted by `ggtree::ggtree()`.
#' @param tree_plot Optional existing rectangular `ggtree` plot. If supplied,
#'   `tree` is ignored. When both `tree` and `tree_plot` are omitted, stored
#'   values on `x` are used when present.
#' @param layout ggtree layout. Currently only `"rectangular"` is supported.
#' @param individual Optional individual selector. When named, names are tree
#'   tip labels and values are `SynSpecies` individual ids.
#' @param chr Optional chromosome/seqname. May be scalar or named by tree tip or
#'   individual id.
#' @param start,end Optional coordinate bounds. May be scalar or named by tree
#'   tip or individual id.
#' @param subset Optional numeric length-2 bounds. May be a scalar vector or a
#'   named list keyed by tree tip or individual id. Overrides `start` and `end`.
#' @param feature_type Feature type passed to [query_features()]. Defaults to
#'   `"gene"`.
#' @param inter_genetic,exon_length Deprecated x-layout arguments. Only
#'   `"scaled"` is supported. Use [strip_scale_x()] in ggexon plots for
#'   gene-tag x-coordinate normalization.
#' @param tree_track Facet-track label assigned to tree segments.
#'
#' @return A `ggtree_genomic_alignment` list with `tip_layout`,
#'   `tree_segments`, and `gene_tags` data frames.
#' @export
compile_ggtree_genomic_alignment <- function(x,
                                             tree = NULL,
                                             tree_plot = NULL,
                                             layout = "rectangular",
                                             individual = NULL,
                                             chr = NULL,
                                             start = NULL,
                                             end = NULL,
                                             subset = NULL,
                                             feature_type = "gene",
                                             inter_genetic = c("scaled", "union"),
                                             exon_length = c("scaled", "union"),
                                             tree_track = "Tree") {
  if (!methods::is(x, "SynSpecies")) {
    stop("`compile_ggtree_genomic_alignment()` expects a SynSpecies object.", call. = FALSE)
  }
  if (!identical(layout, "rectangular")) {
    stop("Only `layout = \"rectangular\"` is currently supported.", call. = FALSE)
  }
  inter_genetic <- match.arg(inter_genetic)
  exon_length <- match.arg(exon_length)
  .genetag_abort_layout_mode(inter_genetic = inter_genetic, exon_length = exon_length)
  tree_inputs <- .resolve_synspecies_tree_inputs(x, tree = tree, tree_plot = tree_plot)
  tree <- tree_inputs$tree
  tree_plot <- tree_inputs$tree_plot

  tree_data <- .ggtree_rectangular_plot_data(tree = tree, tree_plot = tree_plot, layout = layout)
  tip_data <- .ggtree_rectangular_tip_data_from_data(tree_data)
  tip_map <- .genetag_tip_individual_map(
    tip_labels = tip_data$label,
    available_individuals = names(individuals(x)),
    individual = individual
  )
  if (nrow(tip_map) == 0L) {
    stop("No tree tips match SynSpecies individuals.", call. = FALSE)
  }

  tip_rows <- tip_data[match(tip_map$id, tip_data$label), , drop = FALSE]
  tip_layout <- data.frame(
    id = tip_map$id,
    individual = tip_map$individual,
    node = tip_rows$node,
    tree_x = tip_rows$x,
    tree_y = tip_rows$y,
    stringsAsFactors = FALSE
  )
  tip_layout <- tip_layout[order(tip_layout$tree_y), , drop = FALSE]
  rownames(tip_layout) <- NULL

  gene_tags <- compile_ggtree_genetag(
    x = x,
    tree = tree,
    tree_plot = tree_plot,
    layout = layout,
    individual = individual,
    chr = chr,
    start = start,
    end = end,
    subset = subset,
    feature_type = feature_type,
    inter_genetic = inter_genetic,
    exon_length = exon_length,
    include_y = TRUE
  )
  if (nrow(gene_tags) > 0L) {
    gene_tags$alignment_id <- gene_tags$id
    gene_tags$alignment_panel <- gene_tags$individual
  }

  out <- list(
    tip_layout = tip_layout,
    tree_segments = .ggtree_rectangular_segments_from_data(tree_data, track = tree_track),
    gene_tags = gene_tags,
    tree_track = tree_track,
    layout = layout
  )
  class(out) <- c("ggtree_genomic_alignment", class(out))
  out
}

#' Plot a tree-aligned genomic track layout
#'
#' `plot_ggtree_genomic_alignment()` assembles one tree panel beside one
#' species-local genomic panel per tree tip. The tree panel spans all genomic
#' rows, and all panels share the tree-derived tip y positions.
#'
#' @param alignment A `ggtree_genomic_alignment` object returned by
#'   [compile_ggtree_genomic_alignment()].
#' @param mapping Aesthetic mapping passed to [geom_genetag()]. Defaults to
#'   `ggplot2::aes(fill = gene)`.
#' @param tree_width,track_width,label_width Grid units controlling column
#'   widths.
#' @param panel_height Grid unit controlling each tip row height.
#' @param exon_height,arrow_width,arrow_fraction Passed to [geom_genetag()].
#' @param tree_colour,tree_linewidth Tree segment styling.
#' @param show_track_labels Logical; draw tip/individual labels between the tree
#'   and genomic panels.
#' @param show_x_axis Logical; draw x axes for the tree and genomic panels.
#'   Defaults to `TRUE` so species-local coordinate systems are visible.
#' @param base_size Base font size used when `show_x_axis = TRUE`.
#' @param label_gp Optional `grid::gpar()` for track labels.
#' @param ... Additional fixed aesthetics passed to [geom_genetag()].
#'
#' @return A `gtable` object. Print it or call `grid::grid.draw()`.
#' @export
plot_ggtree_genomic_alignment <- function(alignment,
                                          mapping = ggplot2::aes(fill = gene),
                                          tree_width = grid::unit(1.5, "in"),
                                          label_width = grid::unit(0.75, "in"),
                                          track_width = grid::unit(1, "null"),
                                          panel_height = grid::unit(0.5, "in"),
                                          exon_height = 0.6,
                                          arrow_width = NULL,
                                          arrow_fraction = 0.18,
                                          tree_colour = "black",
                                          tree_linewidth = 0.5,
                                          show_track_labels = TRUE,
                                          show_x_axis = TRUE,
                                          base_size = 9,
                                          label_gp = grid::gpar(fontsize = 9),
                                          ...) {
  if (!inherits(alignment, "ggtree_genomic_alignment")) {
    stop("`alignment` must come from `compile_ggtree_genomic_alignment()`.", call. = FALSE)
  }

  tip_layout <- alignment$tip_layout
  if (nrow(tip_layout) == 0L) {
    stop("`alignment` contains no tip rows.", call. = FALSE)
  }
  tip_layout <- tip_layout[order(-tip_layout$tree_y), , drop = FALSE]
  rownames(tip_layout) <- NULL
  y_limits <- range(alignment$tip_layout$tree_y, na.rm = TRUE) + c(-0.5, 0.5)

  tree_x_limits <- .ggtree_alignment_x_limits(
    c(alignment$tree_segments$x, alignment$tree_segments$xend),
    pad_mult = c(0.03, 0.05)
  )
  tree_plot <- ggplot2::ggplot(alignment$tree_segments) +
    ggplot2::geom_segment(
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      colour = tree_colour,
      linewidth = tree_linewidth,
      lineend = "square"
    ) +
    ggplot2::coord_cartesian(xlim = tree_x_limits, ylim = y_limits, expand = FALSE, clip = "off") +
    .ggtree_alignment_panel_theme(
      show_x_axis = show_x_axis,
      base_size = base_size,
      plot_margin = ggplot2::margin(0, 2, 0, 0)
    )
  tree_grob <- ggplot2::ggplotGrob(tree_plot)

  widths <- grid::unit.c(tree_width, label_width, track_width)
  if (!isTRUE(show_track_labels)) {
    widths <- grid::unit.c(tree_width, grid::unit(0, "pt"), track_width)
  }
  out <- gtable::gtable(
    widths = widths,
    heights = rep(panel_height, nrow(tip_layout))
  )
  out <- gtable::gtable_add_grob(
    out,
    tree_grob,
    t = 1L,
    l = 1L,
    b = nrow(tip_layout),
    r = 1L,
    clip = "off",
    name = "tree"
  )

  gene_tags <- alignment$gene_tags
  for (i in seq_len(nrow(tip_layout))) {
    tip_id <- tip_layout$id[[i]]
    individual_id <- tip_layout$individual[[i]]
    tip_y <- tip_layout$tree_y[[i]]
    tip_rows <- gene_tags[
      gene_tags$id == tip_id & gene_tags$individual == individual_id,
      ,
      drop = FALSE
    ]
    track_x_limits <- .ggtree_alignment_x_limits(
      c(tip_rows$xmin, tip_rows$xmax),
      pad_mult = c(0.03, 0.03)
    )
    track_plot <- ggplot2::ggplot(tip_rows) +
      geom_genetag(
        mapping = mapping,
        exon_height = exon_height,
        arrow_width = arrow_width,
        arrow_fraction = arrow_fraction,
        ...
      ) +
      ggplot2::coord_cartesian(
        xlim = track_x_limits,
        ylim = c(tip_y - 0.5, tip_y + 0.5),
        expand = FALSE,
        clip = "off"
      ) +
      .ggtree_alignment_panel_theme(
        show_x_axis = show_x_axis,
        base_size = base_size,
        plot_margin = ggplot2::margin(0, 0, 0, 2)
      )

    if (isTRUE(show_track_labels)) {
      label <- grid::textGrob(
        individual_id,
        x = grid::unit(1, "npc"),
        y = grid::unit(0.5, "npc"),
        just = c("right", "center"),
        gp = label_gp
      )
      out <- gtable::gtable_add_grob(
        out,
        label,
        t = i,
        l = 2L,
        clip = "off",
        name = paste0("label-", individual_id)
      )
    }

    out <- gtable::gtable_add_grob(
      out,
      ggplot2::ggplotGrob(track_plot),
      t = i,
      l = 3L,
      clip = "off",
      name = paste0("track-", individual_id)
    )
  }

  class(out) <- c("ggtree_genomic_alignment_gtable", class(out))
  attr(out, "alignment") <- alignment
  out
}

#' @export
print.ggtree_genomic_alignment_gtable <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(x)
  invisible(x)
}

.ggtree_alignment_panel_theme <- function(show_x_axis = TRUE,
                                          base_size = 9,
                                          plot_margin = ggplot2::margin(0, 0, 0, 0)) {
  if (isTRUE(show_x_axis)) {
    return(
      ggplot2::theme_minimal(base_size = base_size) +
        ggplot2::theme(
          axis.title = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          legend.position = "none",
          plot.margin = plot_margin
        )
    )
  }

  ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = plot_margin
    )
}
