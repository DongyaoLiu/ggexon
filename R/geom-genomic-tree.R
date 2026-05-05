#' Attach a tree for genomic-panel alignment
#'
#' `geom_genomic_tree()` is a ggexon-native tree layer specification. It stores
#' a rectangular ggtree layout on the plot so [facet_genomictree()] can order
#' genomic panels by tree tips and inject a single spanning tree panel.
#'
#' @param tree Optional tree object accepted by `ggtree::ggtree()`.
#' @param tree_plot Optional existing rectangular `ggtree` plot. If supplied,
#'   `tree` is ignored.
#' @param layout ggtree layout. Currently only `"rectangular"` is supported.
#' @param individual Optional individual selector. When named, names are tree
#'   tip labels and values are `SynSpecies` individual ids.
#' @param tree_width Grid unit width of the tree column.
#' @param colour,linecolour Tree segment colour.
#' @param linewidth Tree segment linewidth.
#'
#' @return A ggexon tree specification consumed by [facet_genomictree()].
#' @export
geom_genomic_tree <- function(tree = NULL,
                              tree_plot = NULL,
                              layout = "rectangular",
                              individual = NULL,
                              tree_width = grid::unit(1.5, "in"),
                              colour = "black",
                              linecolour = NULL,
                              linewidth = 0.5) {
  if (!identical(layout, "rectangular")) {
    stop("Only `layout = \"rectangular\"` is currently supported.", call. = FALSE)
  }
  structure(
    list(
      tree = tree,
      tree_plot = tree_plot,
      layout = layout,
      individual = individual,
      tree_width = tree_width,
      colour = linecolour %||% colour,
      linewidth = linewidth
    ),
    class = "ggexon_genomic_tree_spec"
  )
}

#' @export
ggplot_add.ggexon_genomic_tree_spec <- function(object, plot, object_name) {
  if (!is_ggexon(plot)) {
    stop("`geom_genomic_tree()` can only be added to a ggexon plot.", call. = FALSE)
  }
  plot@genomic_tree <- object
  plot
}
