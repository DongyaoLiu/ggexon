# Shared rectangular ggtree layout helpers.
#
# These functions sit between ggtree and ggexon. ggtree is still responsible for
# computing rectangular tree coordinates; ggexon converts those coordinates into
# plain tip and segment tables that can be aligned to genomic panels.

.resolve_synspecies_tree_inputs <- function(x, tree = NULL, tree_plot = NULL) {
  if (!methods::is(x, "SynSpecies") || !is.null(tree) || !is.null(tree_plot)) {
    return(list(tree = tree, tree_plot = tree_plot))
  }
  list(
    tree = species_tree(x),
    tree_plot = species_tree_plot(x)
  )
}

.ggtree_rectangular_plot_data <- function(tree = NULL, tree_plot = NULL, layout = "rectangular") {
  if (is.null(tree_plot)) {
    if (is.null(tree)) {
      stop("Supply either `tree` or `tree_plot`.", call. = FALSE)
    }
    if (!requireNamespace("ggtree", quietly = TRUE)) {
      stop("Package `ggtree` is required to compile rectangular tree layouts.", call. = FALSE)
    }
    tree_plot <- ggtree::ggtree(tree, layout = layout)
  }

  tree_data <- tree_plot$data
  if (is.null(tree_data) || !is.data.frame(tree_data)) {
    stop("`tree_plot` must be a ggtree object with a data frame in `$data`.", call. = FALSE)
  }
  required_cols <- c("node", "parent", "x", "y", "isTip")
  missing_cols <- setdiff(required_cols, names(tree_data))
  if (length(missing_cols) > 0L) {
    stop(
      "`tree_plot$data` is missing required ggtree columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  if (!"label" %in% names(tree_data)) {
    tree_data$label <- NA_character_
  }
  tree_data$label <- as.character(tree_data$label)
  tree_data$node <- as.integer(tree_data$node)
  tree_data$parent <- as.integer(tree_data$parent)
  tree_data$x <- as.numeric(tree_data$x)
  tree_data$y <- as.numeric(tree_data$y)
  tree_data$isTip <- as.logical(tree_data$isTip)
  tree_data
}

.ggtree_rectangular_tip_data <- function(tree = NULL, tree_plot = NULL, layout = "rectangular") {
  tree_data <- .ggtree_rectangular_plot_data(tree = tree, tree_plot = tree_plot, layout = layout)
  .ggtree_rectangular_tip_data_from_data(tree_data)
}

.ggtree_rectangular_tip_data_from_data <- function(tree_data) {
  required_cols <- c("label", "node", "x", "y", "isTip")

  tip_data <- tree_data[tree_data$isTip %in% TRUE, required_cols, drop = FALSE]
  tip_data <- tip_data[!is.na(tip_data$label) & nzchar(tip_data$label), , drop = FALSE]
  tip_data$label <- as.character(tip_data$label)
  tip_data$node <- as.integer(tip_data$node)
  tip_data$x <- as.numeric(tip_data$x)
  tip_data$y <- as.numeric(tip_data$y)
  tip_data[order(tip_data$y), , drop = FALSE]
}

.ggtree_rectangular_segments_from_data <- function(tree_data, track = "Tree") {
  if (!is.character(track) || length(track) != 1L || is.na(track) || !nzchar(track)) {
    stop("`track` must be one non-empty character value.", call. = FALSE)
  }
  if (nrow(tree_data) == 0L) {
    return(.empty_ggtree_rectangular_segments_df())
  }

  node_key <- as.character(tree_data$node)
  parent_index <- match(as.character(tree_data$parent), node_key)
  has_parent <- !is.na(parent_index) & tree_data$parent != tree_data$node

  horizontal <- .empty_ggtree_rectangular_segments_df()
  if (any(has_parent)) {
    children <- tree_data[has_parent, , drop = FALSE]
    parents <- tree_data[parent_index[has_parent], , drop = FALSE]
    horizontal <- data.frame(
      track = rep(track, nrow(children)),
      segment = rep("horizontal", nrow(children)),
      node = children$node,
      parent = children$parent,
      isTip = children$isTip,
      label = children$label,
      x = parents$x,
      xend = children$x,
      y = children$y,
      yend = children$y,
      stringsAsFactors = FALSE
    )
  }

  vertical_pieces <- list()
  children_by_parent <- split(tree_data[has_parent, , drop = FALSE], tree_data$parent[has_parent])
  for (parent_node in names(children_by_parent)) {
    child_rows <- children_by_parent[[parent_node]]
    if (nrow(child_rows) < 2L) {
      next
    }
    parent_row <- tree_data[match(parent_node, node_key), , drop = FALSE]
    if (nrow(parent_row) != 1L) {
      next
    }
    vertical_pieces[[length(vertical_pieces) + 1L]] <- data.frame(
      track = track,
      segment = "vertical",
      node = parent_row$node,
      parent = parent_row$parent,
      isTip = FALSE,
      label = parent_row$label,
      x = parent_row$x,
      xend = parent_row$x,
      y = min(child_rows$y, na.rm = TRUE),
      yend = max(child_rows$y, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }

  vertical <- if (length(vertical_pieces) == 0L) {
    .empty_ggtree_rectangular_segments_df()
  } else {
    do.call(rbind, vertical_pieces)
  }

  out <- rbind(vertical, horizontal)
  rownames(out) <- NULL
  out[order(out$segment, out$parent, out$node), , drop = FALSE]
}

.empty_ggtree_rectangular_segments_df <- function() {
  data.frame(
    track = character(),
    segment = character(),
    node = integer(),
    parent = integer(),
    isTip = logical(),
    label = character(),
    x = numeric(),
    xend = numeric(),
    y = numeric(),
    yend = numeric(),
    stringsAsFactors = FALSE
  )
}

.ggtree_alignment_x_limits <- function(x, pad_mult = c(0.03, 0.03), fallback = c(0, 1)) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(fallback)
  }
  rng <- range(x)
  span <- diff(rng)
  if (span <= 0) {
    span <- 1
  }
  rng + c(-span * pad_mult[[1L]], span * pad_mult[[2L]])
}
