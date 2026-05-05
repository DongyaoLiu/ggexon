#' Compile gene-tag rows for a rectangular ggtree panel
#'
#' `compile_ggtree_genetag()` extracts rectangular-layout tip positions from a
#' ggtree plot or tree object, matches tip labels to `SynSpecies` individuals,
#' and returns one row per gene feature. The first column is `id`, matching
#' ggtree's `facet_plot()` convention.
#'
#' @param x A `SynSpecies` object.
#' @param tree Optional tree object accepted by `ggtree::ggtree()`.
#' @param tree_plot Optional existing `ggtree` plot. If supplied, `tree` is
#'   ignored.
#' @param layout ggtree layout. Currently only `"rectangular"` is supported.
#' @param individual Optional individual selector. When named, names are tree
#'   tip labels and values are `SynSpecies` individual ids. When unnamed, values
#'   are used as both tip labels and individual ids. When `NULL`, matching uses
#'   identical tree-tip labels and individual ids.
#' @param chr Optional chromosome/seqname. May be a scalar or a named vector/list
#'   keyed by tree tip or individual id.
#' @param start,end Optional coordinate bounds. May be scalar or named by tree
#'   tip or individual id.
#' @param subset Optional numeric length-2 bounds. May be a scalar vector or a
#'   named list keyed by tree tip or individual id. Overrides `start` and `end`.
#' @param feature_type Feature type passed to [query_features()]. Defaults to
#'   `"gene"`.
#' @param inter_genetic Intergenic-gap layout mode. `"scaled"` keeps the
#'   original gap between consecutive features within each track. `"union"` uses
#'   the maximum gap observed at each feature step so corresponding gaps are the
#'   same across tracks in the same panel.
#' @param exon_length Feature-length layout mode. `"scaled"` keeps original
#'   feature lengths. `"union"` uses the maximum feature length observed at each
#'   feature step so corresponding features have the same displayed length
#'   across tracks in the same panel.
#' @param include_y Logical; when `TRUE`, also include a `y` column copied from
#'   `tree_y`. Keep the default `FALSE` for `ggtree::facet_plot()`, which
#'   injects its own `y` column after tip matching.
#'
#' @return A `data.frame` ready for `ggtree::facet_plot()` and
#'   [geom_genetag()]. It contains `id`, `individual`, `tree_y`, `xmin`,
#'   `xmax`, `strand`, and gene metadata columns.
#' @export
compile_ggtree_genetag <- function(x,
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
                                   include_y = FALSE) {
  if (!methods::is(x, "SynSpecies")) {
    stop("`compile_ggtree_genetag()` expects a SynSpecies object.", call. = FALSE)
  }
  if (!identical(layout, "rectangular")) {
    stop("Only `layout = \"rectangular\"` is currently supported.", call. = FALSE)
  }
  inter_genetic <- match.arg(inter_genetic)
  exon_length <- match.arg(exon_length)

  tip_data <- .ggtree_rectangular_tip_data(tree = tree, tree_plot = tree_plot, layout = layout)
  tip_map <- .genetag_tip_individual_map(
    tip_labels = tip_data$label,
    available_individuals = names(individuals(x)),
    individual = individual
  )
  if (nrow(tip_map) == 0L) {
    return(.empty_ggtree_genetag_df(include_y = include_y))
  }

  rows <- vector("list", nrow(tip_map))
  for (i in seq_len(nrow(tip_map))) {
    tip_id <- tip_map$id[[i]]
    individual_id <- tip_map$individual[[i]]
    tip_row <- tip_data[match(tip_id, tip_data$label), , drop = FALSE]
    individual_obj <- individuals(x)[[individual_id]]
    window <- .genetag_window_for_individual(
      individual = individual_obj,
      tip_id = tip_id,
      individual_id = individual_id,
      chr = chr,
      start = start,
      end = end,
      subset = subset
    )

    gene_gr <- query_features(
      individual_obj,
      chr = window$chr,
      start = window$start,
      end = window$end,
      feature_type = feature_type,
      all = is.null(window$chr) && is.null(window$start) && is.null(window$end)
    )
    rows[[i]] <- .genetag_gr_to_df(
      gene_gr = gene_gr,
      id = tip_id,
      individual = individual_id,
      tree_node = tip_row$node[[1L]],
      tree_x = tip_row$x[[1L]],
      tree_y = tip_row$y[[1L]],
      include_y = include_y
    )
  }

  out <- do.call(rbind, rows)
  if (is.null(out) || nrow(out) == 0L) {
    return(.empty_ggtree_genetag_df(include_y = include_y))
  }
  rownames(out) <- NULL
  out <- .genetag_apply_layout_modes(
    out,
    inter_genetic = inter_genetic,
    exon_length = exon_length
  )
  out$group <- seq_len(nrow(out))
  out
}

#' Compile rectangular ggtree branches for a ggexon tree panel
#'
#' `compile_ggtree_rectangular_segments()` converts a rectangular ggtree plot or
#' tree object into plain segment rows. The returned data can be drawn with
#' `ggplot2::geom_segment()` inside `ggexon()` and aligned to a gene-tag panel
#' with [facet_genomics()].
#'
#' @param tree Optional tree object accepted by `ggtree::ggtree()`.
#' @param tree_plot Optional existing `ggtree` plot. If supplied, `tree` is
#'   ignored.
#' @param layout ggtree layout. Currently only `"rectangular"` is supported.
#' @param track Facet-track value assigned to all branch segments.
#'
#' @return A `data.frame` with `track`, `x`, `xend`, `y`, `yend`, `node`,
#'   `parent`, `segment`, `isTip`, and `label` columns.
#' @export
compile_ggtree_rectangular_segments <- function(tree = NULL,
                                                tree_plot = NULL,
                                                layout = "rectangular",
                                                track = "Tree") {
  if (!identical(layout, "rectangular")) {
    stop("Only `layout = \"rectangular\"` is currently supported.", call. = FALSE)
  }
  tree_data <- .ggtree_rectangular_plot_data(tree = tree, tree_plot = tree_plot, layout = layout)
  .ggtree_rectangular_segments_from_data(tree_data, track = track)
}

#' @export
ggplot_add.ggtree <- function(object, plot, object_name) {
  if (!inherits(plot, "ggexon")) {
    stop(
      "Adding a ggtree object with `+` is supported only for `ggexon()` plots. ",
      "Use ggtree's own layer grammar inside ggtree plots.",
      call. = FALSE
    )
  }

  tree_segments <- compile_ggtree_rectangular_segments(tree_plot = object)
  plot + ggplot2::geom_segment(
    data = tree_segments,
    mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE
  )
}

#' Draw gene tags as exon bodies with strand-direction triangles
#'
#' `geom_genetag()` draws each stranded gene as a rectangular exon-like body
#' plus a symmetric terminal triangle. The triangle apex points toward the
#' strand-specific end of the feature. It is designed for gene-level summaries,
#' including ggtree side panels generated with `ggtree::facet_plot()`.
#'
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard
#'   ggplot2 layer arguments. `inherit.aes` defaults to `FALSE` so the layer can
#'   be used cleanly in `ggtree::facet_plot()` side panels.
#' @param exon_height Total tag height in y-axis units. Defaults to `0.8`.
#' @param height Deprecated-compatible alias for `exon_height`.
#' @param arrow_width Optional width of the terminal triangle in x-axis units.
#'   When `NULL`, width is calculated from `arrow_fraction`.
#' @param arrow_fraction Fraction of each gene span used for the terminal
#'   triangle when `arrow_width` is `NULL`.
#'
#' @return A ggplot layer.
#' @export
geom_genetag <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         ...,
                         exon_height = NULL,
                         height = NULL,
                         arrow_width = NULL,
                         arrow_fraction = 0.18,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = FALSE) {
  mapping <- .genetag_complete_mapping(mapping, data)
  layer(
    data = data,
    mapping = mapping,
    geom = GeomGeneTag,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      ...,
      exon_height = exon_height,
      height = height,
      arrow_width = arrow_width,
      arrow_fraction = arrow_fraction,
      na.rm = na.rm
    )
  )
}

GeomGeneTag <- ggproto(
  "GeomGeneTag",
  Geom,
  required_aes = c("xmin", "xmax", "y", "strand"),
  default_aes = aes(
    colour = "black",
    fill = "grey35",
    linewidth = 0.25,
    linetype = 1,
    alpha = NA
  ),
  extra_params = c("na.rm", "exon_height", "height", "arrow_width", "arrow_fraction"),
  default_params = function() {
    list(
      exon_height = NULL,
      height = NULL,
      arrow_width = NULL,
      arrow_fraction = 0.18,
      na.rm = FALSE
    )
  },
  handle_na = function(data, params) {
    missing <- is.na(data$xmin) | is.na(data$xmax) | is.na(data$y) | is.na(data$strand)
    if (any(missing)) {
      if (!isTRUE(params$na.rm)) {
        warning(
          "Removed ", sum(missing), " row(s) containing missing values in geom_genetag().",
          call. = FALSE
        )
      }
      data <- data[!missing, , drop = FALSE]
    }
    data
  },
  draw_panel = function(data,
                        panel_params,
                        coord,
                        flipped_aes = FALSE,
                        exon_height = NULL,
                        height = NULL,
                        arrow_width = NULL,
                        arrow_fraction = 0.18) {
    if (nrow(data) == 0L) {
      return(zeroGrob())
    }
    tag_data <- .genetag_polygon_data(
      data = data,
      exon_height = exon_height,
      height = height,
      arrow_width = arrow_width,
      arrow_fraction = arrow_fraction
    )
    ggname("geom_genetag", GeomPolygon$draw_panel(tag_data, panel_params, coord))
  },
  draw_key = draw_key_polygon
)

.ggtree_rectangular_plot_data <- function(tree = NULL, tree_plot = NULL, layout = "rectangular") {
  if (is.null(tree_plot)) {
    if (is.null(tree)) {
      stop("Supply either `tree` or `tree_plot`.", call. = FALSE)
    }
    if (!requireNamespace("ggtree", quietly = TRUE)) {
      stop("Package `ggtree` is required to compile ggtree gene tags.", call. = FALSE)
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

.genetag_tip_individual_map <- function(tip_labels, available_individuals, individual = NULL) {
  tip_labels <- unique(as.character(tip_labels))
  available_individuals <- unique(as.character(available_individuals))

  if (is.null(individual)) {
    matched <- intersect(tip_labels, available_individuals)
    return(data.frame(id = matched, individual = matched, stringsAsFactors = FALSE))
  }

  individual <- as.character(individual)
  if (anyNA(individual) || any(!nzchar(individual))) {
    stop("`individual` must contain only non-empty values.", call. = FALSE)
  }
  if (!is.null(names(individual)) && any(nzchar(names(individual)))) {
    ids <- names(individual)
    ids[!nzchar(ids)] <- individual[!nzchar(ids)]
  } else {
    ids <- individual
  }

  missing_tips <- setdiff(ids, tip_labels)
  if (length(missing_tips) > 0L) {
    stop("Tree tip labels not found: ", paste(missing_tips, collapse = ", "), call. = FALSE)
  }
  missing_individuals <- setdiff(individual, available_individuals)
  if (length(missing_individuals) > 0L) {
    stop(
      "SynSpecies individuals not found: ",
      paste(missing_individuals, collapse = ", "),
      call. = FALSE
    )
  }

  data.frame(id = ids, individual = individual, stringsAsFactors = FALSE)
}

.genetag_window_for_individual <- function(individual,
                                           tip_id,
                                           individual_id,
                                           chr = NULL,
                                           start = NULL,
                                           end = NULL,
                                           subset = NULL) {
  subset_value <- .genetag_pick_value(subset, tip_id = tip_id, individual_id = individual_id)
  start_value <- .genetag_pick_value(start, tip_id = tip_id, individual_id = individual_id)
  end_value <- .genetag_pick_value(end, tip_id = tip_id, individual_id = individual_id)
  chr_value <- .genetag_pick_value(chr, tip_id = tip_id, individual_id = individual_id)

  if (!is.null(subset_value)) {
    if (!is.numeric(subset_value) || length(subset_value) != 2L || anyNA(subset_value)) {
      stop("`subset` values must be numeric vectors of length 2.", call. = FALSE)
    }
    start_value <- min(subset_value)
    end_value <- max(subset_value)
  }

  if (!is.null(start_value)) {
    start_value <- .genetag_scalar_numeric(start_value, "start")
  }
  if (!is.null(end_value)) {
    end_value <- .genetag_scalar_numeric(end_value, "end")
  }
  if (!is.null(chr_value)) {
    if (!is.character(chr_value) || length(chr_value) != 1L || is.na(chr_value) || !nzchar(chr_value)) {
      stop("`chr` values must be single non-empty character values.", call. = FALSE)
    }
    chr_value <- resolve_syn_seqname_or_raw(individual, chr_value)
  }

  list(chr = chr_value, start = start_value, end = end_value)
}

.genetag_pick_value <- function(x, tip_id, individual_id) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.list(x) && !is.data.frame(x)) {
    if (!is.null(names(x))) {
      if (tip_id %in% names(x)) {
        return(x[[tip_id]])
      }
      if (individual_id %in% names(x)) {
        return(x[[individual_id]])
      }
    }
    if (length(x) == 1L) {
      return(x[[1L]])
    }
    stop("Named per-tip/per-individual list values are required for multi-value inputs.", call. = FALSE)
  }

  if (!is.null(names(x))) {
    if (tip_id %in% names(x)) {
      return(unname(x[[tip_id]]))
    }
    if (individual_id %in% names(x)) {
      return(unname(x[[individual_id]]))
    }
  }
  if (length(x) == 1L || (is.numeric(x) && length(x) == 2L)) {
    return(unname(x))
  }
  stop("Named per-tip/per-individual values are required for multi-value inputs.", call. = FALSE)
}

.genetag_scalar_numeric <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    stop("`", name, "` values must be single numeric values.", call. = FALSE)
  }
  as.numeric(x)
}

.genetag_gr_to_df <- function(gene_gr,
                              id,
                              individual,
                              tree_node,
                              tree_x,
                              tree_y,
                              include_y = FALSE) {
  if (length(gene_gr) == 0L) {
    return(.empty_ggtree_genetag_df(include_y = include_y))
  }

  meta <- S4Vectors::mcols(gene_gr)
  gene_ids <- .coalesce_character_cols(meta, c("gene_id", "gene_name", "ID", "Name"))
  gene_labels <- .coalesce_character_cols(meta, c("plot_label", "gene_name", "gene_id", "Name", "ID"))
  gene_ids[is.na(gene_ids) | !nzchar(gene_ids)] <- paste0("gene_", seq_len(length(gene_ids)))[
    is.na(gene_ids) | !nzchar(gene_ids)
  ]
  gene_labels[is.na(gene_labels) | !nzchar(gene_labels)] <- gene_ids[
    is.na(gene_labels) | !nzchar(gene_labels)
  ]

  out <- data.frame(
    id = rep(id, length(gene_gr)),
    individual = rep(individual, length(gene_gr)),
    node = rep(as.integer(tree_node), length(gene_gr)),
    tree_x = rep(as.numeric(tree_x), length(gene_gr)),
    tree_y = rep(as.numeric(tree_y), length(gene_gr)),
    chr = as.character(GenomeInfoDb::seqnames(gene_gr)),
    xmin = IRanges::start(gene_gr),
    xmax = IRanges::end(gene_gr),
    start = IRanges::start(gene_gr),
    end = IRanges::end(gene_gr),
    strand = as.character(BiocGenerics::strand(gene_gr)),
    gene_id = gene_ids,
    gene = gene_labels,
    label = gene_labels,
    track = rep(individual, length(gene_gr)),
    stringsAsFactors = FALSE
  )
  if (isTRUE(include_y)) {
    out$y <- out$tree_y
    out <- out[, c("id", setdiff(names(out), "id")), drop = FALSE]
  }
  out[order(out$xmin, out$xmax, out$gene_id), , drop = FALSE]
}

.genetag_apply_layout_modes <- function(data,
                                        inter_genetic = c("scaled", "union"),
                                        exon_length = c("scaled", "union")) {
  inter_genetic <- match.arg(inter_genetic)
  exon_length <- match.arg(exon_length)
  if (nrow(data) == 0L || (identical(inter_genetic, "scaled") && identical(exon_length, "scaled"))) {
    return(data)
  }
  if (!all(c("xmin", "xmax") %in% names(data))) {
    stop("Gene-tag layout data must contain `xmin` and `xmax` columns.", call. = FALSE)
  }
  if (any(!is.finite(data$xmin)) || any(!is.finite(data$xmax))) {
    stop("Gene-tag layout coordinates must be finite.", call. = FALSE)
  }

  track_col <- .genetag_layout_track_col(data)
  out <- data
  out$genomic_xmin <- out$xmin
  out$genomic_xmax <- out$xmax
  out$genomic_start <- if ("start" %in% names(out)) out$start else out$xmin
  out$genomic_end <- if ("end" %in% names(out)) out$end else out$xmax
  out$.row_id <- seq_len(nrow(out))
  out$.layout_track <- as.character(out[[track_col]])

  sorted <- out[order(out$.layout_track, out$xmin, out$xmax, out$.row_id), , drop = FALSE]
  split_rows <- split(seq_len(nrow(sorted)), sorted$.layout_track)

  sorted$.layout_index <- NA_integer_
  sorted$.feature_width <- pmax(0, sorted$xmax - sorted$xmin)
  sorted$.gap_after <- 0

  for (rows in split_rows) {
    sorted$.layout_index[rows] <- seq_along(rows)
    if (length(rows) > 1L) {
      gaps <- sorted$xmin[rows[-1L]] - sorted$xmax[rows[-length(rows)]]
      sorted$.gap_after[rows[-length(rows)]] <- pmax(0, gaps)
    }
  }

  width_by_index <- tapply(sorted$.feature_width, sorted$.layout_index, max, na.rm = TRUE)
  gap_by_index <- tapply(sorted$.gap_after, sorted$.layout_index, max, na.rm = TRUE)
  base_x <- min(sorted$xmin, na.rm = TRUE)

  for (rows in split_rows) {
    current_x <- base_x
    for (j in seq_along(rows)) {
      row <- rows[[j]]
      layout_index <- sorted$.layout_index[[row]]
      width <- if (identical(exon_length, "union")) {
        width_by_index[[as.character(layout_index)]]
      } else {
        sorted$.feature_width[[row]]
      }
      sorted$xmin[[row]] <- current_x
      sorted$xmax[[row]] <- current_x + width

      if (j < length(rows)) {
        gap <- if (identical(inter_genetic, "union")) {
          gap_by_index[[as.character(layout_index)]]
        } else {
          sorted$.gap_after[[row]]
        }
        current_x <- sorted$xmax[[row]] + gap
      }
    }
  }

  sorted$layout_index <- sorted$.layout_index
  sorted$layout_inter_genetic <- inter_genetic
  sorted$layout_exon_length <- exon_length
  sorted <- sorted[order(sorted$.row_id), , drop = FALSE]
  sorted$.row_id <- NULL
  sorted$.layout_track <- NULL
  sorted$.layout_index <- NULL
  sorted$.feature_width <- NULL
  sorted$.gap_after <- NULL
  rownames(sorted) <- NULL
  sorted
}

.genetag_layout_track_col <- function(data) {
  for (col in c("id", "track", "individual")) {
    if (col %in% names(data)) {
      return(col)
    }
  }
  stop("Gene-tag layout data must contain one of `id`, `track`, or `individual`.", call. = FALSE)
}

.empty_ggtree_genetag_df <- function(include_y = FALSE) {
  out <- data.frame(
    id = character(),
    individual = character(),
    node = integer(),
    tree_x = numeric(),
    tree_y = numeric(),
    chr = character(),
    xmin = numeric(),
    xmax = numeric(),
    start = numeric(),
    end = numeric(),
    strand = character(),
    gene_id = character(),
    gene = character(),
    label = character(),
    track = character(),
    group = integer(),
    stringsAsFactors = FALSE
  )
  if (isTRUE(include_y)) {
    out$y <- numeric()
    out <- out[, c("id", setdiff(names(out), "id")), drop = FALSE]
  }
  out
}

.genetag_complete_mapping <- function(mapping, data) {
  mapping_exprs <- if (is.null(mapping)) {
    list()
  } else {
    as.list(mapping)
  }
  for (col in c("xmin", "xmax", "y", "strand")) {
    if (!col %in% names(mapping_exprs)) {
      mapping_exprs[[col]] <- rlang::sym(col)
    }
  }
  if (length(mapping_exprs) == 0L) {
    return(mapping)
  }
  rlang::inject(ggplot2::aes(!!!mapping_exprs))
}

.genetag_polygon_data <- function(data,
                                  exon_height = NULL,
                                  height = NULL,
                                  arrow_width = NULL,
                                  arrow_fraction = 0.18) {
  exon_height <- .genetag_effective_height(exon_height = exon_height, height = height)
  arrow_fraction <- .genetag_positive_number(arrow_fraction, "arrow_fraction")
  if (arrow_fraction > 0.5) {
    stop("`arrow_fraction` must be no larger than 0.5.", call. = FALSE)
  }
  if (!is.null(arrow_width) && !is.na(arrow_width)) {
    arrow_width <- .genetag_positive_number(arrow_width, "arrow_width")
  } else {
    arrow_width <- NULL
  }

  pieces <- vector("list", nrow(data))
  for (i in seq_len(nrow(data))) {
    xmin <- min(data$xmin[[i]], data$xmax[[i]])
    xmax <- max(data$xmin[[i]], data$xmax[[i]])
    y <- data$y[[i]]
    strand <- .genetag_normalize_strand(data$strand[[i]])
    width <- xmax - xmin
    head_width <- if (is.null(arrow_width)) {
      width * arrow_fraction
    } else {
      min(arrow_width, width)
    }
    y_min <- y - exon_height / 2
    y_max <- y + exon_height / 2

    coords <- switch(
      strand,
      "+" = data.frame(
        x = c(xmin, xmax - head_width, xmax, xmax - head_width, xmin),
        y = c(y_min, y_min, y, y_max, y_max)
      ),
      "-" = data.frame(
        x = c(xmin + head_width, xmax, xmax, xmin + head_width, xmin),
        y = c(y_min, y_min, y_max, y_max, y)
      ),
      data.frame(
        x = c(xmin, xmax, xmax, xmin),
        y = c(y_min, y_min, y_max, y_max)
      )
    )

    row <- data[rep(i, nrow(coords)), , drop = FALSE]
    row$x <- coords$x
    row$y <- coords$y
    row$group <- i
    pieces[[i]] <- row
  }

  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

.genetag_effective_height <- function(exon_height = NULL, height = NULL) {
  if (!is.null(exon_height)) {
    return(.genetag_positive_number(exon_height, "exon_height"))
  }
  if (!is.null(height)) {
    return(.genetag_positive_number(height, "height"))
  }
  0.8
}

.genetag_positive_number <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
    stop("`", name, "` must be one positive numeric value.", call. = FALSE)
  }
  as.numeric(x)
}

.genetag_normalize_strand <- function(x) {
  x <- as.character(x)
  if (length(x) == 0L || is.na(x) || !nzchar(x)) {
    return("*")
  }
  x <- base::tolower(x[[1L]])
  if (x %in% c("+", "plus", "forward", "1")) {
    return("+")
  }
  if (x %in% c("-", "minus", "reverse", "-1")) {
    return("-")
  }
  "*"
}
