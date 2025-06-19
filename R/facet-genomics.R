#' @export
facet_genomics <- function(facets, nrow = NULL, ncol = NULL, scales = "fixed",
                       shrink = TRUE, labeller = "label_value", as.table = TRUE,
                       switch = deprecated(), drop = TRUE, dir = "h",
                       strip.position = 'top', axes = "margins",
                       axis.labels = "all", nuc_link = NULL, pro_link = NULL) {
  scales <- arg_match0(scales %||% "fixed", c("fixed", "free_x", "free_y", "free"))
  dir <- arg_match0(dir, c("h", "v"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )

  # If scales are free, always draw the axes
  draw_axes <- arg_match0(axes, c("margins", "all_x", "all_y", "all"))
  draw_axes <- list(
    x = free$x || any(draw_axes %in% c("all_x", "all")),
    y = free$y || any(draw_axes %in% c("all_y", "all"))
  )

  # Omitting labels is special-cased internally, so only omit labels if
  # scales are not free and the axis is to be drawn
  axis_labels <- arg_match0(axis.labels, c("margins", "all_x", "all_y", "all"))
  axis_labels <- list(
    x = free$x || !draw_axes$x || any(axis_labels %in% c("all_x", "all")),
    y = free$y || !draw_axes$y || any(axis_labels %in% c("all_y", "all"))
  )

  # Check for deprecated labellers
  labeller <- ggplot2:::check_labeller(labeller)

  # Flatten all facets dimensions into a single one
  facets <- ggplot2:::wrap_as_facets_list(facets)


  strip.position <- arg_match0(strip.position, c("top", "bottom", "left", "right"))

  check_number_whole(ncol, allow_null = TRUE, min = 1)
  check_number_whole(nrow, allow_null = TRUE, min = 1)

  if (identical(dir, "v")) {
    # swap
    tmp <- ncol
    ncol <- nrow
    nrow <- tmp
  }

  ggproto(NULL, FacetGenomics,
    shrink = shrink,
    params = list(
      facets = facets,
      free = free,
      as.table = as.table,
      strip.position = strip.position,
      drop = drop,
      ncol = ncol,
      nrow = nrow,
      labeller = labeller,
      dir = dir,
      draw_axes = draw_axes,
      axis_labels = axis_labels,
      nuc_link = nuc_link
    )
  )
}

#' @export
FacetGenomics <- ggproto("FacetGenomics", FacetWrap,


  compute_link_layout = function(self, link_data){

    link_table <- data.frame(
      Query = sapply(strsplit(names(link_data), "_"), `[`, 1),
      Target = sapply(strsplit(names(link_data), "_"), `[`, 2))

  },

  draw_panels = function(self, panels, layout, x_scales, y_scales, ranges, coord, data, theme, params, nuc_link_grob = NULL, pro_link_grob = NULL) {
    if ((params$free$x || params$free$y) && !coord$is_free()) {
      cli::cli_abort("{.fn {snake_class(self)}} can't use free scales with {.fn {snake_class(coord)}}.")
    }

  if (inherits(coord, "CoordFlip")) {
    if (params$free$x) {
      layout$SCALE_X <- seq_len(nrow(layout))
    } else {
      layout$SCALE_X <- 1L
    }
    if (params$free$y) {
      layout$SCALE_Y <- seq_len(nrow(layout))
    } else {
      layout$SCALE_Y <- 1L
    }
  }

  ncol <- max(layout$COL)
  nrow <- max(layout$ROW)
  n <- nrow(layout)
  panel_order <- order(layout$ROW, layout$COL)
  layout <- layout[panel_order, ]
  panels <- panels[panel_order]



  panel_pos <- ggplot2:::convertInd(layout$ROW, layout$COL, nrow)

  # Fill missing parameters for backward compatibility
  params$draw_axes   <- params$draw_axes   %||% list(x = FALSE, y = FALSE)
  params$axis_labels <- params$axis_labels %||% list(x = TRUE,  y = TRUE)

  x_axis_order <- if (params$axis_labels$x) layout$SCALE_X else seq(n)
  y_axis_order <- if (params$axis_labels$y) layout$SCALE_Y else seq(n)

  ranges <- ggplot2:::censor_labels(ranges, layout, params$axis_labels)
  axes <- ggplot2:::render_axes(ranges, ranges, coord, theme, transpose = TRUE)

  if (length(params$facets) == 0) {
    # Add a dummy label
    labels_df <- data_frame0("(all)" = "(all)", .size = 1)
  } else {
    labels_df <- layout[names(params$facets)]
  }
  attr(labels_df, "facet") <- "wrap"
  strips <- render_strips(
    structure(labels_df, type = "rows"),
    structure(labels_df, type = "cols"),
    params$labeller, theme)

  # If user hasn't set aspect ratio, ask the coordinate system if
  # it wants to specify one
  aspect_ratio <- theme$aspect.ratio %||% coord$aspect(ranges[[1]])
  if (is.null(aspect_ratio)) {
    aspect_ratio <- 1
    respect <- FALSE
  } else {
    respect <- TRUE
  }



  # code for arrange the extra panel for linkage data.
  # This is unique feature of ggexon
  # proto-code for only one link panel between two panels.



  if (!is.null(nuc_link_grob) && length(nuc_link_grob) > 0) {

    # target_query
    cli::cli_alert_info("processing nuc_link data by facet_genomics")
    pair_track = strsplit(names(nuc_link_grob), "_")[[1]]
    link_data_names = names(nuc_link_grob)
    nrow = nrow + length(link_data_names)
    panel_pos = c(1, 3, 2)
    panels = append(panels, nuc_link_grob)
  }else{
    cli::cli_alert_info("nuc_link is empty")
  }

  empty_table <- matrix(list(zeroGrob()), nrow = nrow, ncol = ncol)
  panel_table <- empty_table
  panel_table[panel_pos] <- panels


  empties <- apply(panel_table, c(1,2), function(x) is.zero(x[[1]]))

  panel_table <- gtable_matrix("layout", panel_table,
   widths = unit(rep(1, ncol), "null"),
   heights = unit(rep(abs(aspect_ratio), nrow), "null"), respect = respect, clip = coord$clip, z = matrix(1, ncol = ncol, nrow = nrow))
  panel_table$layout$name <- paste0('panel-', rep(seq_len(ncol), nrow), '-', rep(seq_len(nrow), each = ncol))
  panel_table <- gtable_add_col_space(panel_table,
    theme$panel.spacing.x %||% theme$panel.spacing)
  panel_table <- gtable_add_row_space(panel_table,
    theme$panel.spacing.y %||% theme$panel.spacing)

  # Add axes
  axis_mat_x_top <- empty_table
  axis_mat_x_top[panel_pos] <- axes$x$top[x_axis_order]
  axis_mat_x_bottom <- empty_table
  axis_mat_x_bottom[panel_pos] <- axes$x$bottom[x_axis_order]
  axis_mat_y_left <- empty_table
  axis_mat_y_left[panel_pos] <- axes$y$left[y_axis_order]
  axis_mat_y_right <- empty_table
  axis_mat_y_right[panel_pos] <- axes$y$right[y_axis_order]
  if (!(params$free$x || params$draw_axes$x)) {
    axis_mat_x_top[-1,]<- list(zeroGrob())
    axis_mat_x_bottom[-nrow,]<- list(zeroGrob())
  }
  if (!(params$free$y || params$draw_axes$y)) {
    axis_mat_y_left[, -1] <- list(zeroGrob())
    axis_mat_y_right[, -ncol] <- list(zeroGrob())
  }


  # Add back missing axes
  if (any(empties)) {
    row_ind <- row(empties)
    col_ind <- col(empties)
    inside <- (theme$strip.placement %||% "inside") == "inside"
    empty_bottom <- apply(empties, 2, function(x) c(diff(x) == 1, FALSE))
    if (any(empty_bottom)) {
      pos <- which(empty_bottom)
      panel_loc <- data_frame0(
        ROW = row_ind[pos],
        COL = col_ind[pos],
        .size = length(pos)
      )
      panels <- vec_match(panel_loc, layout[, c("ROW", "COL")])
      x_axes <- axes$x$bottom[x_axis_order[panels]]
      if (params$strip.position == "bottom" &&
          !inside &&
          any(!vapply(x_axes, is.zero, logical(1))) &&
          !params$free$x) {
        cli::cli_warn("Suppressing axis rendering when {.code strip.position = \"bottom\"} and {.code strip.placement == \"outside\"}")
      } else {
        axis_mat_x_bottom[pos] <- x_axes
      }
    }
    empty_top <- apply(empties, 2, function(x) c(FALSE, diff(x) == -1))
    if (any(empty_top)) {
      pos <- which(empty_top)
      panel_loc <- data_frame0(
        ROW = row_ind[pos],
        COL = col_ind[pos],
        .size = length(pos)
      )
      panels <- vec_match(panel_loc, layout[, c("ROW", "COL")])
      x_axes <- axes$x$top[x_axis_order[panels]]
      if (params$strip.position == "top" &&
          !inside &&
          any(!vapply(x_axes, is.zero, logical(1))) &&
          !params$free$x) {
        cli::cli_warn("Suppressing axis rendering when {.code strip.position = \"top\"} and {.code strip.placement == \"outside\"}")
      } else {
        axis_mat_x_top[pos] <- x_axes
      }
    }
    empty_right <- t(apply(empties, 1, function(x) c(diff(x) == 1, FALSE)))
    if (any(empty_right)) {
      pos <- which(empty_right)
      panel_loc <- data_frame0(
        ROW = row_ind[pos],
        COL = col_ind[pos],
        .size = length(pos)
      )
      panels <- vec_match(panel_loc, layout[, c("ROW", "COL")])
      y_axes <- axes$y$right[y_axis_order[panels]]
      if (params$strip.position == "right" &&
          !inside &&
          any(!vapply(y_axes, is.zero, logical(1))) &&
          !params$free$y) {
        cli::cli_warn("Suppressing axis rendering when {.code strip.position = \"right\"} and {.code strip.placement == \"outside\"}")
      } else {
        axis_mat_y_right[pos] <- y_axes
      }
    }
    empty_left <- t(apply(empties, 1, function(x) c(FALSE, diff(x) == -1)))
    if (any(empty_left)) {
      pos <- which(empty_left)
      panel_loc <- data_frame0(
        ROW = row_ind[pos],
        COL = col_ind[pos],
        .size = length(pos)
      )
      panels <- vec_match(panel_loc, layout[, c("ROW", "COL")])
      y_axes <- axes$y$left[y_axis_order[panels]]
      if (params$strip.position == "left" &&
          !inside &&
          any(!vapply(y_axes, is.zero, logical(1))) &&
          !params$free$y) {
        cli::cli_warn("Suppressing axis rendering when {.code strip.position = \"left\"} and {.code strip.placement == \"outside\"}")
      } else {
        axis_mat_y_left[pos] <- y_axes
      }
    }
  }
  panel_table <- ggplot2:::weave_axes(
    panel_table,
    axes = list(
      top  = axis_mat_x_top,  bottom = axis_mat_x_bottom,
      left = axis_mat_y_left, right  = axis_mat_y_right
    ),
    empty = empties
  )
  axis_size   <- panel_table$sizes
  panel_table <- panel_table$panels

  strip_padding <- convertUnit(theme$strip.switch.pad.wrap, "cm")
  strip_name <- paste0("strip-", substr(params$strip.position, 1, 1))
  strip_mat <- empty_table
  strip_mat[panel_pos] <- unlist(unname(strips), recursive = FALSE)[[params$strip.position]]
  if (params$strip.position %in% c("top", "bottom")) {
    inside_x <- (theme$strip.placement.x %||% theme$strip.placement %||% "inside") == "inside"
    if (params$strip.position == "top") {
      placement <- if (inside_x) -1 else -2
      strip_pad <- axis_size$top
    } else {
      placement <- if (inside_x) 0 else 1
      strip_pad <- axis_size$bottom
    }
    strip_height <- unit(apply(strip_mat, 1, max_height, value_only = TRUE), "cm")
    panel_table <- ggplot2:::weave_tables_row(panel_table, strip_mat, placement, strip_height, strip_name, 2, coord$clip)
    if (!inside_x) {
      strip_pad[as.numeric(strip_pad) != 0] <- strip_padding
      panel_table <- ggplot2:::weave_tables_row(panel_table, row_shift = placement, row_height = strip_pad)
    }
  } else {
    inside_y <- (theme$strip.placement.y %||% theme$strip.placement %||% "inside") == "inside"
    if (params$strip.position == "left") {
      placement <- if (inside_y) -1 else -2
      strip_pad <- axis_size$left
    } else {
      placement <- if (inside_y) 0 else 1
      strip_pad <- axis_size$right
    }
    strip_pad[as.numeric(strip_pad) != 0] <- strip_padding
    strip_width <- unit(apply(strip_mat, 2, max_width, value_only = TRUE), "cm")
    panel_table <- ggplot2:::weave_tables_col(panel_table, strip_mat, placement, strip_width, strip_name, 2, coord$clip)
    if (!inside_y) {
      strip_pad[as.numeric(strip_pad) != 0] <- strip_padding
      panel_table <- ggplot2:::weave_tables_col(panel_table, col_shift = placement, col_width = strip_pad)
    }
  }
  panel_table
})
