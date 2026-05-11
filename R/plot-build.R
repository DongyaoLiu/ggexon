#' Build a ggexon plot
#'
#' `ggexon_build()` is ggexon's plot-build generic. It mirrors ggplot2's build
#' pipeline while returning ggexon-specific built objects internally and plain
#' ggplot2 built objects through `ggplot_build.ggexon()`.
#'
#' @param plot A ggexon plot object.
#' @param ... Passed through to methods.
#' @export
ggexon_build <- function(plot, ...) {
  # TODO: Swap to S7 generic once S7/#543 is resolved
  env <- ggplot2:::try_prop(plot, "plot_env")
  if (!is.null(env)) {
    attach_plot_env(env)
  }
  UseMethod("ggexon_build")
}

S7::method(ggexon_build, class_ggexon_built) <- function(plot, ...) {
  plot # This is a no-op
}

as_standard_ggplot_built <- function(build) {
  ggplot2::class_ggplot_built(
    data = build@data,
    layout = build@layout,
    plot = build@plot
  )
}

apply_panel_xlim_to_trained_scales <- function(layout) {
  layout_df <- layout$layout %||% NULL
  panel_scales_x <- layout$panel_scales_x %||% NULL
  if (!is.data.frame(layout_df) || is.null(panel_scales_x) || length(panel_scales_x) == 0L) {
    return(layout)
  }
  required_cols <- c("panel_type", "SCALE_X", "xlim_min", "xlim_max")
  if (!all(required_cols %in% names(layout_df))) {
    return(layout)
  }

  annotation_rows <- layout_df$panel_type == "annotation" &
    !is.na(layout_df$xlim_min) &
    !is.na(layout_df$xlim_max)
  if (!any(annotation_rows)) {
    return(layout)
  }

  for (i in which(annotation_rows)) {
    scale_id <- as.integer(layout_df$SCALE_X[[i]])
    if (is.na(scale_id) || scale_id < 1L || scale_id > length(panel_scales_x)) {
      next
    }
    panel_scales_x[[scale_id]]$range$range <- c(
      as.numeric(layout_df$xlim_min[[i]]),
      as.numeric(layout_df$xlim_max[[i]])
    )
  }

  layout$panel_scales_x <- panel_scales_x
  layout
}

#' @export
ggplot_build.ggexon <- function(plot, ...) {
  build <- ggexon_build(plot, ...)
  if ((inherits(plot@facet, "FacetGenomicTree") && !is.null(plot@genomic_tree)) ||
      identical(ggexon_genomic_x_guide_type(plot@genomic_x_scale %||% list(guide = list(type = "genomic"))), "piecewise")) {
    return(build)
  }
  as_standard_ggplot_built(build)
}

#' @export
ggplot_gtable.ggexon_built <- function(data) {
  ggexon_gtable(data)
}


build_ggexon <- S7::method(ggexon_build, class_ggexon) <- function(plot, ...) {
    plot <- plot_clone(plot)
    if (length(plot@layers) == 0) {
      plot <- plot + geom_blank()
    }

    layers <- plot@layers
    data <- rep(list(NULL), length(layers))

    syn_plot_context <- collect_syn_plot_context(layers, plot@data)
    if (!is.null(syn_plot_context)) {
      for (i in seq_along(layers)) {
        layers[[i]]$syn_plot_context <- syn_plot_context
      }
    }

    scales <- plot@scales


    # Allow all layers to make any final adjustments based
    # on raw input data and plot info
    data <- by_layer(function(l, d) l$layer_data(plot@data), layers, data, "computing layer data")
    data <- by_layer(function(l, d) l$setup_layer(d, plot), layers, data, "setting up layer")

    # Initialise panels, add extra data for margins & missing faceting
    # variables, and add on a PANEL variable to data
    layout <- create_layout2(plot@facet, plot@coordinates, plot@layout)
    layout$genomic_tree <- plot@genomic_tree
    data <- layout$setup(data, plot@data, plot@plot_env)

    # add aesthetics mapping to preserve link-anchor metadata. this is specialized for ggexon

    lapply(seq_along(layers), function(i) {
      # only consider link layers
      if (identical(layers[[i]]$geom, GeomNucLink)){
        mapping_names <- names(layers[[i]]$computed_mapping)
        available_metadata <- intersect(
          c("target_anchor_y", "query_anchor_y", "t_panel", "q_panel"),
          names(data[[i]])
        )
        missing_mapping_names <- setdiff(
          available_metadata,
          mapping_names
        )
        if (length(missing_mapping_names) > 0L) {
          outside_mapping = unlist(layers[[i]]$computed_mapping)
          inside_mapping = unlist(ggplot2::aes(
            target_anchor_y = target_anchor_y,
            query_anchor_y = query_anchor_y,
            t_panel = t_panel,
            q_panel = q_panel
          )[missing_mapping_names])
          layers[[i]]$computed_mapping = ggplot2::class_mapping(c(outside_mapping, inside_mapping), env = parent.frame())
        }
      }
    })
    # Compute aesthetics to produce data with generalised variable names.
    data <- by_layer(function(l, d) l$compute_aesthetics(d, plot), layers, data, "computing aesthetics")

    # TODO: future labels presentation should do with this function.
    plot@labels <- ggplot2:::setup_plot_labels(plot, layers, data)
    data <- .ignore_data(data)

    # Transform all data table in the list 'data', globally not layer specific. I should learn this project ggnewscale
    data <- lapply(data, scales$transform_df)

    # Map and train positions so that statistics have access to ranges
    # and all positions are numeric
    scale_x <- function() scales$get_scales("x")
    scale_y <- function() scales$get_scales("y")

    # init ScaleContinuousPosition for each panel and train position and stored
    # in the range property of ScaleContinuousPosition
    layout$train_position(data, scale_x(), scale_y())
    layout <- apply_panel_xlim_to_trained_scales(layout)
    data <- layout$map_position(data)
    data <- .expose_data(data)



    # Apply and map statistics
    data <- by_layer(function(l, d) l$compute_statistic(d, layout), layers, data, "computing stat")
    data <- by_layer(function(l, d) l$map_statistic(d, plot), layers, data, "mapping stat to aesthetics")

    # Make sure missing (but required) aesthetics are added
    plot@scales$add_missing(c("x", "y"), plot@plot_env)

    # Reparameterise geoms from (e.g.) y and width to ymin and ymax
    data <- by_layer(function(l, d) l$compute_geom_1(d), layers, data, "setting up geom")
    if (!is.null(plot@genomic_x_scale)) {
      genomic_x_scaled <- apply_ggexon_genomic_x_scale(data, plot@genomic_x_scale, layout)
      data <- genomic_x_scaled$data
      layout$genomic_x_transforms <- genomic_x_scaled$transforms
      layout$genomic_x_axis_data <- genomic_x_scaled$axis_data
    }
    if (!is.null(plot@strip_scale)) {
      strip_scaled <- apply_strip_scale(data, layers, plot@strip_scale, layout, plot)
      data <- strip_scaled$data
      layout <- strip_scaled$layout
    }

    # Apply position adjustments
    data <- by_layer(function(l, d) l$compute_position(d, layout), layers, data, "computing position")

    # Reset position scales, then re-train and map.  This ensures that facets
    # have control over the range of a plot: is it generated from what is
    # displayed, or does it include the range of underlying data
    data <- .ignore_data(data)
    layout$reset_scales()
    layout$train_position(data, scale_x(), scale_y())
    layout <- apply_panel_xlim_to_trained_scales(layout)
    layout$setup_panel_params()
    layout <- apply_ggexon_genomic_x_axis(layout, plot@genomic_x_scale)
    data <- layout$map_position(data)

    # Hand off position guides to layout
    layout$setup_panel_guides(plot@guides, plot@layers)

    # ggplot2 4.0.0 above, change the code for theme rendering
    plot@theme <- ggplot2:::plot_theme(plot)

    # Train and map non-position scales and guides
    npscales <- scales$non_position_scales()
    if (npscales$n() > 0) {
      npscales$set_palettes(plot@theme)
      lapply(data, npscales$train_df)
      plot@guides <- plot@guides$build(npscales, plot@layers, plot@labels, data, plot@theme)
      data <- lapply(data, npscales$map_df)
    } else {
      # Only keep custom guides if there are no non-position scales
      plot@guides <- plot@guides$get_custom()
    }
    data <- .expose_data(data)

    # Fill in defaults etc.
    data <- by_layer(function(l, d) l$compute_geom_2(d, theme = plot@theme), layers, data, "setting up geom aesthetics")

    # Let layer stat have a final say before rendering
    # finish_stat no longer need theme parameter
    data <- by_layer(function(l, d) l$finish_statistics(d), layers, data, "finishing layer stat")

    # Let Layout modify data before rendering
    data <- layout$finish_data(data)

    # Consolidate alt-text
    plot@labels$alt <- get_alt_text(plot)

    build <- class_ggexon_built(data = data, layout = layout, plot = plot)
    class(build) = union(c("ggexon_built", "ggplot2::ggplot_built"), class(build))
    build
}

#' @export

ggexon_gtable <- function(data) {
  # Attaching the plot env to be fetched by deprecations etc.
  ggplot2:::attach_plot_env(data@plot@plot_env)
  #print(data@plot$plot_env)
  UseMethod('ggexon_gtable')
}

# below code is from the plot-render.
# It inserts a blank panel for drawing link data; this may not be the final
# approach now that ggplot2 4.0.0 encapsulates plot annotation in separate
# functions.
S7::method(ggexon_gtable, class_ggexon_built) <- function(data) {
  build <- data
  plot <- build@plot
  layout <- build@layout
  data <- build@data
  theme <- plot@theme
  labels <- plot@labels

  geom_grobs <- by_layer(function(l, d) l$draw_geom(d, layout), plot@layers, data, "converting geom to grob")

  plot_table <- layout$render(geom_grobs, data, theme, labels)
  plot_table <- inject_genomic_piecewise_axis(plot_table, build)
  # Legends
  legend_box <- plot@guides$assemble(theme)
  #plot_table <- table_add_legends(plot_table, legend_box, theme)
  # whole plot annotation
  plot_table <- table_add_titles(plot_table, labels, theme)
  plot_table <- table_add_caption(plot_table, labels$caption, theme)
  plot_table <- table_add_tag(plot_table, labels$tag, theme)
  plot_table <- inject_genomictree_panel(plot_table, build)
  plot_table <- table_add_background(plot_table, theme)
  plot_table <- inject_cross_panel_annotations(plot_table, build)

  # add alt-text as attribute
  attr(plot_table, "alt-label") <- labels$alt

  plot_table
}

inject_genomic_piecewise_axis <- function(table, build) {
  scale_spec <- build@plot@genomic_x_scale
  axis_data <- build@layout$genomic_x_axis_data %||% NULL
  if (is.null(scale_spec) ||
      !identical(ggexon_genomic_x_guide_type(scale_spec), "piecewise") ||
      is.null(axis_data) ||
      nrow(axis_data) == 0L) {
    return(table)
  }

  layout_df <- as.data.frame(build@layout$layout)
  if (!all(c("PANEL", "COL", "ROW") %in% names(layout_df))) {
    return(table)
  }

  text_gp <- ggexon_element_text_gpar(
    calc_element("axis.text.x", build@plot@theme),
    default_size = 7
  )
  line_gp <- ggexon_element_line_gpar(
    calc_element("axis.line.x", build@plot@theme),
    default_colour = "grey40",
    default_linewidth = 0.25
  )
  tick_gp <- ggexon_element_line_gpar(
    calc_element("axis.ticks.x", build@plot@theme),
    default_colour = "grey45",
    default_linewidth = 0.25
  )
  if (is.null(text_gp) && is.null(line_gp) && is.null(tick_gp)) {
    return(table)
  }

  max_groups <- max(axis_data$axis_group_count, na.rm = TRUE)
  axis_height <- grid::unit(max(18, 16 * max_groups + 4), "pt")
  for (i in seq_len(nrow(layout_df))) {
    panel_id <- as.integer(layout_df$PANEL[[i]])
    panel_axis_data <- axis_data[axis_data$PANEL == panel_id, , drop = FALSE]
    if (nrow(panel_axis_data) == 0L || panel_id > length(build@layout$panel_params)) {
      next
    }

    axis_name <- paste0("axis-b-", layout_df$COL[[i]], "-", layout_df$ROW[[i]])
    axis_idx <- which(table$layout$name == axis_name)
    if (length(axis_idx) != 1L) {
      panel_name <- paste0("panel-", layout_df$COL[[i]], "-", layout_df$ROW[[i]])
      panel_idx <- which(table$layout$name == panel_name)
      if (length(panel_idx) != 1L && nrow(layout_df) == 1L) {
        panel_idx <- which(table$layout$name == "panel")
      }
      if (length(panel_idx) == 1L) {
        axis_idx <- which(
          grepl("^axis-b", table$layout$name) &
            table$layout$l == table$layout$l[[panel_idx]] &
            table$layout$r == table$layout$r[[panel_idx]]
        )
      }
    }
    if (length(axis_idx) != 1L && nrow(layout_df) == 1L) {
      axis_idx <- which(table$layout$name == "axis-b")
    }
    if (length(axis_idx) != 1L) {
      next
    }

    x_range <- build@layout$panel_params[[panel_id]]$x$continuous_range %||%
      build@layout$panel_params[[panel_id]]$x.range
    table$grobs[[axis_idx]] <- ggexon_genomic_piecewise_axis_grob(
      data = panel_axis_data,
      x_range = x_range,
      text_gp = text_gp,
      line_gp = line_gp,
      tick_gp = tick_gp
    )
    table$heights[[table$layout$t[[axis_idx]]]] <- grid::unit.pmax(
      table$heights[[table$layout$t[[axis_idx]]]],
      axis_height
    )
  }

  table
}

ggexon_genomic_piecewise_axis_grob <- function(data,
                                               x_range,
                                               text_gp = NULL,
                                               line_gp = NULL,
                                               tick_gp = NULL,
                                               name = NULL) {
  grid::grob(
    data = data,
    x_range = x_range,
    text_gp = text_gp,
    line_gp = line_gp,
    tick_gp = tick_gp,
    name = name %||% "ggexon-genomic-piecewise-axis",
    cl = "ggexonGenomicPiecewiseAxisGrob"
  )
}

#' @export
drawDetails.ggexonGenomicPiecewiseAxisGrob <- function(x, recording = TRUE) {
  if (is.null(x$data) || nrow(x$data) == 0L || length(x$x_range) < 2L) {
    return(invisible())
  }

  x_range <- range(as.numeric(x$x_range), finite = TRUE)
  if (!all(is.finite(x_range)) || diff(x_range) <= 0) {
    return(invisible())
  }

  rel_x <- function(value) {
    value <- pmin(pmax(as.numeric(value), x_range[[1L]]), x_range[[2L]])
    scales::rescale(value, from = x_range, to = c(0, 1))
  }

  group_count <- max(as.integer(x$data$axis_group_count), na.rm = TRUE)
  if (!is.finite(group_count) || group_count < 1L) {
    group_count <- 1L
  }

  ordered_data <- x$data[order(x$data$axis_group_index, x$data$region_type), , drop = FALSE]
  for (i in seq_len(nrow(ordered_data))) {
    row <- ordered_data[i, , drop = FALSE]
    group_index <- as.integer(row$axis_group_index[[1L]])
    group_top <- 1 - (group_index - 1L) / group_count
    group_bottom <- 1 - group_index / group_count
    group_height <- group_top - group_bottom
    region_y <- if (identical(row$region_type[[1L]], "exon")) {
      group_top - group_height * 0.32
    } else {
      group_top - group_height * 0.68
    }

    x0 <- rel_x(row$plot_start[[1L]])
    x1 <- rel_x(row$plot_end[[1L]])
    if (!is.finite(x0) || !is.finite(x1) || abs(x1 - x0) <= 0) {
      next
    }
    xmid <- (x0 + x1) / 2
    segment_gp <- x$line_gp %||% grid::gpar(col = "grey40", lwd = 0.25)
    if (identical(row$region_type[[1L]], "intron")) {
      segment_gp$lty <- 2
      segment_gp$col <- segment_gp$col %||% "grey55"
    }
    grid::grid.segments(
      x0 = grid::unit(x0, "npc"),
      x1 = grid::unit(x1, "npc"),
      y0 = grid::unit(region_y, "npc"),
      y1 = grid::unit(region_y, "npc"),
      gp = segment_gp
    )

    tick_gp <- x$tick_gp %||% segment_gp
    tick_half_height <- min(group_height * 0.12, 0.07)
    grid::grid.segments(
      x0 = grid::unit(c(x0, x1), "npc"),
      x1 = grid::unit(c(x0, x1), "npc"),
      y0 = grid::unit(c(region_y - tick_half_height, region_y - tick_half_height), "npc"),
      y1 = grid::unit(c(region_y + tick_half_height, region_y + tick_half_height), "npc"),
      gp = tick_gp
    )

    if (!is.null(x$text_gp) && !is.na(row$label[[1L]]) && nzchar(row$label[[1L]])) {
      label_y <- if (identical(row$region_type[[1L]], "exon")) {
        min(region_y + group_height * 0.18, 0.95)
      } else {
        max(region_y - group_height * 0.18, 0.05)
      }
      label_x <- pmin(pmax(xmid, 0.08), 0.92)
      grid::grid.text(
        row$label[[1L]],
        x = grid::unit(label_x, "npc"),
        y = grid::unit(label_y, "npc"),
        just = "center",
        gp = x$text_gp
      )
    }
  }

  invisible()
}

inject_genomictree_panel <- function(table, build) {
  plot <- build@plot
  tree_spec <- plot@genomic_tree
  if (is.null(tree_spec) || !inherits(plot@facet, "FacetGenomicTree")) {
    return(table)
  }

  panel_idx <- grep("^panel", table$layout$name)
  if (length(panel_idx) == 0L) {
    return(table)
  }

  table <- strip_genomictree_panel_strips(table)
  panel_idx <- grep("^panel", table$layout$name)
  panel_rows <- table$layout[panel_idx, , drop = FALSE]
  layout_df <- as.data.frame(build@layout$layout)

  tree_data <- .ggtree_rectangular_plot_data(
    tree = tree_spec$tree,
    tree_plot = tree_spec$tree_plot,
    layout = tree_spec$layout
  )
  tree_segments <- .ggtree_rectangular_segments_from_data(tree_data, track = "Tree")
  panel_tree_y <- genomictree_panel_tree_y(layout_df, tree_data)
  panel_row_map <- genomictree_panel_row_map(table, layout_df)

  keep <- is.finite(panel_tree_y) & !is.na(panel_row_map)
  if (!any(keep)) {
    return(table)
  }
  panel_tree_y <- panel_tree_y[keep]
  panel_row_map <- panel_row_map[keep]
  label_values <- as.character(layout_df$track)[keep]

  full_t <- min(c(panel_rows$t, grep_table_rows(table, "^axis-[tb]")))
  full_b <- max(c(panel_rows$b, grep_table_rows(table, "^axis-[tb]")))
  panel_l <- min(panel_rows$l)

  label_position <- plot@facet$params$label_position %||% "left"
  label_col <- NA_integer_
  if (!identical(label_position, "none")) {
    label_element <- calc_element("strip.text.y", plot@theme)
    label_gp <- ggexon_element_text_gpar(
      label_element,
      default_size = 9
    )
    label_width <- plot@facet$params$label_width %||% grid::unit(0.7, "in")
    if (identical(label_position, "left")) {
      table <- gtable::gtable_add_cols(table, label_width, pos = panel_l - 1L)
      label_col <- panel_l
      panel_l <- panel_l + 1L
      panel_rows$l <- panel_rows$l + 1L
    } else {
      panel_r <- max(panel_rows$r)
      table <- gtable::gtable_add_cols(table, label_width, pos = panel_r)
      label_col <- panel_r + 1L
    }

    if (!is.null(label_gp)) {
      for (i in seq_along(label_values)) {
        table <- gtable::gtable_add_grob(
          table,
          grid::textGrob(
            label_values[[i]],
            x = if (identical(label_position, "left")) grid::unit(1, "npc") else grid::unit(0, "npc"),
            y = grid::unit(0.5, "npc"),
            just = if (identical(label_position, "left")) c("right", "center") else c("left", "center"),
            gp = label_gp
          ),
          t = panel_row_map[[i]],
          l = label_col,
          clip = "off",
          name = paste0("genomic-tree-label-", label_values[[i]])
        )
      }
    }
  }

  tree_width <- tree_spec$tree_width %||% grid::unit(1.5, "in")
  if (identical(label_position, "left") && !is.na(label_col)) {
    table <- gtable::gtable_add_cols(table, tree_width, pos = label_col - 1L)
    tree_col <- label_col
  } else {
    table <- gtable::gtable_add_cols(table, tree_width, pos = panel_l - 1L)
    tree_col <- panel_l
  }

  tree_grob <- genomictree_segments_grob(
    data = tree_segments,
    heights = table$heights[full_t:full_b],
    panel_rows = panel_row_map - full_t + 1L,
    panel_tree_y = panel_tree_y,
    x_range = .ggtree_alignment_x_limits(
      c(tree_segments$x, tree_segments$xend),
      pad_mult = c(0.03, 0.05)
    ),
    colour = tree_spec$colour %||% "black",
    linewidth = tree_spec$linewidth %||% 0.5,
    show_x_axis = isTRUE(plot@facet$params$show_tree_x_axis),
    axis_gp = ggexon_element_text_gpar(
      calc_element("axis.text.x", plot@theme),
      default_size = 8
    )
  )
  table <- gtable::gtable_add_grob(
    table,
    tree_grob,
    t = full_t,
    l = tree_col,
    b = full_b,
    r = tree_col,
    clip = "off",
    name = "genomic-tree"
  )
  # Apply user-specified track width to panel columns
  track_width <- plot@facet$params$track_width %||% NULL
  if (!is.null(track_width)) {
    panel_cols <- seq.int(min(panel_rows$l), max(panel_rows$r))
    table$widths[panel_cols] <- track_width
  }
  attr(table, "genomic_tree") <- tree_spec
  table
}

strip_genomictree_panel_strips <- function(table) {
  strip_idx <- grep("^strip-[tblr]", table$layout$name)
  if (length(strip_idx) == 0L) {
    return(table)
  }
  for (idx in strip_idx) {
    table$grobs[[idx]] <- zeroGrob()
  }
  horizontal <- grepl("^strip-[tb]", table$layout$name[strip_idx])
  vertical <- grepl("^strip-[lr]", table$layout$name[strip_idx])
  if (any(horizontal)) {
    rows <- unique(unlist(Map(seq, table$layout$t[strip_idx[horizontal]], table$layout$b[strip_idx[horizontal]])))
    table$heights[rows] <- grid::unit(0, "pt")
  }
  if (any(vertical)) {
    cols <- unique(unlist(Map(seq, table$layout$l[strip_idx[vertical]], table$layout$r[strip_idx[vertical]])))
    table$widths[cols] <- grid::unit(0, "pt")
  }
  table
}

grep_table_rows <- function(table, pattern) {
  idx <- grep(pattern, table$layout$name)
  if (length(idx) == 0L) {
    return(integer())
  }
  unique(unlist(Map(seq, table$layout$t[idx], table$layout$b[idx])))
}

genomictree_panel_tree_y <- function(layout_df, tree_data) {
  if ("tree_y" %in% names(layout_df)) {
    return(as.numeric(layout_df$tree_y))
  }
  tip_data <- .ggtree_rectangular_tip_data_from_data(tree_data)
  track <- if ("track" %in% names(layout_df)) as.character(layout_df$track) else rep(NA_character_, nrow(layout_df))
  tip_data$y[match(track, tip_data$label)]
}

genomictree_panel_row_map <- function(table, layout_df) {
  out <- rep(NA_integer_, nrow(layout_df))
  for (i in seq_len(nrow(layout_df))) {
    panel_col <- layout_df$COL[[i]] %||% 1L
    panel_row <- layout_df$ROW[[i]] %||% i
    idx <- which(table$layout$name == paste0("panel-", panel_col, "-", panel_row))
    if (length(idx) != 1L) {
      idx <- grep("^panel", table$layout$name)[i]
    }
    if (length(idx) == 1L && !is.na(idx)) {
      out[[i]] <- table$layout$t[[idx]]
    }
  }
  out
}

genomictree_segments_grob <- function(data,
                                      heights,
                                      panel_rows,
                                      panel_tree_y,
                                      x_range,
                                      colour = "black",
                                      linewidth = 0.5,
                                      show_x_axis = TRUE,
                                      axis_gp = NULL,
                                      name = NULL) {
  grid::grob(
    data = data,
    heights = heights,
    panel_rows = panel_rows,
    panel_tree_y = panel_tree_y,
    x_range = x_range,
    colour = colour,
    linewidth = linewidth,
    show_x_axis = show_x_axis,
    axis_gp = axis_gp,
    name = name %||% "genomic-tree",
    cl = "genomicTreeSegmentsGrob"
  )
}

#' @export
drawDetails.genomicTreeSegmentsGrob <- function(x, recording = TRUE) {
  if (nrow(x$data) == 0L || length(x$panel_rows) == 0L) {
    return(invisible())
  }
  total_height_cm <- grid::convertHeight(grid::unit(1, "npc"), "cm", TRUE)
  total_width_cm <- grid::convertWidth(grid::unit(1, "npc"), "cm", TRUE)
  resolved_heights_cm <- resolve_genomictree_heights_cm(x$heights, total_height_cm)

  panel_y_cm <- vapply(x$panel_rows, function(row) {
    panel_relative_y_cm_resolved(resolved_heights_cm, row, 0.5, total_height_cm)
  }, numeric(1))
  ordered <- order(x$panel_tree_y)
  tree_y <- x$panel_tree_y[ordered]
  panel_y_cm <- panel_y_cm[ordered]

  map_y <- function(y) {
    stats::approx(tree_y, panel_y_cm, xout = y, rule = 2, ties = mean)$y
  }
  map_x <- function(x_value) {
    scales::rescale(x_value, from = x$x_range, to = c(0, total_width_cm))
  }

  for (i in seq_len(nrow(x$data))) {
    row <- x$data[i, , drop = FALSE]
    grid::grid.segments(
      x0 = grid::unit(map_x(row$x[[1L]]), "cm"),
      x1 = grid::unit(map_x(row$xend[[1L]]), "cm"),
      y0 = grid::unit(map_y(row$y[[1L]]), "cm"),
      y1 = grid::unit(map_y(row$yend[[1L]]), "cm"),
      gp = grid::gpar(col = x$colour, lwd = x$linewidth),
      default.units = "cm"
    )
  }

  if (isTRUE(x$show_x_axis) && !is.null(x$axis_gp)) {
    breaks <- pretty(x$x_range, n = 4)
    breaks <- breaks[breaks >= min(x$x_range) & breaks <= max(x$x_range)]
    if (length(breaks) > 0L) {
      grid::grid.xaxis(
        at = scales::rescale(breaks, from = x$x_range, to = c(0, 1)),
        label = breaks,
        gp = x$axis_gp
      )
    }
  }
  invisible()
}

resolve_genomictree_heights_cm <- function(heights, total_height_cm) {
  unit_type <- grid::unitType(heights)
  null_rows <- identical(unit_type, "null") | unit_type == "null"
  out <- numeric(length(heights))

  if (any(!null_rows)) {
    out[!null_rows] <- grid::convertHeight(heights[!null_rows], "cm", TRUE)
  }

  if (any(null_rows)) {
    fixed_height <- sum(out[!null_rows], na.rm = TRUE)
    null_weight <- as.numeric(heights[null_rows])
    null_weight[!is.finite(null_weight) | null_weight < 0] <- 0
    total_weight <- sum(null_weight)
    remaining <- max(total_height_cm - fixed_height, 0)
    if (total_weight > 0) {
      out[null_rows] <- remaining * null_weight / total_weight
    }
  }

  out
}

panel_relative_y_cm_resolved <- function(heights_cm, panel_row, rel_y, total_height_cm) {
  offset_top_cm <- if (panel_row > 1L) {
    sum(heights_cm[seq_len(panel_row - 1L)], na.rm = TRUE)
  } else {
    0
  }
  panel_height_cm <- heights_cm[[panel_row]]
  y_from_top_cm <- offset_top_cm + (1 - rel_y) * panel_height_cm
  total_height_cm - y_from_top_cm
}

#' Generate a ggplot2 plot grob.
#'
#' @param x ggplot2 object
#' @keywords internal
#' @export
ggplotGrob <- function(x) {
  ggplot_gtable(ggplot_build(x))
}

S7::method(as.gtable, class_ggplot) <- function(x, ...) ggplotGrob(x)
S7::method(as.gtable, class_ggplot_built) <- function(x, ...) ggplot_gtable(x)

# Add the legends to the gtable
table_add_legends <- function(table, legends, theme) {

  if (ggplot2:::is_zero(legends)) {
    legends <- rep(list(zeroGrob()), 5)
    names(legends) <- c(ggplot2:::.trbl, "inside")
  }

  # Extract sizes
  widths <- heights <- set_names(
    rep(list(unit(0, "cm")), length(legends)),
    names(legends)
  )

  empty <- vapply(legends, ggplot2:::is_zero, logical(1))
  widths[!empty]  <- lapply(legends[!empty], gtable_width)
  heights[!empty] <- lapply(legends[!empty], gtable_height)
  spacing <- calc_element("legend.box.spacing", theme) %||% unit(0.2, "cm")

  # If legend is missing, set spacing to zero for that legend
  zero    <- unit(0, "pt")
  spacing <- lapply(empty, function(is_empty) if (is_empty) zero else spacing)

  location <- switch(
    theme$legend.location %||% "panel",
    "plot" = plot_extent,
    find_panel
  )

  place <- location(table)

  # Add right legend
  table <- gtable_add_cols(table, spacing$right, pos = -1)
  table <- gtable_add_cols(table, widths$right,  pos = -1)
  table <- gtable_add_grob(
    table, legends$right, clip = "off",
    t = place$t, b = place$b, l = -1, r = -1,
    name = "guide-box-right"
  )

  # Add left legend
  table <- gtable_add_cols(table, spacing$left, pos = 0)
  table <- gtable_add_cols(table, widths$left,  pos = 0)
  table <- gtable_add_grob(
    table, legends$left, clip = "off",
    t = place$t, b = place$b, l = 1, r = 1,
    name = "guide-box-left"
  )

  place <- location(table)

  # Add bottom legend
  table <- gtable_add_rows(table, spacing$bottom, pos = -1)
  table <- gtable_add_rows(table, heights$bottom, pos = -1)
  table <- gtable_add_grob(
    table, legends$bottom, clip = "off",
    t = -1, b = -1, l = place$l, r = place$r,
    name = "guide-box-bottom"
  )

  # Add top legend
  table <- gtable_add_rows(table, spacing$top, pos = 0)
  table <- gtable_add_rows(table, heights$top, pos = 0)
  table <- gtable_add_grob(
    table, legends$top, clip = "off",
    t = 1, b = 1, l = place$l, r = place$r,
    name = "guide-box-top"
  )

  # Add manual legend
  place <- find_panel(table)
  table <- gtable_add_grob(
    table, legends$inside, clip = "off",
    t = place$t, b = place$b, l = place$l, r = place$r,
    name = "guide-box-inside"
  )

  table
}

table_add_titles <- function(table, labels, theme) {

  # Title
  title <- element_render(
    theme, "plot.title", labels$title,
    margin_y = TRUE, margin_x = TRUE
  )
  title_height <- grobHeight(title)

  # Subtitle
  subtitle <- element_render(
    theme, "plot.subtitle", labels$subtitle,
    margin_y = TRUE, margin_x = TRUE
  )
  subtitle_height <- grobHeight(subtitle)

  # positioning of title and subtitle is governed by plot.title.position
  #   "panel" means align to the panel(s)
  #   "plot" means align to the entire plot (except margins and tag)
  title_pos <- arg_match0(
    theme$plot.title.position %||% "panel",
    c("panel", "plot"),
    arg_nm = "plot.title.position",
    error_call = expr(theme())
  )

  panels <- table$layout[grepl("^panel", table$layout$name), , drop = FALSE]
  if (title_pos == "panel") {
    l <- min(panels$l)
    r <- max(panels$r)
  } else {
    l <- 1
    r <- ncol(table)
  }

  table <- gtable_add_rows(table, subtitle_height, pos = 0)
  table <- gtable_add_grob(table, subtitle, name = "subtitle",
                           t = 1, b = 1, l = l, r = r, clip = "off")

  table <- gtable_add_rows(table, title_height, pos = 0)
  table <- gtable_add_grob(table, title, name = "title",
                           t = 1, b = 1, l = l, r = r, clip = "off")

  table
}

table_add_caption <- function(table, label, theme) {

  caption <- element_render(
    theme, "plot.caption", label,
    margin_y = TRUE, margin_x = TRUE
  )
  caption_height <- grobHeight(caption)

  # positioning of title and subtitle is governed by plot.title.position
  # positioning of caption is governed by plot.caption.position
  #   "panel" means align to the panel(s)
  #   "plot" means align to the entire plot (except margins and tag)
  position <- arg_match0(
    theme$plot.caption.position %||% "panel",
    values = c("panel", "plot"),
    arg_nm = "plot.caption.position",
    error_call = expr(theme())
  )

  pans <- table$layout[grepl("^panel", table$layout$name), , drop = FALSE]
  if (position == "panel") {
    l <- min(pans$l)
    r <- max(pans$r)
  } else {
    l <- 1
    r <- ncol(table)
  }

  table <- gtable_add_rows(table, caption_height, pos = -1)
  table <- gtable_add_grob(table, caption, name = "caption",
                           t = -1, b = -1, l = l, r = r, clip = "off")
  table
}

# Add the tag element to the gtable
table_add_tag <- function(table, label, theme) {
  # Initialise the tag margins
  table <- gtable_add_padding(table, unit(0, "pt"))

  # Early exit when label is absent or element is blank
  if (length(label) < 1) {
    return(table)
  }
  element <- calc_element("plot.tag", theme)
  if (is_theme_element(element, "blank")) {
    return(table)
  }

  # Resolve position
  position <- calc_element("plot.tag.position", theme) %||% "topleft"
  location <- calc_element("plot.tag.location", theme) %||%
    (if (is.numeric(position)) "plot" else "margin")

  if (is.numeric(position)) {
    if (location == "margin") {
      cli::cli_abort(paste0(
        "A {.cls numeric} {.arg plot.tag.position} cannot be used with ",
        "`{.val margin}` as {.arg plot.tag.location}."
      ),
      call = expr(theme()))
    }
    check_length(
      position, 2L, call = expr(theme()),
      arg = I("A {.cls numeric} {.arg plot.tag.position}")
    )
    top <- left <- right <- bottom <- FALSE
  } else {
    # Break position into top/left/right/bottom
    position <- arg_match0(
      position[1],
      c("topleft", "top", "topright", "left",
        "right", "bottomleft", "bottom", "bottomright"),
      arg_nm = "plot.tag.position",
      error_call = expr(theme())
    )
    top    <- position %in% c("topleft",    "top",    "topright")
    left   <- position %in% c("topleft",    "left",   "bottomleft")
    right  <- position %in% c("topright",   "right",  "bottomright")
    bottom <- position %in% c("bottomleft", "bottom", "bottomright")
  }

  # Resolve tag and sizes
  tag <- element_grob(element, label = label, margin_y = TRUE, margin_x = TRUE)
  height <- grobHeight(tag)
  width  <- grobWidth(tag)

  if (location %in% c("plot", "panel")) {
    if (!is.numeric(position)) {
      hjust <- try_prop(element, "hjust", default = 0.5)
      if (right || left) {
        x <- (1 - hjust) * width
        if (right) {
          x <- unit(1, "npc") - x
        }
      } else {
        x <- unit(hjust, "npc")
      }
      if (top || bottom) {
        vjust <- try_prop(element, "vjust", default = 0.5)
        y <- (1 - vjust) * height
        if (top) {
          y <- unit(1, "npc") - y
        }
      } else {
        y <- unit(vjust, "npc")
      }
    } else {
      x <- unit(position[1], "npc")
      y <- unit(position[2], "npc")
    }
    # Re-render with manual positions
    tag <- element_grob(
      element, x = x, y = y, label = label,
      margin_y = TRUE, margin_x = TRUE
    )
    if (location == "plot") {
      table <- gtable_add_grob(
        table, tag, name = "tag", clip = "off",
        t = 1, b = nrow(table), l = 1, r = ncol(table)
      )
      return(table)
    }
  }

  if (location == "panel") {
    place <- find_panel(table)
  } else {
    n_col <- ncol(table)
    n_row <- nrow(table)
    # Actually fill margin with relevant units
    if (top)    table$heights <- unit.c(height, table$heights[-1])
    if (left)   table$widths  <- unit.c(width,  table$widths[-1])
    if (right)  table$widths  <- unit.c(table$widths[-n_col],  width)
    if (bottom) table$heights <- unit.c(table$heights[-n_row], height)
    place <- data_frame0(t = 1L, r = n_col, b = n_row, l = 1L)
  }

  # Shrink placement to position
  if (top)    place$b <- place$t
  if (left)   place$r <- place$l
  if (right)  place$l <- place$r
  if (bottom) place$t <- place$b

  gtable_add_grob(
    table, tag, name = "tag", clip = "off",
    t = place$t, l = place$l, b = place$b, r = place$r
  )
}

table_add_background <- function(table, theme) {
  # Margins
  margin <- calc_element("plot.margin", theme) %||% margin()
  table  <- gtable_add_padding(table, margin)

  background <- calc_element("plot.background", theme)
  if (is_theme_element(background)) {
    table <- gtable_add_grob(
      table, element_grob(background),
      t = 1, l = 1, b = -1, r = -1,
      name = "background", z = -Inf
    )
  }

  table
}

plot_extent <- function(table) {
  layout <- table$layout
  data_frame0(
    t = min(layout[["t"]]),
    r = max(layout[["r"]]),
    b = max(layout[["b"]]),
    l = min(layout[["l"]]),
    .size = 1L
  )
}

#' Extract a built grob for a layer
#'
#' `layer_grob` is an alias of `get_layer_grob()`.
#'
#' @param plot A built ggplot or ggexon object.
#' @param i Layer index.
#' @export
layer_grob <- get_layer_grob

# Apply function to layer and matching data
by_layer <- function(f, layers, data, step = NULL) {
  ordinal <- label_ordinal()
  out <- vector("list", length(data))
  try_fetch(
    for (i in seq_along(data)) {
      out[[i]] <- f(l = layers[[i]], d = data[[i]])
    },
    error = function(cnd) {
      cli::cli_abort(c(
        "Problem while {step}.",
        "i" = "Error occurred in the {ordinal(i)} layer."),
        call = layers[[i]]$constructor,
        parent = cnd
      )
    }
  )
  out
}
