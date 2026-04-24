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
  as_standard_ggplot_built(ggexon_build(plot, ...))
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
  # Legends
  legend_box <- plot@guides$assemble(theme)
  #plot_table <- table_add_legends(plot_table, legend_box, theme)
  # whole plot annotation
  plot_table <- table_add_titles(plot_table, labels, theme)
  plot_table <- table_add_caption(plot_table, labels$caption, theme)
  plot_table <- table_add_tag(plot_table, labels$tag, theme)
  plot_table <- table_add_background(plot_table, theme)
  plot_table <- inject_cross_panel_annotations(plot_table, build)

  # add alt-text as attribute
  attr(plot_table, "alt-label") <- labels$alt

  plot_table
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
