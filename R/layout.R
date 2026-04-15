create_layout2 <- function(facet, coord, layout = NULL) {
   layout <- layout %||% Layout2
   ggplot2:::check_inherits(layout, "Layout2")
   ggproto(NULL, layout, facet = facet, coord = coord)
}

#' @export
Layout2 <- ggproto("Layout2", Layout,
  setup = function(self, data, plot_data = data_frame0(), plot_env = emptyenv()) {
    plot_data_raw <- plot_data
    layout_override <- NULL
    if (methods::is(plot_data, "SynSpecies") || methods::is(plot_data, "SynIndividual")) {
      plot_data <- data_frame0()
    }
    for (layer_df in data) {
      if (is.data.frame(layer_df)) {
        layout_override <- attr(layer_df, "syn_layout_override", exact = TRUE)
        if (!is.null(layout_override)) {
          break
        }
      }
    }
    data <- c(list(plot_data), data)

    # Setup facets
    self$facet_params <- self$facet$setup_params(data, self$facet$params)
    self$facet_params$plot_data <- plot_data_raw
    self$facet_params$layout_override <- layout_override
    self$facet_params$has_link_layers <- .detect_link_layers(data)

    # detect any link data inside the data list
    # self$facet_params <- self$facet$compute_layer_type(data, self$facet_params)

    self$facet_params$plot_env <- plot_env
    data <- self$facet$setup_data(data, self$facet_params)

    # Setup coords
    self$coord_params <- self$coord$setup_params(data)
    data <- self$coord$setup_data(data, self$coord_params)

    # Generate panel layout
    # PANEL ROW COL "facet variable" SCALE_X SCALE_Y
    self$layout <- self$facet$compute_layout(data, self$facet_params)

    # Rearrange the panel if detect the link data.
    if ("track" %in% colnames(self$layout) &&
        TRUE %in% stringr::str_detect(self$layout$track, "link")) {
      if (!"panel_type" %in% colnames(self$layout)) {
        self$layout <- self$facet$compute_alignment_layout(data, self$layout)
      }

      # final step to assign the y of target alignment and query alignment
      data <- self$facet$map_link_direction(data, self$layout)
    }

    # PANEL ROW COL "facet variable" SCALE_X SCALE_Y COORD
    self$layout <- self$coord$setup_layout(self$layout, self$coord_params)

    ggplot2:::check_layout(self$layout)


    # Add panel coordinates to the data for each layer
    mapped_data <- lapply(data[-1], self$facet$map_data,
      layout = self$layout,
      params = self$facet_params
    )

    mapped_data <- add_layout_panel_metadata(mapped_data, self$layout)
    mapped_data

  },

  map_position = function(self, data) {
    layout <- self$layout

    lapply(data, function(layer_data) {
      match_id <- NULL

      # Loop through each variable, mapping across each scale, then joining
      # back together
      x_vars <- intersect(self$panel_scales_x[[1]]$aesthetics, names(layer_data))
      if (length(x_vars) > 0) {
        match_id <- match(layer_data$PANEL, layout$PANEL)
        names(x_vars) <- x_vars
        SCALE_X <- layout$SCALE_X[match_id]
        new_x <- ggplot2:::scale_apply(layer_data, x_vars, "map", SCALE_X, self$panel_scales_x)
        layer_data[, x_vars] <- new_x
      }

      y_vars <- intersect(self$panel_scales_y[[1]]$aesthetics, names(layer_data))
      if (length(y_vars) > 0) {
        if (is.null(match_id)) {
          match_id <- match(layer_data$PANEL, layout$PANEL)
        }
        names(y_vars) <- y_vars
        SCALE_Y <- layout$SCALE_Y[match_id]
        new_y <- ggplot2:::scale_apply(layer_data, y_vars, "map", SCALE_Y, self$panel_scales_y)
        layer_data[, y_vars] <- new_y
      }

      layer_data
    })
  }

)

add_layout_panel_metadata <- function(data, layout) {
  if (!is.data.frame(layout) || !"PANEL" %in% names(layout)) {
    return(data)
  }

  metadata_cols <- intersect(c("PANEL", "t_panel", "q_panel"), names(layout))
  if (length(metadata_cols) <= 1L) {
    return(data)
  }

  panel_metadata <- unique(layout[, metadata_cols, drop = FALSE])
  panel_levels <- as.integer(panel_metadata$PANEL)
  panel_metadata$PANEL <- panel_levels

  lapply(data, function(layer_data) {
    if (!is.data.frame(layer_data) || !"PANEL" %in% names(layer_data)) {
      return(layer_data)
    }

    panel_ids <- if (is.factor(layer_data$PANEL)) {
      as.integer(as.character(layer_data$PANEL))
    } else {
      as.integer(layer_data$PANEL)
    }

    layer_data$PANEL <- panel_ids
    layer_data <- dplyr::left_join(layer_data, panel_metadata, by = "PANEL")
    layer_data$PANEL <- factor(layer_data$PANEL, levels = panel_levels)
    layer_data
  })
}
