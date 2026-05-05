#' Create the ggexon layout runtime
#'
#' `create_layout2()` builds the `Layout2` ggproto object used by
#' `ggexon_build()`. This runtime object sits between layers, facets, and
#' coordinates during plot build and is responsible for turning the facet's
#' panel table into panel-aware layer data.
#'
#' In ggexon, `Layout2` extends the standard ggplot2 layout pipeline with Syn-
#' specific behavior:
#'
#' - `SynSpecies` / `SynIndividual` plot data can be carried into facet setup
#'   without being treated as an ordinary data frame.
#' - a stored `SynLayout` can be supplied as an override and reused during build.
#' - link layers can trigger genomic panel reordering and panel metadata such as
#'   `t_panel` / `q_panel`.
#' - panel metadata is joined back onto layer data so geoms such as
#'   `geom_nuclink()` can transform each side of a link against the correct
#'   annotation panel.
#'
#' @param facet A facet ggproto object, usually `FacetGenomics` or a standard
#'   ggplot2 facet.
#' @param coord A coordinate ggproto object.
#' @param layout Optional layout ggproto subclass. Defaults to `Layout2`.
#'
#' @return A ggproto layout object used internally by `ggexon_build()`.
#' @keywords internal
create_layout2 <- function(facet, coord, layout = NULL) {
   layout <- layout %||% Layout2
   ggplot2:::check_inherits(layout, "Layout2")
   ggproto(NULL, layout, facet = facet, coord = coord)
}

#' ggexon layout runtime with Syn-aware panel setup
#'
#' `Layout2` is ggexon's custom layout ggproto. It inherits from ggplot2's
#' `Layout` and overrides the parts of the build pipeline where Syn-aware panel
#' structure and link metadata need to be introduced.
#'
#' Compared with the upstream layout, `Layout2` adds two main responsibilities:
#'
#' 1. `setup()` carries Syn plot data and optional stored `SynLayout` metadata
#'    into facet setup, lets `facet_genomics()` generate or reuse genomic
#'    panel layouts, and joins panel-level metadata such as `t_panel` and
#'    `q_panel` back onto layer data.
#' 2. `map_position()` maps x/y aesthetics panel-by-panel using the trained
#'    scales from the resolved layout while preserving the extra panel metadata
#'    introduced during setup.
#'
#' This class is what makes stored `SynLayout` objects, link panels, and
#' cross-panel coordinate borrowing work inside the normal ggplot2 build flow.
#'
#' @section Build flow:
#' The high-level flow is:
#'
#' - `ggexon_build()` creates `Layout2` with `create_layout2()`.
#' - `Layout2$setup()` asks the active facet for the panel table.
#' - `facet_genomics()` may return a stored `SynLayout`, derive a new
#'   chain layout, or fall back to a standard faceting layout.
#' - if link panels are present, link-direction metadata is added and source
#'   panel ids are propagated to layer data.
#' - `Layout2$map_position()` maps each layer's x/y aesthetics against the
#'   `SCALE_X` / `SCALE_Y` assignments in the final panel table.
#'
#' @seealso [SynLayout]
#' @export
Layout2 <- ggproto("Layout2", Layout,
  # Override ggplot2's Layout$setup() to carry Syn plot objects, stored
  # SynLayout overrides, and link-panel metadata through the facet pipeline.
  setup = function(self, data, plot_data = data_frame0(), plot_env = emptyenv()) {
    plot_data_raw <- plot_data
    layout_override <- NULL
    if (methods::is(plot_data, "SynSpecies") || methods::is(plot_data, "SynIndividual")) {
      # The original Syn object is still passed to the facet via
      # facet_params$plot_data, but the standard ggplot2 setup path expects a
      # data frame here.
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
    self$facet_params$genomic_tree <- self$genomic_tree %||% NULL

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

    # This is ggexon-specific: if link panels are present, let the facet
    # reorder panels and annotate the layout with source-panel metadata.
    if ("track" %in% colnames(self$layout) &&
        TRUE %in% stringr::str_detect(self$layout$track, "link")) {
      if (!"panel_type" %in% colnames(self$layout)) {
        self$layout <- self$facet$compute_alignment_layout(data, self$layout)
      }

      # Assign upper/lower link anchors from the resolved panel ordering.
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

  # Override ggplot2's Layout$map_position() so we map positions after ggexon
  # has attached extra panel metadata such as t_panel and q_panel.
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

    # Join source-panel metadata back onto layer data so geoms such as
    # geom_nuclink() can later transform each half-link against the correct
    # annotation panel range.
    layer_data$PANEL <- panel_ids
    layer_data <- dplyr::left_join(layer_data, panel_metadata, by = "PANEL")
    layer_data$PANEL <- factor(layer_data$PANEL, levels = panel_levels)
    layer_data
  })
}
