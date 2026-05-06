#' ggexon themes for genomic tracks
#'
#' `theme_ggexon_track()` provides compact defaults for genomic tracks. It keeps
#' the x axis visible, hides the y axis used only for track geometry, and removes
#' visual noise from minor grids and legends.
#'
#' `theme_ggexon_genomictree()` builds on `theme_ggexon_track()` for stacked
#' tree-aligned genomic panels. The tree-tip labels use `strip.text.y`, and the
#' custom tree branch-length axis reuses the x-axis text styling.
#'
#' @param base_size Base font size passed to [ggplot2::theme_minimal()].
#' @param base_family Base font family passed to [ggplot2::theme_minimal()].
#' @param show_x_axis Logical; show x-axis labels, ticks, and axis line.
#' @param show_y_axis Logical; show y-axis labels and ticks. Defaults to `FALSE`
#'   because most ggexon geoms use y as a track-position coordinate.
#' @param show_x_grid Logical; show major x grid lines.
#' @param show_legend Logical; keep the legend. Defaults to `FALSE`.
#'
#' @return A ggplot2 theme object.
#' @export
theme_ggexon_track <- function(base_size = 8,
                               base_family = "",
                               show_x_axis = TRUE,
                               show_y_axis = FALSE,
                               show_x_grid = TRUE,
                               show_legend = FALSE) {
  x_axis_text <- if (isTRUE(show_x_axis)) {
    ggplot2::element_text(size = base_size * 0.9, colour = "grey25")
  } else {
    ggplot2::element_blank()
  }
  x_axis_line <- if (isTRUE(show_x_axis)) {
    ggplot2::element_line(colour = "grey55", linewidth = 0.25)
  } else {
    ggplot2::element_blank()
  }
  x_axis_ticks <- if (isTRUE(show_x_axis)) {
    ggplot2::element_line(colour = "grey35", linewidth = 0.25)
  } else {
    ggplot2::element_blank()
  }

  y_axis_text <- if (isTRUE(show_y_axis)) {
    ggplot2::element_text(size = base_size * 0.9, colour = "grey25")
  } else {
    ggplot2::element_blank()
  }
  y_axis_ticks <- if (isTRUE(show_y_axis)) {
    ggplot2::element_line(colour = "grey35", linewidth = 0.25)
  } else {
    ggplot2::element_blank()
  }

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.major.x = if (isTRUE(show_x_grid)) {
        ggplot2::element_line(colour = "grey90", linewidth = 0.25)
      } else {
        ggplot2::element_blank()
      },
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text.x = x_axis_text,
      axis.text.y = y_axis_text,
      axis.ticks.x = x_axis_ticks,
      axis.ticks.y = y_axis_ticks,
      axis.ticks.length.x = grid::unit(2, "pt"),
      axis.line.x = x_axis_line,
      legend.position = if (isTRUE(show_legend)) "right" else "none",
      strip.text.y = ggplot2::element_text(
        size = base_size,
        colour = "grey20",
        margin = ggplot2::margin(r = 4, l = 4)
      ),
      strip.background = ggplot2::element_blank(),
      panel.spacing.y = grid::unit(5, "pt")
    )
}

#' @rdname theme_ggexon_track
#' @export
theme_ggexon_genomictree <- function(base_size = 8,
                                     base_family = "",
                                     show_x_axis = TRUE,
                                     show_y_axis = FALSE,
                                     show_x_grid = TRUE,
                                     show_legend = FALSE) {
  theme_ggexon_track(
    base_size = base_size,
    base_family = base_family,
    show_x_axis = show_x_axis,
    show_y_axis = show_y_axis,
    show_x_grid = show_x_grid,
    show_legend = show_legend
  ) +
    ggplot2::theme(
      strip.text.y = ggplot2::element_text(
        size = base_size * 1.05,
        colour = "grey15",
        margin = ggplot2::margin(r = 5, l = 5)
      ),
      plot.margin = ggplot2::margin(4, 6, 4, 4)
    )
}

ggexon_element_text_gpar <- function(element, default_size = 8) {
  if (is.null(element) || inherits(element, "element_blank")) {
    return(NULL)
  }
  grid::gpar(
    col = element$colour %||% element$color %||% "black",
    fontsize = element$size %||% default_size,
    fontfamily = element$family %||% "",
    fontface = element$face %||% "plain",
    lineheight = element$lineheight %||% 0.9
  )
}

ggexon_element_line_gpar <- function(element,
                                     default_colour = "black",
                                     default_linewidth = 0.25,
                                     default_linetype = 1) {
  if (is.null(element) || inherits(element, "element_blank")) {
    return(NULL)
  }
  grid::gpar(
    col = element$colour %||% element$color %||% default_colour,
    lwd = element$linewidth %||% element$size %||% default_linewidth,
    lty = element$linetype %||% default_linetype
  )
}
