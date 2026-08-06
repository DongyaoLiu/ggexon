#' Shared background-free theme for ggexon plots
#'
#' `theme_ggexon_base()` is the common foundation for ggexon's specialized
#' themes. It uses [ggplot2::theme_minimal()] typography and coordinate grids
#' while removing decorative plot, panel, strip, border, and legend
#' backgrounds. Strip text, axes, and grid lines remain available for derived
#' themes to style.
#'
#' @param base_size Base font size passed to [ggplot2::theme_minimal()].
#' @param base_family Base font family passed to [ggplot2::theme_minimal()].
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point() +
#'   theme_ggexon_base()
#'
#' @seealso [theme_ggexon_track()], [theme_ggexon_side_strips()],
#'   [theme_ggexon_pairwise()]
#' @export
theme_ggexon_base <- function(base_size = 8, base_family = "") {
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) +
    .theme_ggexon_backgrounds()
}

.theme_ggexon_backgrounds <- function() {
  ggplot2::theme(
    plot.background = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank()
  )
}

#' ggexon themes for genomic tracks
#'
#' `theme_ggexon_track()` provides compact defaults for genomic tracks. It keeps
#' the x axis visible, hides the y axis used only for track geometry, and removes
#' visual noise from minor grids and legends. It inherits its background-free
#' foundation from [theme_ggexon_base()].
#'
#' `theme_ggexon_genomictree()` builds on `theme_ggexon_track()` for stacked
#' tree-aligned genomic panels. The tree-tip labels use `strip.text.y`, and the
#' custom tree branch-length axis reuses the x-axis text styling. It inherits
#' the shared background contract through the track theme.
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

  theme_ggexon_base(base_size = base_size, base_family = base_family) +
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

#' Place ggexon facet strips on the side
#'
#' A theme helper for putting facet strip labels (e.g. species tracks) on the
#' left or right of the panels instead of stacked on top, which reclaims the
#' vertical row a top strip would otherwise occupy. This styles the side-strip
#' text so labels read horizontally and sit just outside the panels. The helper
#' shares [theme_ggexon_base()]'s blank backgrounds without replacing axes or
#' grids supplied by an existing complete theme.
#'
#' The actual strip *position* is set by the facet, so pair this with
#' `facet_genomics(strip.position = "<side>")` using the same `side`.
#'
#' @param side `"right"` or `"left"`. Must match the `strip.position` passed to
#'   [facet_genomics()].
#' @param base_size Base font size for the strip text.
#' @param face Font face for the strip text (e.g. `"bold"`).
#' @param background Strip-background fill colour. `NA` (the default) or
#'   `"none"` draws no strip rectangle. A colour explicitly overrides the
#'   shared background-free default.
#'
#' @return A ggplot2 theme object to add to a ggexon plot.
#'
#' @examples
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point() +
#'   ggplot2::facet_wrap(ggplot2::vars(cyl), ncol = 1, strip.position = "left")
#' p + theme_ggexon_side_strips("left")
#' @seealso [theme_ggexon_base()], [theme_ggexon_track()], [facet_genomics()]
#' @export
theme_ggexon_side_strips <- function(side = c("right", "left"),
                                     base_size = 8,
                                     face = "bold",
                                     background = NA) {
  side <- arg_match0(side, c("right", "left"))
  strip_text <- ggplot2::element_text(
    size = base_size,
    face = face,
    colour = "grey20",
    angle = 0,
    hjust = if (side == "left") 1 else 0,
    margin = ggplot2::margin(l = 3, r = 3)
  )
  strip_bg <- if (length(background) != 1L || is.na(background) ||
                  identical(background, "none")) {
    ggplot2::element_blank()
  } else {
    ggplot2::element_rect(fill = background, colour = NA)
  }
  args <- list(
    strip.placement = "outside",
    strip.text.y = strip_text,
    strip.background = strip_bg
  )
  args[[if (side == "left") "strip.text.y.left" else "strip.text.y.right"]] <- strip_text
  .theme_ggexon_backgrounds() + do.call(ggplot2::theme, args)
}

#' Theme for pairwise genomic alignments
#'
#' `theme_ggexon_pairwise()` provides compact styling for a top annotation
#' panel, a middle linkage panel, and a bottom annotation panel. It hides the
#' annotation y axes and places horizontal facet-label styling on the left
#' without drawing strip-background bars. It inherits the shared background
#' contract through [theme_ggexon_track()].
#'
#' The facet controls the actual strip position and annotation alignment. Pair
#' this theme with
#' `facet_genomics(strip.position = "left", vertical = "center")`.
#'
#' @inheritParams theme_ggexon_track
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' tracks <- c("human", "link_human_mouse", "mouse")
#' genes <- data.frame(
#'   track = factor(c("human", "mouse"), levels = tracks),
#'   xmin = c(10, 1010),
#'   xmax = c(80, 1080),
#'   y = 1,
#'   strand = "+",
#'   gene = c("GENE1", "Gene1")
#' )
#' links <- data.frame(
#'   track = factor("link_human_mouse", levels = tracks),
#'   tspecies = "human", tchr = "chr1", tstart = 20, tend = 60,
#'   strand = "+",
#'   qspecies = "mouse", qchr = "chr1", qstart = 1020, qend = 1060,
#'   group = 1
#' )
#'
#' ggexon() +
#'   geom_genetag(data = genes, label_position = "outside") +
#'   geom_synteny_link(
#'     data = links,
#'     ggplot2::aes(
#'       tspecies = tspecies, tchr = tchr, tstart = tstart, tend = tend,
#'       strand = strand,
#'       qspecies = qspecies, qchr = qchr, qstart = qstart, qend = qend,
#'       group = group
#'     ),
#'     inherit.aes = FALSE
#'   ) +
#'   facet_genomics(
#'     ggplot2::vars(track),
#'     ncol = 1,
#'     scales = "free_x",
#'     strip.position = "left",
#'     link_axis = "none",
#'     link_strip = "blank",
#'     annotation_axis = "bottom",
#'     vertical = "center"
#'   ) +
#'   theme_ggexon_pairwise()
#'
#' @seealso [theme_ggexon_base()], [theme_ggexon_track()],
#'   [theme_ggexon_side_strips()],
#'   [facet_genomics()]
#' @export
theme_ggexon_pairwise <- function(base_size = 8,
                                  base_family = "",
                                  show_x_axis = TRUE,
                                  show_x_grid = TRUE,
                                  show_legend = FALSE) {
  theme_ggexon_track(
    base_size = base_size,
    base_family = base_family,
    show_x_axis = show_x_axis,
    show_y_axis = FALSE,
    show_x_grid = show_x_grid,
    show_legend = show_legend
  ) +
    theme_ggexon_side_strips(side = "left", base_size = base_size)
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
