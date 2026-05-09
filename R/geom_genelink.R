#' Compute cubic Bézier curve points
#'
#' Evaluates a cubic Bézier curve defined by four control points.
#'
#' @param x0,y0 Start point.
#' @param x1,y1 First control point.
#' @param x2,y2 Second control point.
#' @param x3,y3 End point.
#' @param n Number of evaluation points.
#' @return A data.frame with columns `x` and `y`.
#' @keywords internal
.bezier_curve <- function(x0, y0, x1, y1, x2, y2, x3, y3, n = 50L) {
  t <- seq(0, 1, length.out = n)
  mt <- 1 - t
  bx <- mt^3 * x0 + 3 * mt^2 * t * x1 + 3 * mt * t^2 * x2 + t^3 * x3
  by <- mt^3 * y0 + 3 * mt^2 * t * y1 + 3 * mt * t^2 * y2 + t^3 * y3
  data.frame(x = bx, y = by, stringsAsFactors = FALSE)
}


#' Build link line grobs from already-transformed data
#'
#' Internal helper shared by `GeomGeneLink` and other geoms (e.g.
#' `GeomGeneLabel`). Takes coordinate-transformed data and returns
#' a grid grob (or gList).
#'
#' @param data A data.frame with columns `x`, `y`, `xend`, `yend`,
#'   `colour`, `linewidth`, `linetype`, `alpha`. Coordinates must be
#'   in native units.
#' @param link_type One of `"straight"`, `"elbow"`, or `"spline"`.
#' @return A grob.
#' @keywords internal
.draw_link_grobs_raw <- function(data, link_type) {
  link_type <- match.arg(link_type, c("straight", "elbow", "spline"))
  if (nrow(data) == 0L) return(zeroGrob())

  if (link_type == "straight") {
    return(segmentsGrob(
      x0 = data$x, y0 = data$y,
      x1 = data$xend, y1 = data$yend,
      default.units = "native",
      gp = gpar(
        col = alpha(data$colour, data$alpha),
        lwd = data$linewidth,
        lty = data$linetype
      )
    ))
  }

  if (link_type == "elbow") {
    seg1 <- segmentsGrob(
      x0 = data$x, y0 = data$y,
      x1 = data$x, y1 = data$yend,
      default.units = "native",
      gp = gpar(
        col = alpha(data$colour, data$alpha),
        lwd = data$linewidth,
        lty = data$linetype
      )
    )
    seg2 <- segmentsGrob(
      x0 = data$x, y0 = data$yend,
      x1 = data$xend, y1 = data$yend,
      default.units = "native",
      gp = gpar(
        col = alpha(data$colour, data$alpha),
        lwd = data$linewidth,
        lty = data$linetype
      )
    )
    return(gList(seg1, seg2))
  }

  # spline
  spline_grobs <- lapply(seq_len(nrow(data)), function(i) {
    pts <- .bezier_curve(
      x0 = data$x[[i]],      y0 = data$y[[i]],
      x1 = data$x[[i]],      y1 = data$yend[[i]],
      x2 = data$xend[[i]],   y2 = data$y[[i]],
      x3 = data$xend[[i]],   y3 = data$yend[[i]],
      n = 50L
    )
    polylineGrob(
      x = pts$x, y = pts$y,
      default.units = "native",
      gp = gpar(
        col = alpha(data$colour[[i]], data$alpha[[i]]),
        lwd = data$linewidth[[i]],
        lty = data$linetype[[i]]
      )
    )
  })

  if (length(spline_grobs) == 1L) spline_grobs[[1L]]
  else do.call(gList, spline_grobs)
}


GeomGeneLink <- ggproto("GeomGeneLink", Geom,
  required_aes = c("x", "y", "xend", "yend"),
  default_aes = aes(
    colour = "grey60",
    linewidth = 0.5,
    linetype = "solid",
    alpha = NA
  ),
  extra_params = c("na.rm", "link_type"),
  default_params = function() {
    list(
      link_type = "straight"
    )
  },

  draw_panel = function(data, panel_params, coord,
                         link_type = "straight",
                         ...) {
    if (nrow(data) == 0L) return(zeroGrob())
    data <- coord$transform(data, panel_params)
    .draw_link_grobs_raw(data, link_type)
  },

  draw_key = draw_key_path
)


#' Draw link lines between pairs of coordinates
#'
#' `geom_genelink()` draws link lines between start points `(x, y)` and end
#' points `(xend, yend)`. Three line styles are supported via the
#' `link_type` parameter:
#'
#' * `"straight"` (default) — a single straight segment.
#' * `"elbow"` — two right-angle segments with a bend at `(x, yend)`.
#' * `"spline"` — a smooth cubic Bézier curve from `(x, y)` to
#'   `(xend, yend)` with auto-derived control points.
#'
#' This geom is designed to work generically with any coordinate data,
#' making it reusable for gene labels, mutation annotations, or other
#' link-line needs.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#'   Required: `x`, `y`, `xend`, `yend`.
#' @param data The data to be displayed.
#' @param stat,position,...,na.rm,show.legend,inherit.aes Standard ggplot2
#'   layer arguments.
#' @param link_type Line style: `"straight"`, `"elbow"`, or `"spline"`.
#'   Default `"straight"`.
#'
#' @return A ggplot2 layer using the internal `GeomGeneLink` ggproto.
#' @export
geom_genelink <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ..., na.rm = FALSE, show.legend = NA,
                          link_type = "straight",
                          inherit.aes = TRUE) {
  link_type <- match.arg(link_type, c("straight", "elbow", "spline"))

  layer(
    data = data,
    mapping = mapping,
    geom = GeomGeneLink,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      link_type = link_type,
      ...
    )
  )
}
