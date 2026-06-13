#' Declare the intended output size for a ggexon plot
#'
#' `ggexon_output_size()` records the final render size that downstream
#' layout code can use as an output-size contract. Text annotation layout still
#' measures each panel viewport when possible, but this metadata gives ggexon a
#' stable fallback and documents the dimensions the figure is designed for.
#'
#' @param width,height Positive numeric output dimensions.
#' @param units Unit for `width` and `height`. One of `"in"`, `"cm"`, `"mm"`,
#'   or `"px"`.
#' @param dpi Pixel density used when `units = "px"`.
#'
#' @return A ggexon output-size specification.
#' @export
ggexon_output_size <- function(width,
                               height,
                               units = "in",
                               dpi = 300) {
  units <- match.arg(units, c("in", "cm", "mm", "px"))
  width <- .ggexon_output_dimension(width, "width")
  height <- .ggexon_output_dimension(height, "height")
  dpi <- .ggexon_output_dpi(dpi)
  mm <- .ggexon_output_to_mm(width = width, height = height, units = units, dpi = dpi)

  structure(
    list(
      width = width,
      height = height,
      units = units,
      dpi = dpi,
      width_mm = mm$width,
      height_mm = mm$height
    ),
    class = "ggexon_output_size_spec"
  )
}

#' @export
ggplot_add.ggexon_output_size_spec <- function(object, plot, ...) {
  if (!is_ggexon(plot)) {
    stop("`ggexon_output_size()` can only be added to a ggexon plot.", call. = FALSE)
  }
  plot@output_size <- object
  plot
}

.ggexon_output_dimension <- function(x, arg) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
    stop("`", arg, "` must be a positive number.", call. = FALSE)
  }
  x
}

.ggexon_output_dpi <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
    stop("`dpi` must be a positive number.", call. = FALSE)
  }
  x
}

.ggexon_output_to_mm <- function(width, height, units, dpi = 300) {
  multiplier <- switch(
    units,
    `in` = 25.4,
    cm = 10,
    mm = 1,
    px = 25.4 / dpi
  )
  list(width = width * multiplier, height = height * multiplier)
}

ggexon_output_width_mm <- function(output_size) {
  if (!inherits(output_size, "ggexon_output_size_spec")) {
    return(NA_real_)
  }
  width_mm <- suppressWarnings(as.numeric(output_size$width_mm))
  if (length(width_mm) != 1L || is.na(width_mm) || !is.finite(width_mm) || width_mm <= 0) {
    return(NA_real_)
  }
  width_mm
}
