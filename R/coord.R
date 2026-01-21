coord_cartesian_genomic <- function(xlim = NULL, ylim = NULL, expand = TRUE,
                            default = FALSE, clip = "on", reverse = "none",
                            ratio = NULL) {
  ggplot2:::check_coord_limits(xlim)
  ggplot2:::check_coord_limits(ylim)
  ggplot2:::check_number_decimal(ratio, allow_infinite = FALSE, allow_null = TRUE)
  ggproto(NULL, CoordCartesianGenomic,
    limits = list(x = xlim, y = ylim),
    reverse = reverse,
    expand = expand,
    default = default,
    clip = clip,
    ratio = ratio
  )
}

CoordCartesianGenomic <- ggproto("CoordCartesianGenomic", CoordCartesian,
    transform_x = function(data, panel_params){
    reverse <- panel_params$reverse %||% "none"
    x <- panel_params$x[[switch(reverse, xy = , x = "reverse", "rescale")]]
    data <- ggplot2::transform_position(df = data, trans_x = x)
    ggplot2::transform_position(df = data, trans_x = squish_infinite)
    transform_position(data, squish_infinite, squish_infinite)
                          },
    transform_y = function(data, panel_params){
    reverse <- panel_params$reverse %||% "none"
    y <- panel_params$y[[switch(reverse, xy = , y = "reverse", "rescale")]]
    data <- ggplot2::transform_position(df = data, trans_y = y)
    transform_position(data, squish_infinite, squish_infinite)
    }
)
