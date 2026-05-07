#' @importFrom ggplot2 ggplot_add ggplot_build ggplot_gtable
NULL

create_layout <- function(facet, coord, layout = NULL) {
   layout <- layout %||% Layout
   check_inherits(layout, "Layout")
   ggproto(NULL, layout, facet = facet, coord = coord)
 }

