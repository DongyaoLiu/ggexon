#' @rdname reexports
#' @importFrom ggplot2 zeroGrob
#' @export
zeroGrob <- ggplot2::zeroGrob

is.zero <- function(x) is.null(x) || inherits(x, "zeroGrob")
