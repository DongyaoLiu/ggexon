#' @export
ggexon <- function(data = NULL, mapping = aes(), ...,
                   environment = parent.frame()) {
  UseMethod("ggexon")
}


#' @export
ggexon.default <- function(data = NULL, mapping = aes(), ...,
                           environment = parent.frame()){
  if (!missing(mapping) && !is_mapping(mapping)) {
    cli::cli_abort(c(
      "{.arg mapping} must be created with {.fn aes}.",
      "x" = "You've supplied {.obj_type_friendly {mapping}}."
    ))
  }

  data <- fortify(data, ...)
  print(environment)

  p <- structure(list(
    data = data,
    layers = list(),
    scales = ggplot2:::scales_list(),
    guides = ggplot2:::guides_list(),
    mapping = mapping,
    theme = list(),
    coordinates = coord_cartesian(default = TRUE),
    facet = ggplot2::facet_null(),
    plot_env = environment,
    layout = ggplot2::ggproto(NULL, Layout)), class = c("gg", "ggplot", "ggexon"))

  p$labels <- make_labels(mapping)

  set_last_plot(p)
  class(p) = c("ggexon", "ggplot","gg")
  return(p)
}

#' @export
is.ggexon <- function(x) inherits(x, "ggexon")



#' @export
print.ggexon <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  set_last_plot(x)
  if (newpage) grid.newpage()

  # Record dependency on 'ggplot2' on the display list
  # (AFTER grid.newpage())
  grDevices::recordGraphics(
    requireNamespace("ggexon", quietly = TRUE),
    list(),
    getNamespace("ggexon")
  )
  cli::cli_alert_info("Building......")
  data <- ggplot_build(x)
  print(class(data))
  cli::cli_alert_success("Building finished")


  cli::cli_alert_info("Start Gtabling")
  gtable <- ggplot_gtable2(data)
  cli::cli_alert_success("Gtabling finished")
  if (is.null(vp)) {
    grid.draw(gtable)
  } else {
    if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
    grid.draw(gtable)
    upViewport()
  }

  if (isTRUE(getOption("BrailleR.VI")) && rlang::is_installed("BrailleR")) {
    print(asNamespace("BrailleR")$VI(x))
  }

  invisible(x)
}

plot_clone <- function(plot) {
  p <- plot
  p$scales <- plot$scales$clone()

  p
}

#' @export
plot.ggexon <- print.ggexon

