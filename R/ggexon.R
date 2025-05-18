#' @export
ggexon <- function(data = NULL, mapping = aes(), ...,
                   environment = parent.frame()) {
  UseMethod("ggexon")
}


#' @export
ggexon.default <- function(data = NULL, mapping = aes(), ...,
                           environment = parent.frame()){
    p = ggplot2::ggplot(data = data, mapping = mapping, ...,
                            environment = environment)
    class(p) = c("ggexon", "ggplot","gg")
    print(class(p))
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
  cli::cli_alert_success("Building finished")


  cli::cli_alert_info("Start Gtabling")
  gtable <- ggplot_gtable(data)
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

