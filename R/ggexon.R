#' @export
ggexon <- function(data = NULL, mapping = aes(), ...,
                   environment = parent.frame()) {
  UseMethod("ggexon")
}


#' @export
ggexon.default <- function(data = NULL, mapping = aes(), nuc_link = NULL, pro_link = NULL, ...,
                           environment = parent.frame()){
  if (!missing(mapping) && !is_mapping(mapping)) {
    cli::cli_abort(c(
      "{.arg mapping} must be created with {.fn aes}.",
      "x" = "You've supplied {.obj_type_friendly {mapping}}."
    ))
  }

  data <- fortify(data, ...)
  print(environment)

  p <- class_ggexon(
    data = data,
    mapping = mapping,
    plot_env = environment,
    nuc_link = nuc_link  %||% NULL,
    pro_link = pro_link %||% NULL
    )

  class(p) = union(union(c("ggexon", "ggplot2::ggplot", "ggplot"), class(p)), "gg")

  set_last_plot(p)
  return(p)
}

#' @export
#is.ggexon <- function(x) inherits(x, "ggexon")
#switch to S7
is_ggexon <- function(x) S7::S7_inherits(x, class_ggexon)

local({
  S7::method(print, class_ggexon) <- S7::method(plot, class_ggexon) <-
    function(x, newpage = is.null(vp), vp = NULL, ...) {
      set_last_plot(x)
      if (newpage) grid.newpage()

      # Record dependency on 'ggplot2' on the display list
      # (AFTER grid.newpage())
      grDevices::recordGraphics(
        requireNamespace("ggplot2", quietly = TRUE),
        list(),
        getNamespace("ggplot2")
      )

      data <- ggplot_build(x)

#only rewrite the code inside ggplot_gtable
      gtable <- ggplot_gtable(data)
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
})


plot_clone <- function(plot) {
  p <- plot
  p@scales <- plot@scales$clone()
  p
}
