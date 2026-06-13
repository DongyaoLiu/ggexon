#' Create a ggexon plot
#'
#' `ggexon()` starts a ggplot-like object for genomic annotations, synteny,
#' and associated track data. It follows the same basic shape as
#' [`ggplot2::ggplot()`], while preserving `SynIndividual` and `SynSpecies`
#' containers so ggexon layers can resolve their plotting data lazily during
#' plot build.
#'
#' @param data A data frame, `SynIndividual`, `SynSpecies`, or another object
#'   that can be fortified for plotting.
#' @param mapping Default aesthetic mappings created by [`ggplot2::aes()`].
#' @param ... Additional arguments passed to `fortify()` when `data` is not a
#'   ggexon Syn object.
#' @param environment Plot environment. Defaults to the caller environment.
#'
#' @return A ggexon plot object inheriting from `ggplot`.
#' @seealso [`geom_exon()`], [`facet_genomics()`], [`SynIndividual()`],
#'   [`SynSpecies()`]
#' @export
ggexon <- function(data = NULL, mapping = aes(), ...,
                   environment = parent.frame()) {
  UseMethod("ggexon")
}


#' @rdname ggexon
#' @export
ggexon.default <- function(data = NULL, mapping = aes(), ...,
                           environment = parent.frame()){
  if (!missing(mapping) && !is_mapping(mapping)) {
    cli::cli_abort(c(
      "{.arg mapping} must be created with {.fn aes}.",
      "x" = "You've supplied {.obj_type_friendly {mapping}}."
    ))
  }

  if (!(methods::is(data, "SynSpecies") || methods::is(data, "SynIndividual"))) {
    data <- fortify(data, ...)
  }

  p <- class_ggexon(
    data = data,
    mapping = mapping,
    plot_env = environment,
    )
  class(p) = union(union(c("ggexon", "ggexon::ggexon", "ggplot2::ggplot"), class(p)), "gg")

  p <- p + ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0.05, 0.05))
  )

  set_last_plot(p)
  return(p)
}

#' Test whether an object is a ggexon plot
#'
#' @param x An object to test.
#' @return `TRUE` when `x` is a `ggexon` plot object, otherwise `FALSE`.
#' @export
is_ggexon <- function(x) S7::S7_inherits(x, class_ggexon)

local({
  S7::method(print, class_ggexon) <- S7::method(plot, class_ggexon) <-
    function(x, newpage = is.null(vp), vp = NULL, ...) {
      set_last_plot(x)
      if (newpage) grid.newpage()

      # Record dependency on 'ggplot2' on the display list
      # (AFTER grid.newpage())
      grDevices::recordGraphics(
        requireNamespace("ggexon", quietly = TRUE),
        list(),
        getNamespace("ggexon")
      )
      print("using ggexon build")
      data <- ggexon_build(x)

      gtable <- ggplot2::ggplot_gtable(data)
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
