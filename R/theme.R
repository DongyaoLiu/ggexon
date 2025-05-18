theme <- function(...,
                  line,
                  rect,
                  text,
                  title,
                  aspect.ratio,
                  axis.title,
                  axis.title.x,
                  axis.title.x.top,
                  axis.title.x.bottom,
                  axis.title.y,
                  axis.title.y.left,
                  axis.title.y.right,
                  axis.text,
                  axis.text.x,
                  axis.text.x.top,
                  axis.text.x.bottom,
                  axis.text.y,
                  axis.text.y.left,
                  axis.text.y.right,
                  axis.text.theta,
                  axis.text.r,
                  axis.ticks,
                  axis.ticks.x,
                  axis.ticks.x.top,
                  axis.ticks.x.bottom,
                  axis.ticks.y,
                  axis.ticks.y.left,
                  axis.ticks.y.right,
                  axis.ticks.theta,
                  axis.ticks.r,
                  axis.minor.ticks.x.top,
                  axis.minor.ticks.x.bottom,
                  axis.minor.ticks.y.left,
                  axis.minor.ticks.y.right,
                  axis.minor.ticks.theta,
                  axis.minor.ticks.r,
                  axis.ticks.length,
                  axis.ticks.length.x,
                  axis.ticks.length.x.top,
                  axis.ticks.length.x.bottom,
                  axis.ticks.length.y,
                  axis.ticks.length.y.left,
                  axis.ticks.length.y.right,
                  axis.ticks.length.theta,
                  axis.ticks.length.r,
                  axis.minor.ticks.length,
                  axis.minor.ticks.length.x,
                  axis.minor.ticks.length.x.top,
                  axis.minor.ticks.length.x.bottom,
                  axis.minor.ticks.length.y,
                  axis.minor.ticks.length.y.left,
                  axis.minor.ticks.length.y.right,
                  axis.minor.ticks.length.theta,
                  axis.minor.ticks.length.r,
                  axis.line,
                  axis.line.x,
                  axis.line.x.top,
                  axis.line.x.bottom,
                  axis.line.y,
                  axis.line.y.left,
                  axis.line.y.right,
                  axis.line.theta,
                  axis.line.r,
                  legend.background,
                  legend.margin,
                  legend.spacing,
                  legend.spacing.x,
                  legend.spacing.y,
                  legend.key,
                  legend.key.size,
                  legend.key.height,
                  legend.key.width,
                  legend.key.spacing,
                  legend.key.spacing.x,
                  legend.key.spacing.y,
                  legend.frame,
                  legend.ticks,
                  legend.ticks.length,
                  legend.axis.line,
                  legend.text,
                  legend.text.position,
                  legend.title,
                  legend.title.position,
                  legend.position,
                  legend.position.inside,
                  legend.direction,
                  legend.byrow,
                  legend.justification,
                  legend.justification.top,
                  legend.justification.bottom,
                  legend.justification.left,
                  legend.justification.right,
                  legend.justification.inside,
                  legend.location,
                  legend.box,
                  legend.box.just,
                  legend.box.margin,
                  legend.box.background,
                  legend.box.spacing,
                  panel.background,
                  panel.border,
                  panel.spacing,
                  panel.spacing.x,
                  panel.spacing.y,
                  panel.grid,
                  panel.grid.major,
                  panel.grid.minor,
                  panel.grid.major.x,
                  panel.grid.major.y,
                  panel.grid.minor.x,
                  panel.grid.minor.y,
                  panel.ontop,
                  plot.background,
                  plot.title,
                  plot.title.position,
                  plot.subtitle,
                  plot.caption,
                  plot.caption.position,
                  plot.tag,
                  plot.tag.position,
                  plot.tag.location,
                  plot.margin,
                  strip.background,
                  strip.background.x,
                  strip.background.y,
                  strip.clip,
                  strip.placement,
                  strip.text,
                  strip.text.x,
                  strip.text.x.bottom,
                  strip.text.x.top,
                  strip.text.y,
                  strip.text.y.left,
                  strip.text.y.right,
                  strip.switch.pad.grid,
                  strip.switch.pad.wrap,
                  complete = FALSE,
                  validate = TRUE) {
  elements <- find_args(..., complete = NULL, validate = NULL)

  if (!is.null(elements$axis.ticks.margin)) {
    deprecate_warn0(
      "2.0.0", "theme(axis.ticks.margin)",
      details = "Please set `margin` property of `axis.text` instead"
    )
    elements$axis.ticks.margin <- NULL
  }
  if (!is.null(elements$panel.margin)) {
    deprecate_warn0(
      "2.2.0", "theme(panel.margin)", "theme(panel.spacing)"
    )
    elements$panel.spacing <- elements$panel.margin
    elements$panel.margin <- NULL
  }
  if (!is.null(elements$panel.margin.x)) {
    deprecate_warn0(
      "2.2.0", "theme(panel.margin.x)", "theme(panel.spacing.x)"
    )
    elements$panel.spacing.x <- elements$panel.margin.x
    elements$panel.margin.x <- NULL
  }
  if (!is.null(elements$panel.margin.y)) {
    deprecate_warn0(
      "2.2.0", "theme(panel.margin.y)", "theme(panel.spacing.y)"
    )
    elements$panel.spacing.y <- elements$panel.margin.y
    elements$panel.margin.y <- NULL
  }
  if (is.unit(elements$legend.margin) && !is_margin(elements$legend.margin)) {
    cli::cli_warn(c(
      "{.var legend.margin} must be specified using {.fn margin}",
      "i" = "For the old behavior use {.var legend.spacing}"
    ))
    elements$legend.spacing <- elements$legend.margin
    elements$legend.margin <- margin()
  }
  if (!is.null(elements$legend.title.align)) {
    deprecate_soft0(
      "3.5.0", "theme(legend.title.align)",
      I("theme(legend.title = element_text(hjust))")
    )
    if (is.null(elements[["legend.title"]])) {
      elements$legend.title <- element_text(hjust = elements$legend.title.align)
    } else {
      elements$legend.title$hjust <- elements$legend.title$hjust %||%
        elements$legend.title.align
    }
    elements$legend.title.align <- NULL
  }
  if (!is.null(elements$legend.text.align)) {
    deprecate_soft0(
      "3.5.0", "theme(legend.text.align)",
      I("theme(legend.text = element_text(hjust))")
    )
    if (is.null(elements[["legend.text"]])) {
      elements$legend.text <- element_text(hjust = elements$legend.text.align)
    } else {
      elements$legend.text$hjust <- elements$legend.text$hjust %||%
        elements$legend.text.align
    }
    elements$legend.text.align <- NULL
  }
  if (is.numeric(elements[["legend.position"]])) {
    deprecate_soft0(
      "3.5.0", I("A numeric `legend.position` argument in `theme()`"),
      "theme(legend.position.inside)"
    )
    elements$legend.position.inside <- elements$legend.position
    elements$legend.position <- "inside"
  }

  # If complete theme set all non-blank elements to inherit from blanks
  if (complete) {
    elements <- lapply(elements, function(el) {
      if (is_theme_element(el) && !is_theme_element(el, "blank")) {
        el$inherit.blank <- TRUE
      }
      el
    })
  }
  structure(
    elements,
    class = c("theme", "gg"),
    complete = complete,
    validate = validate
  )
}

# check whether theme is complete
is_theme_complete <- function(x) isTRUE(attr(x, "complete", exact = TRUE))

# check whether theme should be validated
is_theme_validate <- function(x) {
  validate <- attr(x, "validate", exact = TRUE)
  isTRUE(validate %||% TRUE)
}

validate_theme <- function(theme, tree = get_element_tree(), call = caller_env()) {
  if (!is_theme_validate(theme)) {
    return()
  }
  mapply(
    validate_element, theme, names(theme),
    MoreArgs = list(element_tree = tree, call = call)
  )
}

# Combine plot defaults with current theme to get complete theme for a plot
plot_theme <- function(x, default = theme_get()) {
  theme <- x$theme

  # apply theme defaults appropriately if needed
  if (is_theme_complete(theme)) {
    # for complete themes, we fill in missing elements but don't do any element merging
    # can't use `defaults()` because it strips attributes
    missing <- setdiff(names(default), names(theme))
    theme[missing] <- default[missing]
  } else {
    # otherwise, we can just add the theme to the default theme
    theme <- default + theme
  }

  # if we're still missing elements relative to fallback default, fill in those
  missing <- setdiff(names(ggplot_global$theme_default), names(theme))
  theme[missing] <- ggplot_global$theme_default[missing]

  # Check that all elements have the correct class (element_text, unit, etc)
  validate_theme(theme)

  # Remove elements that are not registered
  theme[setdiff(names(theme), names(get_element_tree()))] <- NULL
  theme
}

#' Modify properties of an element in a theme object
#'
#' @param t1 A theme object
#' @param t2 A theme object that is to be added to `t1`
#' @param t2name A name of the t2 object. This is used for printing
#'   informative error messages.
#' @keywords internal
add_theme <- function(t1, t2, t2name, call = caller_env()) {
  if (is.null(t2)) {
    return(t1)
  }
  if (!is.list(t2)) { # in various places in the code base, simple lists are used as themes
    cli::cli_abort("Can't add {.arg {t2name}} to a theme object.", call = call)
  }

  # If t2 is a complete theme or t1 is NULL, just return t2
  if (is_theme_complete(t2) || is.null(t1))
    return(t2)

  # Iterate over the elements that are to be updated
  try_fetch(
    for (item in names(t2)) {
      x <- merge_element(t2[[item]], t1[[item]])

      # Assign it back to t1
      # This is like doing t1[[item]] <- x, except that it preserves NULLs.
      # The other form will simply drop NULL values
      t1[item] <- list(x)
    },
    error = function(cnd) {
      cli::cli_abort("Can't merge the {.var {item}} theme element.", parent = cnd, call = call)
    }
  )

  # make sure the "complete" attribute is set; this can be missing
  # when t1 is an empty list
  attr(t1, "complete") <- is_theme_complete(t1)

  # Only validate if both themes should be validated
  attr(t1, "validate") <-
    is_theme_validate(t1) && is_theme_validate(t2)

  t1
}


#' Calculate the element properties, by inheriting properties from its parents
#'
#' @param element The name of the theme element to calculate
#' @param theme A theme object (like [theme_grey()])
#' @param verbose If TRUE, print out which elements this one inherits from
#' @param skip_blank If TRUE, elements of type `element_blank` in the
#'   inheritance hierarchy will be ignored.
#' @keywords internal
#' @export
#' @examples
#' t <- theme_grey()
#' calc_element('text', t)
#'
#' # Compare the "raw" element definition to the element with calculated inheritance
#' t$axis.text.x
#' calc_element('axis.text.x', t, verbose = TRUE)
#'
#' # This reports that axis.text.x inherits from axis.text,
#' # which inherits from text. You can view each of them with:
#' t$axis.text.x
#' t$axis.text
#' t$text
calc_element <- function(element, theme, verbose = FALSE, skip_blank = FALSE,
                         call = caller_env()) {
  if (verbose) cli::cli_inform(paste0(element, " --> "))

  el_out <- theme[[element]]

  # If result is element_blank, we skip it if `skip_blank` is `TRUE`,
  # and otherwise we don't inherit anything from parents
  if (inherits(el_out, "element_blank")) {
    if (isTRUE(skip_blank)) {
      el_out <- NULL
    } else {
      if (verbose) cli::cli_inform("{.fn element_blank} (no inheritance)")
      return(el_out)
    }
  }

  # Obtain the element tree
  element_tree <- get_element_tree()

  # If the element is defined (and not just inherited), check that
  # it is of the class specified in element_tree
  if (!is.null(el_out) &&
      !inherits(el_out, element_tree[[element]]$class)) {
    cli::cli_abort("Theme element {.var {element}} must have class {.cls {ggplot_global$element_tree[[element]]$class}}.", call = call)
  }

  # Get the names of parents from the inheritance tree
  pnames <- element_tree[[element]]$inherit

  # If no parents, this is a "root" node. Just return this element.
  if (is.null(pnames)) {
    if (verbose) cli::cli_inform("nothing (top level)")

    # Check that all the properties of this element are non-NULL
    nullprops <- vapply(el_out, is.null, logical(1))
    if (!any(nullprops)) {
      return(el_out) # no null properties, return element as is
    }

    # if we have null properties, try to fill in from ggplot_global$theme_default
    el_out <- combine_elements(el_out, ggplot_global$theme_default[[element]])
    nullprops <- vapply(el_out, is.null, logical(1))
    if (!any(nullprops)) {
      return(el_out) # no null properties remaining, return element
    }

    cli::cli_abort("Theme element {.var {element}} has {.code NULL} property without default: {.field {names(nullprops)[nullprops]}}.", call = call)
  }

  # Calculate the parent objects' inheritance
  if (verbose) cli::cli_inform("{pnames}")
  parents <- lapply(
    pnames,
    calc_element,
    theme,
    verbose = verbose,
    # once we've started skipping blanks, we continue doing so until the end of the
    # recursion; we initiate skipping blanks if we encounter an element that
    # doesn't inherit blank.
    skip_blank = skip_blank || (!is.null(el_out) && !isTRUE(el_out$inherit.blank)),
    call = call
  )

  # Combine the properties of this element with all parents
  Reduce(combine_elements, parents, el_out)
}

#' Merge a parent element into a child element
#'
#' This is a generic and element classes must provide an implementation of this
#' method
#'
#' @param new The child element in the theme hierarchy
#' @param old The parent element in the theme hierarchy
#' @return A modified version of `new` updated with the properties of
#' `old`
#' @keywords internal
#' @export
#' @examples
#' new <- element_text(colour = "red")
#' old <- element_text(colour = "blue", size = 10)
#'
#' # Adopt size but ignore colour
#' merge_element(new, old)
#'
merge_element <- function(new, old) {
  UseMethod("merge_element")
}

#' @rdname merge_element
#' @export
merge_element.default <- function(new, old) {
  if (is.null(old) || inherits(old, "element_blank")) {
    # If old is NULL or element_blank, then just return new
    return(new)
  } else if (is.null(new) || is.character(new) || is.numeric(new) || is.unit(new) ||
             is.logical(new)) {
    # If new is NULL, or a string, numeric vector, unit, or logical, just return it
    return(new)
  }

  # otherwise we can't merge
  cli::cli_abort("No method for merging {.cls {class(new)[1]}} into {.cls {class(old)[1]}}.")
}

#' @rdname merge_element
#' @export
merge_element.element_blank <- function(new, old) {
  # If new is element_blank, just return it
  new
}

#' @rdname merge_element
#' @export
merge_element.element <- function(new, old) {
  if (is.null(old) || inherits(old, "element_blank")) {
    # If old is NULL or element_blank, then just return new
    return(new)
  }

  # actual merging can only happen if classes match
  if (!inherits(new, class(old)[1])) {
    cli::cli_abort("Only elements of the same class can be merged.")
  }

  # Override NULL properties of new with the values in old
  # Get logical vector of NULL properties in new
  idx <- vapply(new, is.null, logical(1))
  # Get the names of TRUE items
  idx <- names(idx[idx])

  # Update non-NULL items
  new[idx] <- old[idx]

  new
}

#' Combine the properties of two elements
#'
#' @param e1 An element object
#' @param e2 An element object from which e1 inherits
#'
#' @noRd
#'
combine_elements <- function(e1, e2) {

  # If e2 is NULL, nothing to inherit
  if (is.null(e2) || inherits(e1, "element_blank")) {
    return(e1)
  }

  # If e1 is NULL inherit everything from e2
  if (is.null(e1)) {
    return(e2)
  }

  # Inheritance of rel objects
  if (is.rel(e1)) {
    # Both e1 and e2 are rel, give product as another rel
    if (is.rel(e2)) {
      return(rel(unclass(e1) * unclass(e2)))
    }
    # If e2 is a unit/numeric, return modified unit/numeric
    # Note that unit objects are considered numeric
    if (is.numeric(e2) || is.unit(e2)) {
      return(unclass(e1) * e2)
    }
    return(e1)
  }

  # If neither of e1 or e2 are element_* objects, return e1
  if (!is_theme_element(e1) && !is_theme_element(e2)) {
    return(e1)
  }

  # If e2 is element_blank, and e1 inherits blank inherit everything from e2,
  # otherwise ignore e2
  if (inherits(e2, "element_blank")) {
    if (e1$inherit.blank) {
      return(e2)
    } else {
      return(e1)
    }
  }

  # If e1 has any NULL properties, inherit them from e2
  n <- names(e1)[vapply(e1, is.null, logical(1))]
  e1[n] <- e2[n]

  # Calculate relative sizes
  if (is.rel(e1$size)) {
    e1$size <- e2$size * unclass(e1$size)
  }

  # Calculate relative linewidth
  if (is.rel(e1$linewidth)) {
    e1$linewidth <- e2$linewidth * unclass(e1$linewidth)
  }

  # If e2 is 'richer' than e1, fill e2 with e1 parameters
  if (is.subclass(e2, e1)) {
    new <- defaults(e1, e2)
    e2[names(new)] <- new
    return(e2)
  }

  e1
}

is.subclass <- function(x, y) {
  inheritance <- inherits(x, class(y), which = TRUE)
  !any(inheritance == 0) && length(setdiff(class(x), class(y))) > 0
}


#' @export
#' @rdname is_tests
is_theme <- function(x) inherits(x, "theme")

#' @export
#' @rdname is_tests
#' @usage is.theme(x) # Deprecated
is.theme <- function(x) {
  deprecate_soft0("3.5.2", "is.theme()", "is_theme()")
  is_theme(x)
}

#' @export
`$.theme` <- function(x, ...) {
  .subset2(x, ...)
}

#' @export
print.theme <- function(x, ...) utils::str(x)
