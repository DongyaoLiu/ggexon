#' Label shared continuous x coordinates by facet
#'
#' `scale_x_facet_labels()` supplies continuous x-axis labels for each named
#' facet value. `facet` may be a bare column name or a one-element
#' column-name string, and `labels` is a named list whose entries provide the
#' labels for each facet. Labels are not inferred from geom data, so their
#' order must correspond to `breaks`. Add this helper after [ggplot2::facet_grid()]
#' or [ggplot2::facet_wrap()] with fixed x scales. All panels share the same
#' trained x range; only their axis labels differ. This preserves compatibility
#' with [ggplot2::coord_fixed()]. For a grid, `facet` must identify columns,
#' because each column shares an x axis. Unlisted facets retain numeric labels.
#' Wrapped facets show an x axis in every panel so each label mapping is visible.
#'
#' @param facet A bare facet column or a one-element string naming that column.
#' @param labels A non-empty named list of non-empty character vectors, one for
#'   each facet value.
#' @param breaks Numeric break positions. When `NULL`, uses sequential positions
#'   matching the length of the label vectors.
#' @param limits Optional increasing numeric x-axis limits. Like ordinary scale
#'   limits, these can remove data before layout. With [strip_scale_x()], omit
#'   this argument to use the complete slot template's computed range.
#' @param expand Expansion specification passed to [ggplot2::scale_x_continuous()].
#' @param ... Additional arguments passed to [ggplot2::scale_x_continuous()].
#'
#' @return A facet-label specification added to a ggplot2 or ggexon plot with `+`.
#' @export
#'
#' @examples
#' data <- data.frame(
#'   cluster = rep(c("A", "B"), each = 2),
#'   x = rep(c(1, 2), 2),
#'   y = 0
#' )
#' ggplot2::ggplot(data, ggplot2::aes(x, y)) +
#'   ggplot2::geom_point() +
#'   ggplot2::facet_grid(. ~ cluster) +
#'   scale_x_facet_labels(
#'     cluster,
#'     labels = list(A = c("A2", "A1"), B = c("B2", "B1"))
#'   ) +
#'   ggplot2::coord_fixed(ratio = 1)
#'
#' hox_labels <- list(
#'   HoxA = paste0("HOXA", 1:15),
#'   HoxB = paste0("HOXB", 1:15)
#' )
#' scale_x_facet_labels("cluster", labels = hox_labels, limits = c(0.5, 15.5))
scale_x_facet_labels <- function(facet,
                                 labels,
                                 breaks = NULL,
                                 limits = NULL,
                                 expand = ggplot2::expansion(mult = 0),
                                 ...) {
  facet_expr <- rlang::get_expr(rlang::enquo(facet))
  if (is.character(facet_expr) && length(facet_expr) == 1L &&
      !is.na(facet_expr) && nzchar(facet_expr)) {
    facet_expr <- rlang::sym(facet_expr)
  }
  if (!rlang::is_symbol(facet_expr)) {
    stop("`facet` must be a bare column name or a one-element column-name string.",
      call. = FALSE
    )
  }

  if (!is.list(labels) || length(labels) == 0L || is.null(names(labels))) {
    stop("`labels` must be a non-empty named list.", call. = FALSE)
  }
  label_names <- names(labels)
  if (anyNA(label_names) || any(!nzchar(label_names)) || anyDuplicated(label_names)) {
    stop("`labels` must have unique, non-empty, non-missing names.", call. = FALSE)
  }
  if (!all(vapply(labels, function(x) is.character(x) && length(x) > 0L, logical(1)))) {
    stop("Each `labels` entry must be a non-empty character vector.", call. = FALSE)
  }

  label_length <- length(labels[[1L]])
  if (!all(lengths(labels) == label_length)) {
    stop("All `labels` entries must have the same length.", call. = FALSE)
  }

  if (is.null(breaks)) {
    breaks <- seq_len(label_length)
  } else if (!is.numeric(breaks) || length(breaks) != label_length ||
             any(!is.finite(breaks)) || anyDuplicated(breaks)) {
    stop("`breaks` must be a finite, unique numeric vector with the same length as each label vector.",
      call. = FALSE
    )
  }

  if (!is.null(limits) &&
      (!is.numeric(limits) || length(limits) != 2L || any(!is.finite(limits)) ||
        limits[[1L]] >= limits[[2L]])) {
    stop("`limits` must be an increasing finite numeric vector of length two.",
      call. = FALSE
    )
  }

  structure(list(
    facet = rlang::as_string(facet_expr),
    labels = labels,
    breaks = breaks,
    scale = ggplot2::scale_x_continuous(limits = limits, expand = expand, ...)
  ), class = "ggexon_facet_labels_spec")
}

#' @export
ggplot_add.ggexon_facet_labels_spec <- function(object, plot, ...) {
  parent <- plot$facet
  if (!inherits(parent, "FacetGrid") && !inherits(parent, "FacetWrap")) {
    stop("`scale_x_facet_labels()` requires `facet_grid()` or `facet_wrap()` first.",
         call. = FALSE)
  }
  if (isTRUE(parent$params$free$x)) {
    stop("`scale_x_facet_labels()` requires fixed x scales; remove `scales = \"free_x\"`.",
         call. = FALSE)
  }
  plot <- plot + object$scale
  plot$facet <- ggplot2::ggproto(
    "FacetSharedXLabels", parent,
    compute_layout = function(self, data, params) {
      layout <- ggplot2::ggproto_parent(parent, self)$compute_layout(data, params)
      if (!object$facet %in% names(layout)) {
        stop("`facet` must name a faceting variable in the plot layout.", call. = FALSE)
      }
      if (inherits(parent, "FacetGrid")) {
        values <- split(as.character(layout[[object$facet]]), layout$COL)
        if (any(lengths(lapply(values, unique)) > 1L)) {
          stop("For `facet_grid()`, `facet` must identify columns, not rows.",
               call. = FALSE)
        }
      }
      # Separate presentation scales give each panel its own labels. Their
      # ranges are trained together below, so coordinates remain shared.
      layout$SCALE_X <- seq_len(nrow(layout))
      layout
    },
    init_scales = function(self, layout, x_scale = NULL, y_scale = NULL, params) {
      if (!is.null(x_scale) && x_scale$is_discrete()) {
        stop("`scale_x_facet_labels()` requires continuous x coordinates.", call. = FALSE)
      }
      scales <- ggplot2::ggproto_parent(parent, self)$init_scales(
        layout, x_scale, y_scale, params
      )
      if (!is.null(scales$x)) {
        for (i in seq_len(nrow(layout))) {
          labels <- object$labels[[as.character(layout[[object$facet]][i])]]
          if (!is.null(labels)) {
            scales$x[[layout$SCALE_X[i]]]$breaks <- object$breaks
            scales$x[[layout$SCALE_X[i]]]$labels <- labels
          }
        }
      }
      scales
    },
    train_scales = function(self, x_scales, y_scales, layout, data, params) {
      shared <- layout
      shared$SCALE_X <- 1L
      ggplot2::ggproto_parent(parent, self)$train_scales(
        x_scales[1L], y_scales, shared, data, params
      )
      if (length(x_scales) > 1L) {
        for (i in seq.int(2L, length(x_scales))) {
          x_scales[[i]]$train(x_scales[[1L]]$range$range)
        }
      }
    }
  )
  if (inherits(parent, "FacetWrap")) {
    plot$facet$params$draw_axes$x <- TRUE
    plot$facet$params$axis_labels$x <- TRUE
  }
  plot
}
