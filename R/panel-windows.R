#' Inspect effective annotation panel windows
#'
#' Returns a compact table describing the annotation-panel windows currently in
#' effect. For a ggexon plot object, this inspects the built plot so the result
#' reflects the actual panel selection and x ranges after stored layouts,
#' `set_panel_xlim()`, explicit `subset =`, and link-derived windows have all
#' been resolved.
#'
#' The returned columns distinguish between requested layout windows and the
#' ranges actually observed in built annotation data:
#'
#' - `chr`, `start`, `end`: window stored on the layout, usually from
#'   [`set_panel_xlim()`] or direct `SynLayout` edits.
#' - `observed_start`, `observed_end`: min/max `xmin` and `xmax` seen in the
#'   built annotation layers for that panel.
#'
#' These values are often different. For example, if you request
#' `start = 0, end = 5000` on a scaffold but the first annotated exon starts at
#' 1283 and the last one ends at 1871, then the observed range will be
#' `1283..1871` even though the effective panel window remains `0..5000`.
#' This helper is therefore useful both for checking stored panel windows and
#' for seeing how much of that window is actually occupied by annotation.
#'
#' @param x A ggexon plot object, [`SynSpecies`], or [`SynLayout`] object.
#'
#' @return A `data.frame` with one row per annotation panel.
#' @export
effective_panel_windows <- function(x) {
  if (inherits(x, "ggexon")) {
    return(.effective_panel_windows_ggexon(x))
  }
  if (methods::is(x, "SynSpecies")) {
    return(.effective_panel_windows_synspecies(x))
  }
  if (methods::is(x, "SynLayout")) {
    return(.effective_panel_windows_synlayout(x))
  }
  stop(
    "`effective_panel_windows()` expects a ggexon plot, SynSpecies, or SynLayout object.",
    call. = FALSE
  )
}

.effective_panel_windows_synlayout <- function(x) {
  panels <- syn_layout_panels(x)
  if (!is.data.frame(panels) || nrow(panels) == 0L) {
    return(data.frame())
  }

  panels <- .ensure_syn_layout_xlim_cols(panels)
  panel_type <- if ("panel_type" %in% names(panels)) {
    as.character(panels$panel_type)
  } else {
    rep("annotation", nrow(panels))
  }
  species_col <- if ("species" %in% names(panels)) {
    as.character(panels$species)
  } else {
    as.character(panels$track)
  }

  out <- data.frame(
    PANEL = as.integer(panels$PANEL),
    track = as.character(panels$track),
    panel_type = panel_type,
    individual = species_col,
    chr = as.character(panels$xlim_chr),
    start = as.numeric(panels$xlim_min),
    end = as.numeric(panels$xlim_max),
    stringsAsFactors = FALSE
  )
  out[out$panel_type == "annotation", , drop = FALSE]
}

.effective_panel_windows_synspecies <- function(x) {
  layout <- species_layout(x)
  if (is.null(layout)) {
    return(data.frame())
  }
  .effective_panel_windows_synlayout(layout)
}

.effective_panel_windows_ggexon <- function(x) {
  built <- ggexon_build(x)
  layout_df <- as.data.frame(built@layout$layout)
  if (!is.data.frame(layout_df) || nrow(layout_df) == 0L) {
    return(data.frame())
  }

  panel_type <- if ("panel_type" %in% names(layout_df)) {
    as.character(layout_df$panel_type)
  } else {
    rep("annotation", nrow(layout_df))
  }
  layout_df <- layout_df[panel_type == "annotation", , drop = FALSE]
  if (nrow(layout_df) == 0L) {
    return(data.frame())
  }

  annotation_layers <- Filter(function(df) {
    is.data.frame(df) &&
      all(c("PANEL", "track", "xmin", "xmax") %in% names(df)) &&
      !any(c("tspecies", "qspecies") %in% names(df))
  }, built@data)

  observed <- if (length(annotation_layers) == 0L) {
    data.frame(PANEL = integer(), track = character(), observed_start = numeric(), observed_end = numeric())
  } else {
    annotation_df <- dplyr::bind_rows(annotation_layers)
    annotation_df$PANEL <- as.integer(as.character(annotation_df$PANEL))
    dplyr::summarise(
      dplyr::group_by(annotation_df, .data$PANEL, .data$track),
      observed_start = min(.data$xmin, na.rm = TRUE),
      observed_end = max(.data$xmax, na.rm = TRUE),
      .groups = "drop"
    )
  }

  layout_df <- .ensure_syn_layout_xlim_cols(layout_df)
  out <- dplyr::left_join(
    data.frame(
      PANEL = as.integer(layout_df$PANEL),
      track = as.character(layout_df$track),
      panel_type = if ("panel_type" %in% names(layout_df)) as.character(layout_df$panel_type) else "annotation",
      individual = if ("species" %in% names(layout_df)) as.character(layout_df$species) else as.character(layout_df$track),
      chr = as.character(layout_df$xlim_chr),
      start = as.numeric(layout_df$xlim_min),
      end = as.numeric(layout_df$xlim_max),
      stringsAsFactors = FALSE
    ),
    observed,
    by = c("PANEL", "track")
  )

  context_windows <- list()
  for (layer in built@plot@layers) {
    windows <- layer$syn_plot_context$windows %||% NULL
    if (!is.null(windows) && length(windows) > 0L) {
      context_windows <- windows
      break
    }
  }
  if (length(context_windows) > 0L) {
    for (i in seq_len(nrow(out))) {
      window <- context_windows[[out$track[[i]]]] %||%
        context_windows[[out$individual[[i]]]] %||%
        NULL
      if (is.null(window)) {
        next
      }
      if (length(window$chr) == 1L) {
        out$chr[[i]] <- as.character(window$chr)
      }
      if (length(window$start) == 1L) {
        out$start[[i]] <- as.numeric(window$start)
      }
      if (length(window$end) == 1L) {
        out$end[[i]] <- as.numeric(window$end)
      }
    }
  }

  out
}
