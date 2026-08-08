#' X-only strip scale for gene tracks
#'
#' `strip_scale_x()` normalizes gene-tag or gene-box x coordinates. Its default
#' layout gives genes and intergenic gaps predictable visual widths. Homology
#' mode can compress species-specific local runs and translate tracks to align
#' the most conserved block against an explicit reference track. Exact-template
#' mode maps gene anchors to a complete synthetic `slot_order`, independently
#' of the raw genomic distances between genes.
#'
#' Once genomic x distances are stripped, gene-body overlap lanes are collapsed
#' to a single baseline per gene-tag layer. Outside labels remain coordinated
#' independently, so label lanes can still alternate above and below the shared
#' gene-body line.
#'
#' In exact-template mode, visible gene-box direction is inferred separately for
#' each panel and track from the rank correlation between genomic anchors and
#' template-slot positions. A track needs at least two distinct genomic anchors
#' in at least two distinct slots and a non-zero rank correlation. Otherwise
#' `strip_scale_x()` warns once per build and uses `+1` (no template-driven
#' direction reversal) for every underdetermined track.
#'
#' @param gene_gap_ratio Ratio of full gene visual width to intergenic gap
#'   visual width. When `NULL`, the ratio is estimated from the densest track.
#'   It is not used when `slot_order` is supplied.
#' @param align Alignment for level-1, non-homology tracks with fewer genes than
#'   the widest track.
#' @param reference_track Optional single reference track name for homology-aware
#'   layout. This is the preferred alias for `homo_align`.
#' @param slot_order Optional character vector defining exact shared comparison
#'   slots from left to right. Gene rows are matched through `slot`, falling
#'   back to `reference_gene` and then `gene_key`. The selected genomic anchor
#'   of every matching row is mapped to the center of its slot, so unoccupied
#'   template positions remain visible. Slot membership is supplied metadata,
#'   not an inference of one-to-one homology or evolutionary loss. This
#'   synthetic-template mode does not require, and cannot be combined with,
#'   `reference_track`.
#' @param homo_align `FALSE` for level-1 layout only, or a single character
#'   reference track name for homology-aware layout. `TRUE` is not supported.
#'   Prefer `reference_track` for new code.
#' @param gene_order Gene ordering strategy. `"genomic"` keeps each track in its
#'   native genomic order. `"reference"` orders query tracks by the resolved
#'   homolog order in `reference_track`, keeping unmapped local runs between the
#'   nearest surrounding reference-ordered homologs. When `slot_order` is
#'   supplied, that exact order governs the layout regardless of this setting.
#' @param species_specific_ratio Visual width of a species-specific gene or
#'   collapsed run relative to a homologous gene.
#' @param secondary_homology_ratio Visual width of a homologous off-track or
#'   duplicate gene relative to a primary visible homologous gene.
#' @param species_ratio Deprecated alias for `species_specific_ratio`.
#' @param collapse_contiguous_slot Logical; when `TRUE`, contiguous
#'   species-specific genes are compressed into one local run slot.
#' @param block_align Homology-mode track translation. `"conserved"` aligns the
#'   highest-support reference homology block by median center offset, without
#'   requiring query genes to appear in the same order. `"left"`, `"center"`,
#'   and `"right"` align each local track span to the reference span. `"none"`
#'   leaves level-2 local coordinates untranslated.
#' @param guide Strip-scale x-axis guide. `"range"` draws a simple per-track
#'   begin/end genomic bp range guide using the panel window when one is
#'   available, and otherwise the visible gene range; `"none"` suppresses the
#'   custom guide.
#' @param ... Arguments passed from the compatibility wrapper `strip_scale()` to
#'   `strip_scale_x()`.
#'
#' @return A ggexon strip-scale-x specification, added to a plot with `+`.
#' @export
strip_scale_x <- function(gene_gap_ratio = NULL,
                          align = c("left", "right", "center"),
                          reference_track = NULL,
                          homo_align = FALSE,
                          gene_order = c("genomic", "reference"),
                          species_specific_ratio = 0.5,
                          secondary_homology_ratio = 0.75,
                          species_ratio = NULL,
                          collapse_contiguous_slot = TRUE,
                          block_align = c("conserved", "left", "center", "right", "none"),
                          guide = c("range", "none"),
                          slot_order = NULL) {
  align <- match.arg(align)
  gene_order <- match.arg(gene_order)
  block_align <- match.arg(block_align)
  guide <- match.arg(guide)
  if (!is.null(slot_order)) {
    if (!is.character(slot_order) || length(slot_order) == 0L ||
        anyNA(slot_order) || any(!nzchar(slot_order)) ||
        anyDuplicated(slot_order)) {
      stop(
        "`slot_order` must be `NULL` or a unique character vector of non-empty keys.",
        call. = FALSE
      )
    }
    slot_order <- as.character(slot_order)
  }
  if (!is.null(reference_track)) {
    if (!is.character(reference_track) || length(reference_track) != 1L ||
        is.na(reference_track) || !nzchar(reference_track)) {
      stop("`reference_track` must be `NULL` or a single non-empty track name.", call. = FALSE)
    }
    if (!identical(homo_align, FALSE)) {
      stop("Supply only one of `reference_track` or `homo_align`.", call. = FALSE)
    }
    homo_align <- reference_track
  }
  if (!is.null(species_ratio)) {
    warning("`species_ratio` is deprecated; use `species_specific_ratio`.", call. = FALSE)
    species_specific_ratio <- species_ratio
  }
  if (!is.null(gene_gap_ratio)) {
    if (!is.numeric(gene_gap_ratio) || length(gene_gap_ratio) != 1L ||
        is.na(gene_gap_ratio) || gene_gap_ratio <= 0) {
      stop("`gene_gap_ratio` must be a single positive number or `NULL`.", call. = FALSE)
    }
  }
  if (!is.numeric(species_specific_ratio) || length(species_specific_ratio) != 1L ||
      is.na(species_specific_ratio) || species_specific_ratio <= 0 ||
      species_specific_ratio > 1) {
    stop("`species_specific_ratio` must be a number in (0, 1].", call. = FALSE)
  }
  if (!is.numeric(secondary_homology_ratio) || length(secondary_homology_ratio) != 1L ||
      is.na(secondary_homology_ratio) || secondary_homology_ratio <= 0 ||
      secondary_homology_ratio > 1) {
    stop("`secondary_homology_ratio` must be a number in (0, 1].", call. = FALSE)
  }
  if (!isTRUE(collapse_contiguous_slot) && !identical(collapse_contiguous_slot, FALSE)) {
    stop("`collapse_contiguous_slot` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  if (isTRUE(homo_align)) {
    stop(
      "`homo_align = TRUE` is no longer supported because homology alignment ",
      "requires an explicit reference track. Use `reference_track = \"<reference track>\"`.",
      call. = FALSE
    )
  }
  homo_active <- is.character(homo_align) && length(homo_align) == 1L &&
    !is.na(homo_align) && nzchar(homo_align)
  if (!identical(homo_align, FALSE) && !homo_active) {
    stop("`homo_align` must be `FALSE` or a single reference track name.", call. = FALSE)
  }
  template_active <- !is.null(slot_order)
  if (template_active && homo_active) {
    stop("Supply only one of `slot_order` and `reference_track` / `homo_align`.", call. = FALSE)
  }
  if (identical(gene_order, "reference") && !homo_active && !template_active) {
    stop(
      "`gene_order = \"reference\"` requires `slot_order`, `reference_track`, or `homo_align`.",
      call. = FALSE
    )
  }
  if (!homo_active && !template_active && !identical(block_align, "conserved")) {
    warning("`block_align` is ignored unless `reference_track` or `homo_align` is set.", call. = FALSE)
  }
  if (template_active && !identical(block_align, "conserved")) {
    warning("`block_align` is ignored when `slot_order` is supplied.", call. = FALSE)
  }

  structure(
    list(
      gene_gap_ratio = gene_gap_ratio,
      align = align,
      reference_track = if (homo_active) homo_align else NULL,
      slot_order = slot_order,
      template_active = template_active,
      homo_align = homo_align,
      homo_active = homo_active,
      gene_order = gene_order,
      species_specific_ratio = as.numeric(species_specific_ratio),
      secondary_homology_ratio = as.numeric(secondary_homology_ratio),
      collapse_contiguous_slot = isTRUE(collapse_contiguous_slot),
      block_align = block_align,
      guide = guide
    ),
    class = "ggexon_strip_scale_x_spec"
  )
}

strip_scale_x_guide_type <- function(strip_scale_spec) {
  strip_scale_spec$guide %||% "range"
}

#' @rdname strip_scale_x
#' @export
strip_scale <- function(...) {
  strip_scale_x(...)
}

#' @export
ggplot_add.ggexon_strip_scale_x_spec <- function(object, plot, ...) {
  if (!is_ggexon(plot)) {
    stop("`strip_scale_x()` can only be added to a ggexon plot.", call. = FALSE)
  }
  if (!is.null(plot@genomic_x_scale)) {
    stop("`strip_scale_x()` and `scale_x_ggexon_genomic()` are mutually exclusive.", call. = FALSE)
  }
  if (!is.null(plot@strip_scale)) {
    warning("`strip_scale_x()` is already set on this plot; replacing it.", call. = FALSE)
  }
  plot@strip_scale <- object
  plot
}

apply_strip_scale_x <- function(data, layers, strip_scale_spec, layout, plot) {
  gene_layers <- strip_scale_x_gene_layers(layers)
  tag_layers <- strip_scale_x_genetag_layers(layers)
  if (length(gene_layers) == 0L) {
    stop(
      "`strip_scale_x()` requires at least one `geom_genetag()` or `geom_genebox()` layer.",
      call. = FALSE
    )
  }

  tags <- strip_scale_x_collect_genetags(data, gene_layers)
  if (nrow(tags) == 0L) {
    stop("`strip_scale_x()` found no gene rows.", call. = FALSE)
  }
  ratio <- if (isTRUE(strip_scale_spec$template_active)) {
    1
  } else {
    strip_scale_x_resolve_ratio(tags, strip_scale_spec$gene_gap_ratio)
  }

  built <- if (isTRUE(strip_scale_spec$template_active)) {
    strip_scale_x_build_template_layout(tags, strip_scale_spec)
  } else if (isTRUE(strip_scale_spec$homo_active)) {
    strip_scale_x_build_homology_layout(tags, ratio, strip_scale_spec)
  } else {
    strip_scale_x_build_level1_layout(tags, ratio, strip_scale_spec$align)
  }

  data <- strip_scale_x_apply_transforms(data, gene_layers, built$transform)
  data <- strip_scale_x_flatten_genetag_lanes(data, layers, tag_layers)
  layout <- .strip_scale_force_fixed_x(layout, unique(as.character(built$transform$PANEL)))
  layout$strip_scale_x_transform <- built$transform
  layout$strip_scale_x_limits <- built$limits %||% NULL
  layout$strip_scale_x_axis_data <- if (identical(strip_scale_x_guide_type(strip_scale_spec), "range")) {
    strip_scale_x_range_axis_data(built$transform, layout = layout)
  } else {
    data.frame()
  }
  layout$strip_scale_x_spec <- strip_scale_spec
  layout$strip_scale_x_conserved_block <- built$conserved_reference_block %||% character()
  list(data = data, layout = layout, transforms = built$transform)
}

strip_scale_x_gene_layers <- function(layers) {
  unique(c(
    strip_scale_x_genetag_layers(layers),
    strip_scale_x_genebox_layers(layers)
  ))
}

strip_scale_x_genetag_layers <- function(layers) {
  which(vapply(layers, function(l) identical(l$geom, GeomGeneTag), logical(1)))
}

strip_scale_x_genebox_layers <- function(layers) {
  if (!exists("GeomGeneBox", inherits = TRUE)) return(integer())
  geom <- get("GeomGeneBox", inherits = TRUE)
  which(vapply(layers, function(l) identical(l$geom, geom), logical(1)))
}

strip_scale_x_collect_genetags <- function(data, tag_layers) {
  pieces <- list()
  for (layer_i in tag_layers) {
    df <- data[[layer_i]]
    if (!is.data.frame(df) || nrow(df) == 0L) next
    if (!"track" %in% names(df)) {
      stop("`strip_scale_x()` requires gene-layer data with a `track` column.", call. = FALSE)
    }
    if (!"PANEL" %in% names(df)) next
    if (!"genomic_xmin" %in% names(df) && "xmin" %in% names(df)) {
      df$genomic_xmin <- df$xmin
    }
    if (!"genomic_xmax" %in% names(df) && "xmax" %in% names(df)) {
      df$genomic_xmax <- df$xmax
    }
    if ((!"genomic_xmin" %in% names(df) || !"genomic_xmax" %in% names(df)) &&
        "x" %in% names(df)) {
      point_x <- if ("genomic_x" %in% names(df)) df$genomic_x else df$x
      df$genomic_xmin <- as.numeric(point_x) - 0.5
      df$genomic_xmax <- as.numeric(point_x) + 0.5
    }
    if (!all(c("genomic_xmin", "genomic_xmax") %in% names(df))) next
    if (!"gene_key" %in% names(df)) df$gene_key <- .genetag_gene_key(df)

    reference_gene <- if ("reference_gene" %in% names(df)) {
      as.character(df$reference_gene)
    } else {
      rep(NA_character_, nrow(df))
    }
    homology_hit <- if ("homology_hit" %in% names(df)) {
      strip_scale_x_logical_or_na(df$homology_hit)
    } else {
      rep(NA, nrow(df))
    }
    inferred_hit <- !is.na(reference_gene) & nzchar(reference_gene)
    homology_hit[is.na(homology_hit)] <- inferred_hit[is.na(homology_hit)]

    start <- pmin(as.numeric(df$genomic_xmin), as.numeric(df$genomic_xmax))
    end <- pmax(as.numeric(df$genomic_xmin), as.numeric(df$genomic_xmax))
    genomic_anchor <- if ("genomic_x" %in% names(df)) {
      as.numeric(df$genomic_x)
    } else if ("x" %in% names(df) && !all(is.na(df$x))) {
      as.numeric(df$x)
    } else {
      (start + end) / 2
    }
    out <- data.frame(
      layer = layer_i,
      row = seq_len(nrow(df)),
      PANEL = ggexon_panel_id(df$PANEL),
      track = as.character(df$track),
      gene_key = as.character(df$gene_key),
      label = if ("label" %in% names(df)) as.character(df$label) else as.character(df$gene_key),
      gene_id = if ("gene_id" %in% names(df)) as.character(df$gene_id) else NA_character_,
      gene_name = if ("gene_name" %in% names(df)) as.character(df$gene_name) else NA_character_,
      gene = if ("gene" %in% names(df)) as.character(df$gene) else NA_character_,
      reference_gene = reference_gene,
      reference_gene_name = if ("reference_gene_name" %in% names(df)) {
        as.character(df$reference_gene_name)
      } else {
        NA_character_
      },
      slot = if ("slot" %in% names(df)) as.character(df$slot) else NA_character_,
      anchor_mode = if ("anchor_mode" %in% names(df)) {
        as.character(df$anchor_mode)
      } else {
        NA_character_
      },
      homology_hit = homology_hit,
      genomic_start = start,
      genomic_end = end,
      genomic_anchor = genomic_anchor,
      stringsAsFactors = FALSE
    )
    keep <- is.finite(out$genomic_start) & is.finite(out$genomic_end) &
      out$genomic_end > out$genomic_start &
      is.finite(out$genomic_anchor) &
      !is.na(out$track) & nzchar(out$track) &
      !is.na(out$gene_key) & nzchar(out$gene_key)
    if (any(!keep)) {
      warning("`strip_scale_x()` dropped invalid gene row(s).", call. = FALSE)
    }
    out <- out[keep, , drop = FALSE]
    if (nrow(out) > 0L) pieces[[length(pieces) + 1L]] <- out
  }
  if (length(pieces) == 0L) return(data.frame())
  out <- dplyr::bind_rows(pieces)
  out <- out[order(out$PANEL, out$track, out$genomic_start, out$genomic_end, out$row), , drop = FALSE]
  rownames(out) <- NULL
  out
}

strip_scale_x_logical_or_na <- function(x) {
  if (is.logical(x)) return(x)
  x_chr <- base::tolower(as.character(x))
  out <- rep(NA, length(x_chr))
  out[x_chr %in% c("true", "t", "1", "yes", "y")] <- TRUE
  out[x_chr %in% c("false", "f", "0", "no", "n")] <- FALSE
  out
}

strip_scale_x_resolve_ratio <- function(tags, gene_gap_ratio = NULL) {
  if (!is.null(gene_gap_ratio)) return(as.numeric(gene_gap_ratio))
  group_key <- paste(tags$PANEL, tags$track, sep = "\r")
  counts <- sort(table(group_key), decreasing = TRUE)
  if (length(counts) == 0L) return(3)
  densest <- names(counts)[[1L]]
  genes <- tags[group_key == densest, , drop = FALSE]
  genes <- genes[order(genes$genomic_start, genes$genomic_end), , drop = FALSE]
  gene_widths <- genes$genomic_end - genes$genomic_start
  gene_widths <- gene_widths[is.finite(gene_widths) & gene_widths > 0]
  gap_widths <- pmax(genes$genomic_start[-1L] - genes$genomic_end[-nrow(genes)], 0)
  gap_widths <- gap_widths[is.finite(gap_widths) & gap_widths > 0]
  med_gene <- stats::median(gene_widths, na.rm = TRUE)
  med_gap <- stats::median(gap_widths, na.rm = TRUE)
  if (!is.finite(med_gene) || med_gene <= 0) return(3)
  if (!is.finite(med_gap) || med_gap <= 0) return(max(med_gene / 1000, 3))
  max(med_gene / med_gap, 0.1)
}

strip_scale_x_build_template_layout <- function(tags, spec) {
  slot_order <- as.character(spec$slot_order)
  resolved_slot <- strip_scale_x_resolve_template_slot(tags)

  missing_slot <- is.na(resolved_slot) | !nzchar(resolved_slot)
  if (any(missing_slot)) {
    bad <- unique(tags$gene_key[missing_slot])
    stop(
      "`strip_scale_x(slot_order = ...)` could not resolve a slot for gene row(s): ",
      paste(utils::head(bad, 8L), collapse = ", "),
      if (length(bad) > 8L) ", ..." else "",
      ". Supply `slot`, `reference_gene`, or a matching `gene_key`.",
      call. = FALSE
    )
  }

  unknown <- unique(resolved_slot[!resolved_slot %in% slot_order])
  if (length(unknown) > 0L) {
    stop(
      "Gene row slot(s) are absent from `slot_order`: ",
      paste(utils::head(unknown, 8L), collapse = ", "),
      if (length(unknown) > 8L) ", ..." else "",
      ".",
      call. = FALSE
    )
  }

  slot_index <- match(resolved_slot, slot_order)
  plot_anchor <- as.numeric(slot_index)
  genomic_width <- tags$genomic_end - tags$genomic_start
  # The point-like gene box is positioned directly at `plot_anchor`. This
  # compact interval mapping is retained so geom_genetag() can also participate
  # without allowing its body to close an empty template slot.
  visual_width <- 0.4
  slope <- visual_width / genomic_width
  plot_start <- plot_anchor + (tags$genomic_start - tags$genomic_anchor) * slope
  plot_end <- plot_anchor + (tags$genomic_end - tags$genomic_anchor) * slope

  transform <- dplyr::bind_rows(lapply(seq_len(nrow(tags)), function(i) {
    strip_scale_x_transform_row(
      PANEL = tags$PANEL[[i]],
      track = tags$track[[i]],
      local_slot_id = slot_index[[i]],
      global_slot_id = resolved_slot[[i]],
      slot_type = "template_slot",
      visual_class = "template_slot",
      gene_key = tags$gene_key[[i]],
      label = tags$label[[i]],
      reference_gene = resolved_slot[[i]],
      is_anchor = TRUE,
      homology_anchor = TRUE,
      members = tags$gene_key[[i]],
      source_keys = paste(tags$layer[[i]], tags$row[[i]], sep = ":"),
      genomic_start = tags$genomic_start[[i]],
      genomic_end = tags$genomic_end[[i]],
      plot_start_raw = plot_start[[i]],
      plot_end_raw = plot_end[[i]],
      region_type = "gene",
      genomic_anchor = tags$genomic_anchor[[i]],
      plot_anchor_raw = plot_anchor[[i]],
      slot = resolved_slot[[i]]
    )
  }))
  transform <- strip_scale_x_template_directions(transform)

  built <- strip_scale_x_finalize_layout(transform)
  built$limits <- c(0.5, length(slot_order) + 0.5)
  built$template <- data.frame(
    slot = slot_order,
    slot_index = seq_along(slot_order),
    plot_anchor = seq_along(slot_order),
    stringsAsFactors = FALSE
  )
  built
}

strip_scale_x_template_directions <- function(transform) {
  transform$strip_x_direction <- 1
  groups <- split(
    seq_len(nrow(transform)),
    paste(transform$PANEL, transform$track, sep = "\r"),
    drop = TRUE
  )
  underdetermined <- character()
  for (group_name in names(groups)) {
    idx <- groups[[group_name]]
    genomic <- transform$genomic_anchor[idx]
    plotted <- transform$plot_anchor_raw[idx]
    usable <- is.finite(genomic) & is.finite(plotted)
    genomic <- genomic[usable]
    plotted <- plotted[usable]
    direction <- 1
    reason <- NULL
    if (length(unique(genomic)) < 2L || length(unique(plotted)) < 2L) {
      reason <- "fewer than two distinct anchors or slots"
    } else {
      correlation <- suppressWarnings(stats::cor(genomic, plotted, method = "spearman"))
      if (!is.finite(correlation) || abs(correlation) <= sqrt(.Machine$double.eps)) {
        reason <- "zero or undefined rank correlation"
      } else {
        direction <- sign(correlation)
      }
    }
    if (!is.null(reason)) {
      track <- as.character(transform$track[idx[[1L]]])
      panel <- as.character(transform$PANEL[idx[[1L]]])
      underdetermined <- c(
        underdetermined,
        paste0("'", track, "' (panel ", panel, ": ", reason, ")")
      )
    }
    transform$strip_x_direction[idx] <- direction
  }
  if (length(underdetermined) > 0L) {
    shown <- utils::head(underdetermined, 8L)
    warning(
      "`strip_scale_x(slot_order = ...)` could not infer template direction for ",
      length(underdetermined), " panel/track group(s): ",
      paste(shown, collapse = ", "),
      if (length(underdetermined) > length(shown)) ", ..." else "",
      ". Using +1 (no template-driven direction reversal) for those groups.",
      call. = FALSE
    )
  }
  transform
}

strip_scale_x_resolve_template_slot <- function(tags) {
  out <- rep(NA_character_, nrow(tags))
  for (column in c("slot", "reference_gene", "gene_key")) {
    if (!column %in% names(tags)) next
    unresolved <- is.na(out) | !nzchar(out)
    candidate <- as.character(tags[[column]])
    use <- unresolved & !is.na(candidate) & nzchar(candidate)
    out[use] <- candidate[use]
  }
  out
}

strip_scale_x_build_level1_layout <- function(tags, ratio, align = "left") {
  g <- ratio
  p <- 1
  groups <- split(tags, paste(tags$PANEL, tags$track, sep = "\r"), drop = TRUE)
  max_genes <- max(vapply(groups, nrow, integer(1)))
  total_span <- max_genes * g + max(max_genes - 1L, 0L) * p
  transforms <- list()

  for (group_name in names(groups)) {
    group <- groups[[group_name]]
    group <- group[order(group$genomic_start, group$genomic_end, group$row), , drop = FALSE]
    track_span <- nrow(group) * g + max(nrow(group) - 1L, 0L) * p
    offset <- switch(
      align,
      left = 0,
      right = total_span - track_span,
      center = (total_span - track_span) / 2
    )
    transforms[[length(transforms) + 1L]] <- strip_scale_x_track_layout(
      group,
      gene_width = g,
      gap_width = p,
      widths = rep(g, nrow(group)),
      slot_types = rep("gene", nrow(group)),
      is_anchor = rep(FALSE, nrow(group)),
      reference_gene = rep(NA_character_, nrow(group)),
      start_offset = max(offset, 0)
    )
  }
  transform <- dplyr::bind_rows(transforms)
  strip_scale_x_finalize_layout(transform)
}

strip_scale_x_build_homology_layout <- function(tags, ratio, spec) {
  reference_track <- spec$homo_align
  if (!reference_track %in% tags$track) {
    stop("Reference track '", reference_track, "' not found in `geom_genetag()` tracks.", call. = FALSE)
  }

  g <- ratio
  p <- 1
  species_width <- g * spec$species_specific_ratio
  secondary_width <- g * spec$secondary_homology_ratio
  groups <- split(tags, paste(tags$PANEL, tags$track, sep = "\r"), drop = TRUE)
  ref_tags <- tags[tags$track == reference_track, , drop = FALSE]
  ref_alias <- strip_scale_x_reference_aliases(ref_tags)

  ref_transforms <- list()
  for (group_name in names(groups)) {
    group <- groups[[group_name]]
    if (unique(group$track) %in% reference_track) {
      group <- group[order(group$genomic_start, group$genomic_end, group$row), , drop = FALSE]
      ref_transforms[[length(ref_transforms) + 1L]] <- strip_scale_x_track_layout(
        group,
        gene_width = g,
        gap_width = p,
        widths = rep(g, nrow(group)),
        slot_types = rep("homologous_visible_primary", nrow(group)),
        is_anchor = rep(TRUE, nrow(group)),
        reference_gene = group$gene_key,
        visual_classes = rep("homologous_visible_primary", nrow(group)),
        homology_anchor = rep(TRUE, nrow(group)),
        start_offset = 0
      )
    }
  }
  reference_transform <- dplyr::bind_rows(ref_transforms)
  reference_centers <- strip_scale_x_reference_centers(reference_transform)
  reference_span <- strip_scale_x_span(reference_transform)
  conserved_reference_block <- strip_scale_x_conserved_reference_block(
    groups = groups,
    reference_track = reference_track,
    ref_alias = ref_alias,
    reference_centers = reference_centers
  )

  transforms <- ref_transforms
  for (group_name in names(groups)) {
    group <- groups[[group_name]]
    track_name <- unique(group$track)
    if (length(track_name) != 1L || identical(track_name, reference_track)) next
    group <- group[order(group$genomic_start, group$genomic_end, group$row), , drop = FALSE]
    resolved <- strip_scale_x_resolve_group_reference_gene(group, ref_alias)
    ordered <- strip_scale_x_order_homology_group(
      group = group,
      resolved_reference = resolved,
      reference_centers = reference_centers,
      gene_order = spec$gene_order %||% "genomic"
    )
    group <- ordered$group
    resolved <- ordered$resolved_reference
    class <- strip_scale_x_homology_class(group, resolved)
    transforms[[length(transforms) + 1L]] <- strip_scale_x_homology_track_layout(
      group = group,
      resolved_reference = resolved,
      visual_class = class$visual_class,
      homology_anchor = class$homology_anchor,
      gene_width = g,
      gap_width = p,
      species_width = species_width,
      secondary_width = secondary_width,
      collapse_contiguous_slot = spec$collapse_contiguous_slot,
      block_align = spec$block_align,
      reference_centers = reference_centers,
      reference_span = reference_span,
      conserved_reference_block = conserved_reference_block
    )
  }

  transform <- dplyr::bind_rows(transforms)
  built <- strip_scale_x_finalize_layout(transform)
  built$conserved_reference_block <- conserved_reference_block
  built
}

strip_scale_x_order_homology_group <- function(group,
                                               resolved_reference,
                                               reference_centers,
                                               gene_order = "genomic") {
  if (!identical(gene_order, "reference") || nrow(group) <= 1L) {
    return(list(group = group, resolved_reference = resolved_reference))
  }

  ref_rank <- match(resolved_reference, reference_centers$gene_key)
  if (!any(!is.na(ref_rank))) {
    return(list(group = group, resolved_reference = resolved_reference))
  }

  order_key <- as.numeric(ref_rank)
  native_index <- seq_len(nrow(group))
  unmapped <- is.na(ref_rank)

  if (any(unmapped)) {
    run_ids <- cumsum(c(TRUE, diff(unmapped) != 0L))
    for (run_id in unique(run_ids[unmapped])) {
      run_idx <- which(run_ids == run_id)
      prev_candidates <- which(!unmapped & native_index < min(run_idx))
      next_candidates <- which(!unmapped & native_index > max(run_idx))

      has_prev <- length(prev_candidates) > 0L
      has_next <- length(next_candidates) > 0L
      if (has_prev && has_next) {
        prev_idx <- max(prev_candidates)
        next_idx <- min(next_candidates)
        base <- min(ref_rank[[prev_idx]], ref_rank[[next_idx]]) + 0.5
      } else if (has_prev) {
        prev_idx <- max(prev_candidates)
        base <- ref_rank[[prev_idx]] + 0.5
      } else if (has_next) {
        next_idx <- min(next_candidates)
        base <- ref_rank[[next_idx]] - 0.5
      } else {
        base <- max(ref_rank, na.rm = TRUE) + 0.5
      }

      order_key[run_idx] <- base
    }
  }

  order_idx <- order(order_key, native_index, na.last = TRUE)
  list(
    group = group[order_idx, , drop = FALSE],
    resolved_reference = resolved_reference[order_idx]
  )
}

strip_scale_x_homology_class <- function(group, resolved_reference) {
  n <- nrow(group)
  homology_hit <- group$homology_hit %in% TRUE
  visible <- !is.na(resolved_reference) & nzchar(resolved_reference)
  visual_class <- rep("species_specific", n)
  visual_class[homology_hit & !visible] <- "homologous_offtrack"

  homology_anchor <- rep(FALSE, n)
  seen <- character()
  for (i in seq_len(n)) {
    if (!isTRUE(visible[[i]])) next
    ref_key <- resolved_reference[[i]]
    if (ref_key %in% seen) {
      visual_class[[i]] <- "homologous_visible_duplicate"
      next
    }
    visual_class[[i]] <- "homologous_visible_primary"
    homology_anchor[[i]] <- TRUE
    seen <- c(seen, ref_key)
  }

  list(
    visual_class = visual_class,
    homology_anchor = homology_anchor
  )
}

strip_scale_x_track_layout <- function(group,
                                       gene_width,
                                       gap_width,
                                       widths,
                                       slot_types,
                                       is_anchor,
                                       reference_gene,
                                       visual_classes = slot_types,
                                       homology_anchor = is_anchor,
                                       start_offset = 0) {
  slots <- vector("list", nrow(group))
  for (i in seq_len(nrow(group))) {
    slots[[i]] <- list(
      rows = i,
      genomic_start = group$genomic_start[[i]],
      genomic_end = group$genomic_end[[i]],
      width = widths[[i]],
      slot_type = slot_types[[i]],
      visual_class = visual_classes[[i]],
      gene_key = group$gene_key[[i]],
      label = group$label[[i]],
      reference_gene = reference_gene[[i]],
      is_anchor = is_anchor[[i]],
      homology_anchor = homology_anchor[[i]]
    )
  }
  strip_scale_x_slots_to_transform(group, slots, gap_width, start_offset)
}

strip_scale_x_homology_track_layout <- function(group,
                                                resolved_reference,
                                                visual_class,
                                                homology_anchor,
                                                gene_width,
                                                gap_width,
                                                species_width,
                                                secondary_width,
                                                collapse_contiguous_slot,
                                                block_align,
                                                reference_centers,
                                                reference_span,
                                                conserved_reference_block = character()) {
  slots <- list()
  i <- 1L
  while (i <= nrow(group)) {
    if (isTRUE(collapse_contiguous_slot) && identical(visual_class[[i]], "species_specific")) {
      run_start <- i
      while (i <= nrow(group) && identical(visual_class[[i]], "species_specific")) i <- i + 1L
      run_idx <- seq.int(run_start, i - 1L)
      slots[[length(slots) + 1L]] <- list(
        rows = run_idx,
        genomic_start = min(group$genomic_start[run_idx], na.rm = TRUE),
        genomic_end = max(group$genomic_end[run_idx], na.rm = TRUE),
        width = species_width,
        slot_type = "species_specific_run",
        visual_class = "species_specific",
        gene_key = paste(group$gene_key[run_idx], collapse = ","),
        label = paste(group$label[run_idx], collapse = ","),
        reference_gene = NA_character_,
        is_anchor = FALSE,
        homology_anchor = FALSE
      )
      next
    }

    class <- visual_class[[i]]
    width <- switch(
      class,
      homologous_visible_primary = gene_width,
      homologous_visible_duplicate = secondary_width,
      homologous_offtrack = secondary_width,
      species_specific = species_width,
      species_width
    )
    ref_gene <- switch(
      class,
      homologous_visible_primary = resolved_reference[[i]],
      homologous_visible_duplicate = resolved_reference[[i]],
      homologous_offtrack = group$reference_gene[[i]],
      species_specific = NA_character_,
      NA_character_
    )
    slots[[length(slots) + 1L]] <- list(
      rows = i,
      genomic_start = group$genomic_start[[i]],
      genomic_end = group$genomic_end[[i]],
      width = width,
      slot_type = class,
      visual_class = class,
      gene_key = group$gene_key[[i]],
      label = group$label[[i]],
      reference_gene = ref_gene,
      is_anchor = homology_anchor[[i]],
      homology_anchor = homology_anchor[[i]]
    )
    i <- i + 1L
  }

  transform <- strip_scale_x_slots_to_transform(group, slots, gap_width, start_offset = 0)
  local_span <- strip_scale_x_span(transform)
  track_offset <- switch(
    block_align,
    none = 0,
    left = reference_span$min - local_span$min,
    center = ((reference_span$min + reference_span$max) / 2) - ((local_span$min + local_span$max) / 2),
    right = reference_span$max - local_span$max,
    conserved = strip_scale_x_conserved_offset(
      transform,
      group,
      homology_anchor,
      resolved_reference,
      reference_centers,
      conserved_reference_block = conserved_reference_block
    )
  )
  transform$track_offset <- track_offset
  transform
}

strip_scale_x_slots_to_transform <- function(group, slots, gap_width, start_offset = 0) {
  pieces <- list()
  current <- start_offset
  panel <- group$PANEL[[1L]]
  track <- group$track[[1L]]
  for (slot_i in seq_along(slots)) {
    slot <- slots[[slot_i]]
    width <- slot$width
    pieces[[length(pieces) + 1L]] <- strip_scale_x_transform_row(
      PANEL = panel,
      track = track,
      local_slot_id = slot_i,
      global_slot_id = if (isTRUE(slot$is_anchor)) slot$reference_gene else paste(track, slot_i, sep = "::"),
      slot_type = slot$slot_type,
      visual_class = slot$visual_class %||% slot$slot_type,
      gene_key = slot$gene_key,
      label = slot$label,
      reference_gene = slot$reference_gene,
      is_anchor = slot$is_anchor,
      homology_anchor = slot$homology_anchor %||% slot$is_anchor,
      members = paste(group$gene_key[slot$rows], collapse = ","),
      source_keys = paste(group$layer[slot$rows], group$row[slot$rows], sep = ":", collapse = ","),
      genomic_start = slot$genomic_start,
      genomic_end = slot$genomic_end,
      plot_start_raw = current,
      plot_end_raw = current + width,
      region_type = "gene"
    )
    current <- current + width

    if (slot_i < length(slots)) {
      next_slot <- slots[[slot_i + 1L]]
      gap_start <- slot$genomic_end
      gap_end <- next_slot$genomic_start
      if (is.finite(gap_start) && is.finite(gap_end) && gap_end > gap_start) {
        pieces[[length(pieces) + 1L]] <- strip_scale_x_transform_row(
          PANEL = panel,
          track = track,
          local_slot_id = slot_i,
          global_slot_id = paste(track, "gap", slot_i, sep = "::"),
          slot_type = "gap",
          visual_class = "gap",
          gene_key = NA_character_,
          label = NA_character_,
          reference_gene = NA_character_,
          is_anchor = FALSE,
          homology_anchor = FALSE,
          members = NA_character_,
          source_keys = NA_character_,
          genomic_start = gap_start,
          genomic_end = gap_end,
          plot_start_raw = current,
          plot_end_raw = current + gap_width,
          region_type = "gap"
        )
      }
      current <- current + gap_width
    }
  }
  out <- dplyr::bind_rows(pieces)
  out$track_offset <- out$track_offset %||% 0
  out
}

strip_scale_x_transform_row <- function(PANEL,
                                        track,
                                        local_slot_id,
                                        global_slot_id,
                                        slot_type,
                                        visual_class,
                                        gene_key,
                                        label,
                                        reference_gene,
                                        is_anchor,
                                        homology_anchor,
                                        members,
                                        source_keys,
                                        genomic_start,
                                        genomic_end,
                                        plot_start_raw,
                                        plot_end_raw,
                                        region_type,
                                        genomic_anchor = NULL,
                                        plot_anchor_raw = NULL,
                                        slot = NA_character_,
                                        strip_x_direction = 1) {
  width <- genomic_end - genomic_start
  if (is.null(genomic_anchor)) genomic_anchor <- (genomic_start + genomic_end) / 2
  if (is.null(plot_anchor_raw)) {
    plot_anchor_raw <- plot_start_raw +
      (genomic_anchor - genomic_start) *
      if (is.finite(width) && width > 0) (plot_end_raw - plot_start_raw) / width else 0
  }
  data.frame(
    PANEL = PANEL,
    track = track,
    local_slot_id = local_slot_id,
    global_slot_id = global_slot_id,
    slot_type = slot_type,
    visual_class = visual_class,
    gene_key = gene_key,
    label = label,
    reference_gene = reference_gene,
    is_anchor = isTRUE(is_anchor),
    homology_anchor = isTRUE(homology_anchor),
    members = members,
    source_keys = source_keys,
    genomic_start = genomic_start,
    genomic_end = genomic_end,
    genomic_anchor = genomic_anchor,
    plot_anchor_raw = plot_anchor_raw,
    plot_anchor = plot_anchor_raw,
    slot = slot,
    strip_x_direction = strip_x_direction,
    plot_start_raw = plot_start_raw,
    plot_end_raw = plot_end_raw,
    slope = if (is.finite(width) && width > 0) (plot_end_raw - plot_start_raw) / width else NA_real_,
    region_type = region_type,
    track_offset = 0,
    global_offset = 0,
    plot_start = plot_start_raw,
    plot_end = plot_end_raw,
    stringsAsFactors = FALSE
  )
}

strip_scale_x_reference_aliases <- function(ref_tags) {
  ref_counts <- table(ref_tags$gene_key)
  duplicate_ref <- names(ref_counts)[ref_counts > 1L]
  alias_df <- dplyr::bind_rows(lapply(seq_len(nrow(ref_tags)), function(i) {
    values <- unique(stats::na.omit(c(
      ref_tags$gene_key[[i]],
      ref_tags$gene_id[[i]],
      ref_tags$gene_name[[i]],
      ref_tags$gene[[i]],
      ref_tags$label[[i]],
      ref_tags$reference_gene[[i]],
      ref_tags$reference_gene_name[[i]]
    )))
    values <- values[nzchar(values)]
    if (length(values) == 0L) return(NULL)
    data.frame(alias = values, gene_key = ref_tags$gene_key[[i]], stringsAsFactors = FALSE)
  }))
  if (is.null(alias_df) || nrow(alias_df) == 0L) return(stats::setNames(character(), character()))
  alias_df <- alias_df[!alias_df$gene_key %in% duplicate_ref, , drop = FALSE]
  split_keys <- split(alias_df$gene_key, alias_df$alias)
  usable <- vapply(split_keys, function(x) length(unique(x)) == 1L, logical(1))
  stats::setNames(vapply(split_keys[usable], function(x) unique(x)[[1L]], character(1)), names(split_keys)[usable])
}

strip_scale_x_resolve_reference_gene <- function(reference_gene, ref_alias) {
  ref <- as.character(reference_gene)
  out <- rep(NA_character_, length(ref))
  ok <- !is.na(ref) & nzchar(ref) & ref %in% names(ref_alias)
  out[ok] <- unname(ref_alias[ref[ok]])
  out
}

strip_scale_x_resolve_group_reference_gene <- function(group, ref_alias) {
  resolved <- strip_scale_x_resolve_reference_gene(group$reference_gene, ref_alias)
  unresolved <- is.na(resolved) | !nzchar(resolved)
  if (!any(unresolved)) {
    return(resolved)
  }

  fallback_cols <- intersect(c("gene_key", "gene_id", "gene", "label"), names(group))
  for (col in fallback_cols) {
    unresolved <- is.na(resolved) | !nzchar(resolved)
    if (!any(unresolved)) {
      break
    }
    fallback <- strip_scale_x_resolve_reference_gene(group[[col]], ref_alias)
    use_fallback <- unresolved & !is.na(fallback) & nzchar(fallback)
    resolved[use_fallback] <- fallback[use_fallback]
  }
  resolved
}

strip_scale_x_reference_centers <- function(reference_transform) {
  ref <- reference_transform[reference_transform$region_type == "gene" &
    !is.na(reference_transform$gene_key) & nzchar(reference_transform$gene_key), , drop = FALSE]
  ref <- ref[order(ref$plot_start_raw, ref$plot_end_raw), , drop = FALSE]
  data.frame(
    gene_key = ref$gene_key,
    ref_order = seq_len(nrow(ref)),
    ref_center = (ref$plot_start_raw + ref$plot_end_raw) / 2,
    ref_start = ref$genomic_start,
    ref_end = ref$genomic_end,
    stringsAsFactors = FALSE
  )
}

strip_scale_x_conserved_reference_block <- function(groups,
                                                    reference_track,
                                                    ref_alias,
                                                    reference_centers) {
  if (nrow(reference_centers) == 0L) return(character())

  support <- stats::setNames(rep(0L, nrow(reference_centers)), reference_centers$gene_key)
  for (group_name in names(groups)) {
    group <- groups[[group_name]]
    track_name <- unique(group$track)
    if (length(track_name) != 1L) next
    if (identical(track_name, reference_track)) {
      track_refs <- unique(group$gene_key)
    } else {
      group <- group[order(group$genomic_start, group$genomic_end, group$row), , drop = FALSE]
      resolved <- strip_scale_x_resolve_group_reference_gene(group, ref_alias)
      class <- strip_scale_x_homology_class(group, resolved)
      track_refs <- resolved[class$homology_anchor]
    }
    track_refs <- unique(track_refs[!is.na(track_refs) & nzchar(track_refs) & track_refs %in% names(support)])
    support[track_refs] <- support[track_refs] + 1L
  }

  if (!any(support > 1L)) return(character())
  max_support <- max(support, na.rm = TRUE)
  is_best <- support[reference_centers$gene_key] == max_support
  best_idx <- which(is_best)
  if (length(best_idx) == 0L) return(character())

  runs <- split(best_idx, cumsum(c(TRUE, diff(best_idx) != 1L)))
  scores <- do.call(rbind, lapply(runs, function(idx) {
    rows <- reference_centers[idx, , drop = FALSE]
    data.frame(
      n = nrow(rows),
      ref_span = max(rows$ref_end, na.rm = TRUE) - min(rows$ref_start, na.rm = TRUE),
      left_order = min(rows$ref_order, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  best <- order(-scores$n, -scores$ref_span, scores$left_order)[[1L]]
  reference_centers[runs[[best]], "gene_key", drop = TRUE]
}

strip_scale_x_first_reference_hits <- function(resolved_reference) {
  seen <- character()
  hits <- character()
  for (ref_key in resolved_reference) {
    if (is.na(ref_key) || !nzchar(ref_key) || ref_key %in% seen) next
    hits <- c(hits, ref_key)
    seen <- c(seen, ref_key)
  }
  hits
}

strip_scale_x_conserved_offset <- function(transform,
                                           group,
                                           is_anchor,
                                           resolved_reference,
                                           reference_centers,
                                           conserved_reference_block = character()) {
  gene_rows <- transform[transform$region_type == "gene", , drop = FALSE]
  anchor_rows <- gene_rows[gene_rows$is_anchor %in% TRUE, , drop = FALSE]
  if (nrow(anchor_rows) == 0L) return(0)
  anchor_rows$query_center <- (anchor_rows$plot_start_raw + anchor_rows$plot_end_raw) / 2
  anchor_rows$gene_order <- match(anchor_rows$gene_key, group$gene_key)
  ref_match <- match(anchor_rows$reference_gene, reference_centers$gene_key)
  anchor_rows$ref_center <- reference_centers$ref_center[ref_match]
  anchor_rows$ref_order <- reference_centers$ref_order[ref_match]
  anchor_rows$ref_start <- reference_centers$ref_start[ref_match]
  anchor_rows$ref_end <- reference_centers$ref_end[ref_match]
  anchor_rows <- anchor_rows[is.finite(anchor_rows$ref_center), , drop = FALSE]
  if (nrow(anchor_rows) == 0L) return(0)

  block_rows <- anchor_rows[anchor_rows$reference_gene %in% conserved_reference_block, , drop = FALSE]
  if (nrow(block_rows) > 0L) {
    return(stats::median(block_rows$ref_center - block_rows$query_center, na.rm = TRUE))
  }

  blocks <- list()
  current <- integer()
  for (i in seq_len(nrow(group))) {
    if (!isTRUE(is_anchor[[i]])) {
      if (length(current) > 0L) blocks[[length(blocks) + 1L]] <- current
      current <- integer()
      next
    }
    row_idx <- match(group$gene_key[[i]], anchor_rows$gene_key)
    if (is.na(row_idx)) next
    current <- c(current, row_idx)
  }
  if (length(current) > 0L) blocks[[length(blocks) + 1L]] <- current
  if (length(blocks) == 0L) return(0)

  scores <- do.call(rbind, lapply(blocks, function(idx) {
    rows <- anchor_rows[idx, , drop = FALSE]
    data.frame(
      n = nrow(rows),
      ref_span = max(rows$ref_end, na.rm = TRUE) - min(rows$ref_start, na.rm = TRUE),
      left_order = min(rows$ref_order, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  best <- order(-scores$n, -scores$ref_span, scores$left_order)[[1L]]
  best_rows <- anchor_rows[blocks[[best]], , drop = FALSE]
  stats::median(best_rows$ref_center - best_rows$query_center, na.rm = TRUE)
}

strip_scale_x_span <- function(transform) {
  rows <- transform[transform$region_type == "gene", , drop = FALSE]
  if (nrow(rows) == 0L) rows <- transform
  list(
    min = min(rows$plot_start_raw, rows$plot_end_raw, na.rm = TRUE),
    max = max(rows$plot_start_raw, rows$plot_end_raw, na.rm = TRUE)
  )
}

strip_scale_x_finalize_layout <- function(transform) {
  if (nrow(transform) == 0L) {
    return(list(transform = transform, axis_data = data.frame()))
  }
  transform$plot_start <- transform$plot_start_raw + transform$track_offset
  transform$plot_end <- transform$plot_end_raw + transform$track_offset
  if ("plot_anchor_raw" %in% names(transform)) {
    transform$plot_anchor <- transform$plot_anchor_raw + transform$track_offset
  }
  min_x <- min(transform$plot_start, transform$plot_end, na.rm = TRUE)
  global_offset <- if (is.finite(min_x) && min_x < 0) -min_x else 0
  transform$global_offset <- global_offset
  transform$plot_start <- transform$plot_start + global_offset
  transform$plot_end <- transform$plot_end + global_offset
  if ("plot_anchor" %in% names(transform)) {
    transform$plot_anchor <- transform$plot_anchor + global_offset
  }

  axis_data <- strip_scale_x_range_axis_data(transform)
  list(transform = transform, axis_data = axis_data)
}

strip_scale_x_range_axis_data <- function(transform, layout = NULL) {
  gene_rows <- transform[transform$region_type == "gene", , drop = FALSE]
  if (nrow(gene_rows) == 0L) {
    return(data.frame())
  }

  panel_windows <- strip_scale_x_panel_windows(layout)
  groups <- split(gene_rows, paste(gene_rows$PANEL, gene_rows$track, sep = "\r"), drop = TRUE)
  pieces <- lapply(groups, function(group) {
    window <- panel_windows[panel_windows$PANEL == group$PANEL[[1L]], , drop = FALSE]
    if (nrow(window) == 1L) {
      genomic_start <- window$genomic_start[[1L]]
      genomic_end <- window$genomic_end[[1L]]
    } else {
      genomic_start <- min(group$genomic_start, na.rm = TRUE)
      genomic_end <- max(group$genomic_end, na.rm = TRUE)
    }
    plot_start <- min(group$plot_start, group$plot_end, na.rm = TRUE)
    plot_end <- max(group$plot_start, group$plot_end, na.rm = TRUE)
    data.frame(
      PANEL = group$PANEL[[1L]],
      track = group$track[[1L]],
      genomic_start = genomic_start,
      genomic_end = genomic_end,
      plot_start = plot_start,
      plot_end = plot_end,
      start_label = strip_scale_x_bp_label(genomic_start),
      end_label = strip_scale_x_bp_label(genomic_end),
      stringsAsFactors = FALSE
    )
  })
  axis_data <- dplyr::bind_rows(pieces)
  axis_data <- axis_data[order(axis_data$PANEL, axis_data$track), , drop = FALSE]
  axis_data$axis_group_index <- ave(
    seq_len(nrow(axis_data)),
    axis_data$PANEL,
    FUN = seq_along
  )
  axis_data$axis_group_count <- ave(
    axis_data$axis_group_index,
    axis_data$PANEL,
    FUN = function(x) rep(length(x), length(x))
  )
  rownames(axis_data) <- NULL
  axis_data
}

strip_scale_x_panel_windows <- function(layout) {
  layout_df <- if (!is.null(layout)) layout$layout else NULL
  required <- c("PANEL", "xlim_min", "xlim_max")
  if (!is.data.frame(layout_df) || !all(required %in% names(layout_df))) {
    return(data.frame(
      PANEL = integer(),
      genomic_start = numeric(),
      genomic_end = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  out <- data.frame(
    PANEL = ggexon_panel_id(layout_df$PANEL),
    genomic_start = pmin(as.numeric(layout_df$xlim_min), as.numeric(layout_df$xlim_max)),
    genomic_end = pmax(as.numeric(layout_df$xlim_min), as.numeric(layout_df$xlim_max)),
    stringsAsFactors = FALSE
  )
  keep <- is.finite(out$PANEL) & is.finite(out$genomic_start) &
    is.finite(out$genomic_end) & out$genomic_end > out$genomic_start
  out <- out[keep & !duplicated(out$PANEL), , drop = FALSE]
  rownames(out) <- NULL
  out
}

strip_scale_x_bp_label <- function(x) {
  x <- as.numeric(x)
  if (length(x) != 1L || !is.finite(x)) {
    return(NA_character_)
  }
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

strip_scale_x_apply_transforms <- function(data, tag_layers, transform) {
  split_transform <- split(transform, paste(transform$PANEL, transform$track, sep = "\r"), drop = TRUE)
  for (layer_i in tag_layers) {
    df <- data[[layer_i]]
    if (!is.data.frame(df) || nrow(df) == 0L) next
    if (!all(c("PANEL", "track") %in% names(df))) next
    if ("xmin" %in% names(df) && !"genomic_xmin" %in% names(df)) {
      df$genomic_xmin <- df$xmin
    }
    if ("xmax" %in% names(df) && !"genomic_xmax" %in% names(df)) {
      df$genomic_xmax <- df$xmax
    }
    if ("x" %in% names(df) && !"genomic_x" %in% names(df)) {
      df$genomic_x <- df$x
    }
    panel_ids <- ggexon_panel_id(df$PANEL)
    keys <- paste(panel_ids, as.character(df$track), sep = "\r")
    for (key in unique(keys)) {
      tr <- split_transform[[key]]
      if (is.null(tr) || nrow(tr) == 0L) next
      idx <- which(keys == key)
      df <- strip_scale_x_apply_layer_coordinates(df, layer_i, idx, tr)
      df <- strip_scale_x_apply_row_metadata(df, layer_i, idx, tr)
    }
    data[[layer_i]] <- df
  }
  data
}

strip_scale_x_flatten_genetag_lanes <- function(data, layers, tag_layers) {
  for (layer_i in tag_layers) {
    df <- data[[layer_i]]
    if (!is.data.frame(df) || nrow(df) == 0L) next

    params <- syn_layer_params(layers[[layer_i]])
    exon_height <- .genetag_effective_height(
      exon_height = params$exon_height,
      height = params$height
    )
    label_position <- .genetag_label_position(
      params$label_position %||% "auto",
      show_label = params$show_label %||% TRUE
    )

    df$y <- exon_height / 2
    df$ymin <- df$y - exon_height / 2
    df$ymax <- df$y + exon_height / 2
    df$.ggexon_body_ymin <- df$ymin
    df$.ggexon_body_ymax <- df$ymax
    df$gene_lane <- 1L
    df$gene_lane_count <- 1L
    df$gene_layout <- "single"

    if (!identical(label_position, "none") && !identical(label_position, "inside")) {
      label_space <- .genetag_label_reserved_space(
        exon_height = exon_height,
        label_offset_fraction = params$label_offset_fraction %||% 0.3,
        label_max_lanes = params$label_max_lanes %||% 3L
      )
      positions <- .parse_label_positions(params$label_direction %||% "top")
      if ("top" %in% positions || "center" %in% positions) {
        df$ymax <- df$ymax + label_space
      }
      if ("bottom" %in% positions) {
        df$ymin <- df$ymin - label_space
      }
    }

    data[[layer_i]] <- df
  }

  data
}

strip_scale_x_apply_layer_coordinates <- function(df, layer_i, idx, transform) {
  row_keys <- paste(layer_i, seq_len(nrow(df)), sep = ":")
  source_index <- strip_scale_x_source_key_index(transform)
  transform_idx <- unname(source_index[row_keys[idx]])
  has_source <- !is.na(transform_idx)

  if (any(has_source)) {
    rows <- transform[transform_idx[has_source], , drop = FALSE]
    matched <- idx[has_source]
    if (all(c("xmin", "genomic_xmin") %in% names(df))) {
      df$xmin[matched] <- strip_scale_x_map_with_rows(df$genomic_xmin[matched], rows)
    }
    if (all(c("xmax", "genomic_xmax") %in% names(df))) {
      df$xmax[matched] <- strip_scale_x_map_with_rows(df$genomic_xmax[matched], rows)
    }
    if (all(c("x", "genomic_x") %in% names(df))) {
      df$x[matched] <- strip_scale_x_map_with_rows(df$genomic_x[matched], rows)
    }
  }

  if (any(!has_source)) {
    fallback <- idx[!has_source]
    if (all(c("xmin", "genomic_xmin") %in% names(df))) {
      df$xmin[fallback] <- strip_scale_to_plot_x(df$genomic_xmin[fallback], transform)
    }
    if (all(c("xmax", "genomic_xmax") %in% names(df))) {
      df$xmax[fallback] <- strip_scale_to_plot_x(df$genomic_xmax[fallback], transform)
    }
    if (all(c("x", "genomic_x") %in% names(df))) {
      df$x[fallback] <- strip_scale_to_plot_x(df$genomic_x[fallback], transform)
    }
  }

  df
}

strip_scale_x_source_key_index <- function(transform) {
  if (!"source_keys" %in% names(transform)) {
    return(stats::setNames(integer(), character()))
  }
  gene_row_index <- which(transform$region_type == "gene")
  gene_rows <- transform[gene_row_index, , drop = FALSE]
  if (nrow(gene_rows) == 0L) {
    return(stats::setNames(integer(), character()))
  }

  keys <- strsplit(as.character(gene_rows$source_keys), ",", fixed = TRUE)
  valid <- lengths(keys) > 0L & !is.na(gene_rows$source_keys)
  if (!any(valid)) {
    return(stats::setNames(integer(), character()))
  }

  keys <- keys[valid]
  row_index <- gene_row_index[valid]
  out <- rep(row_index, lengths(keys))
  names(out) <- unlist(keys, use.names = FALSE)
  out[nzchar(names(out))]
}

strip_scale_x_map_with_rows <- function(x, rows) {
  vapply(seq_along(x), function(i) {
    value <- as.numeric(x[[i]])
    if (!is.finite(value)) return(value)
    row <- rows[i, , drop = FALSE]
    has_anchor <- all(c("genomic_anchor", "plot_anchor") %in% names(row)) &&
      is.finite(row$genomic_anchor[[1L]]) &&
      is.finite(row$plot_anchor[[1L]])
    if (!is.finite(row$slope[[1L]]) || !has_anchor) {
      return(value)
    }
    row$plot_anchor[[1L]] + (value - row$genomic_anchor[[1L]]) * row$slope[[1L]]
  }, numeric(1))
}

strip_scale_x_apply_row_metadata <- function(df, layer_i, idx, transform) {
  gene_rows <- transform[transform$region_type == "gene", , drop = FALSE]
  if (nrow(gene_rows) == 0L || !"source_keys" %in% names(gene_rows)) return(df)

  for (col in c("visual_class", "slot_type")) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }
  for (col in c("homology_anchor", "is_anchor")) {
    if (!col %in% names(df)) df[[col]] <- FALSE
  }
  if (!"strip_x_direction" %in% names(df)) df$strip_x_direction <- 1

  row_keys <- paste(layer_i, seq_len(nrow(df)), sep = ":")
  for (i in seq_len(nrow(gene_rows))) {
    source_keys <- strsplit(as.character(gene_rows$source_keys[[i]]), ",", fixed = TRUE)[[1L]]
    matched <- idx[row_keys[idx] %in% source_keys]
    if (length(matched) == 0L) next
    df$visual_class[matched] <- gene_rows$visual_class[[i]]
    df$slot_type[matched] <- gene_rows$slot_type[[i]]
    df$homology_anchor[matched] <- isTRUE(gene_rows$homology_anchor[[i]])
    df$is_anchor[matched] <- isTRUE(gene_rows$is_anchor[[i]])
    df$strip_x_direction[matched] <- gene_rows$strip_x_direction[[i]] %||% 1
  }
  df
}
