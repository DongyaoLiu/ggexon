#' Uniform gene-width scale for multi-track genomic plots
#'
#' `strip_scale()` normalizes gene and intergenic region widths across
#' genomic tracks so that every gene occupies the same visual width and
#' every intergenic gap occupies the same visual width. The track with the
#' most genes determines the shared coordinate system; sparser tracks are
#' aligned within that space.
#'
#' This scale is designed for [`geom_genelabel()`] layers and is mutually
#' exclusive with [`scale_x_ggexon_genomic()`]. It works best with
#' `facet_genomictree(scales = "fixed_x")` — the function will modify the
#' facet to use fixed x scales internally.
#'
#' @param gene_gap_ratio Ratio of gene visual width to intergenic gap width.
#'   When `NULL` (the default), the ratio is derived from the densest
#'   track's actual genomic proportions (median gene width divided by
#'   median gap width).
#' @param align How tracks with fewer genes are positioned within the
#'   shared x-axis. `"left"` packs genes to the left edge, `"right"` to
#'   the right edge, `"center"` centres them.  Ignored when `homo_align`
#'   is active (a warning is issued if set explicitly).
#' @param homo_align `FALSE` (default) for independent per-track ordering.
#'   `TRUE` to auto-detect the reference species from homology annotations
#'   and align homologous genes at the same visual x-position across all
#'   tracks.  A character value (e.g. `"C. elegans N2"`) explicitly names
#'   the reference species.
#' @param species_ratio Visual width ratio for species-specific genes
#'   relative to homologous genes.  `NULL` (default) auto-scales each
#'   gene proportionally to its genomic length relative to the median
#'   reference gene length.  A numeric value (e.g. `0.5`) sets a fixed
#'   ratio.
#'
#' @return A ggexon strip-scale specification, added to the plot with `+`.
#' @export
strip_scale <- function(gene_gap_ratio = NULL,
                        align = c("left", "right", "center"),
                        homo_align = FALSE,
                        species_ratio = NULL) {
  align <- match.arg(align)
  if (!is.null(gene_gap_ratio)) {
    if (!is.numeric(gene_gap_ratio) ||
        length(gene_gap_ratio) != 1L ||
        is.na(gene_gap_ratio) ||
        gene_gap_ratio <= 0) {
      stop("`gene_gap_ratio` must be a single positive number or `NULL`.",
           call. = FALSE)
    }
  }
  if (!is.null(species_ratio)) {
    if (!is.numeric(species_ratio) || length(species_ratio) != 1L ||
        is.na(species_ratio) || species_ratio <= 0 || species_ratio > 1) {
      stop("`species_ratio` must be a number in (0, 1] or `NULL`.",
           call. = FALSE)
    }
  }
  homo_active <- .strip_scale_homo_active(homo_align)
  structure(
    list(gene_gap_ratio = gene_gap_ratio,
         align = align,
         homo_align = homo_align,
         species_ratio = species_ratio,
         homo_active = homo_active),
    class = "ggexon_strip_scale_spec"
  )
}

.strip_scale_homo_active <- function(homo_align) {
  isTRUE(homo_align) || (is.character(homo_align) && length(homo_align) == 1L &&
                         !is.na(homo_align) && nzchar(homo_align))
}

#' @export
ggplot_add.ggexon_strip_scale_spec <- function(object, plot, object_name) {
  if (!is_ggexon(plot)) {
    stop("`strip_scale()` can only be added to a ggexon plot.", call. = FALSE)
  }
  if (!is.null(plot@genomic_x_scale)) {
    stop("`strip_scale()` and `scale_x_ggexon_genomic()` are mutually exclusive.",
         call. = FALSE)
  }
  if (!is.null(plot@strip_scale)) {
    warning("`strip_scale()` is already set on this plot; replacing it.",
            call. = FALSE)
  }
  plot@strip_scale <- object
  plot
}

# ── Build-pipeline integration ──────────────────────────────────────────

apply_strip_scale <- function(data, layers, strip_scale_spec, layout, plot) {
  gene_layers <- .strip_scale_gene_layers(layers)
  if (length(gene_layers) == 0L) {
    stop("`strip_scale()` requires at least one `geom_genelabel()` layer.",
         call. = FALSE)
  }

  gene_intervals <- .strip_scale_collect_intervals(
    data, gene_layers, strip_scale_spec
  )
  if (nrow(gene_intervals) == 0L) {
    stop("`strip_scale()` found no gene-level data in genelabel layers.",
         call. = FALSE)
  }

  if (isTRUE(strip_scale_spec$homo_active) &&
      !identical(strip_scale_spec$align, "left")) {
    warning("`align` is ignored when `homo_align` is active.", call. = FALSE)
  }

  ratio <- .strip_scale_resolve_ratio(gene_intervals,
                                       strip_scale_spec$gene_gap_ratio)

  if (isTRUE(strip_scale_spec$homo_active)) {
    transforms <- .strip_scale_build_homo_transforms(
      gene_intervals, ratio, strip_scale_spec, plot
    )
  } else {
    transforms <- .strip_scale_build_transforms(
      gene_intervals, ratio, strip_scale_spec$align
    )
  }

  data <- lapply(seq_along(data), function(i) {
    .strip_scale_transform_layer(data[[i]], transforms)
  })
  layout <- .strip_scale_force_fixed_x(layout, names(transforms))
  list(data = data, layout = layout, transforms = transforms)
}

.strip_scale_gene_layers <- function(layers) {
  which(vapply(layers, function(l) identical(l$geom, GeomGeneLabel), logical(1)))
}

.strip_scale_collect_intervals <- function(data, gene_layers, spec) {
  pieces <- list()
  for (i in gene_layers) {
    df <- data[[i]]
    if (!is.data.frame(df) || nrow(df) == 0L ||
        !all(c("PANEL", "transcripts", "xmin", "xmax", "track") %in% names(df))) {
      next
    }
    want_refgene <- isTRUE(spec$homo_active) && "reference_gene" %in% names(df)

    gene_bounds <- df %>%
      dplyr::mutate(
        PANEL = ggexon_panel_id(.data$PANEL),
        gene_start = pmin(as.numeric(.data$xmin), as.numeric(.data$xmax)),
        gene_end   = pmax(as.numeric(.data$xmin), as.numeric(.data$xmax))
      ) %>%
      dplyr::filter(is.finite(.data$gene_start), is.finite(.data$gene_end),
                    .data$gene_end > .data$gene_start)

    if (want_refgene) {
      gene_bounds <- gene_bounds %>%
        dplyr::group_by(.data$PANEL, .data$track, .data$transcripts) %>%
        dplyr::summarise(
          gene_start     = min(.data$gene_start, na.rm = TRUE),
          gene_end       = max(.data$gene_end,   na.rm = TRUE),
          reference_gene = dplyr::first(.data$reference_gene),
          .groups = "drop"
        )
    } else {
      gene_bounds <- gene_bounds %>%
        dplyr::group_by(.data$PANEL, .data$track, .data$transcripts) %>%
        dplyr::summarise(
          gene_start = min(.data$gene_start, na.rm = TRUE),
          gene_end   = max(.data$gene_end,   na.rm = TRUE),
          .groups = "drop"
        )
    }

    gene_bounds <- gene_bounds %>%
      dplyr::arrange(.data$PANEL, .data$track, .data$gene_start)
    if (nrow(gene_bounds) == 0L) next
    pieces[[length(pieces) + 1L]] <- gene_bounds
  }
  intervals <- dplyr::bind_rows(pieces)
  if (is.null(intervals) || nrow(intervals) == 0L) return(data.frame())
  dplyr::distinct(intervals)
}

.strip_scale_resolve_ratio <- function(gene_intervals, gene_gap_ratio = NULL) {
  if (!is.null(gene_gap_ratio)) return(as.numeric(gene_gap_ratio))
  gene_counts <- dplyr::count(gene_intervals, .data$PANEL, name = "n_genes")
  if (nrow(gene_counts) == 0L) return(3)
  densest_panel <- gene_counts$PANEL[which.max(gene_counts$n_genes)]
  densest_genes <- gene_intervals[
    gene_intervals$PANEL == densest_panel, , drop = FALSE
  ]
  densest_genes <- densest_genes[order(densest_genes$gene_start), , drop = FALSE]
  gene_widths <- densest_genes$gene_end - densest_genes$gene_start
  gene_widths <- gene_widths[is.finite(gene_widths) & gene_widths > 0]
  gap_widths <- pmax(
    densest_genes$gene_start[-1L] - densest_genes$gene_end[-nrow(densest_genes)], 0
  )
  gap_widths <- gap_widths[is.finite(gap_widths) & gap_widths > 0]
  med_gene <- stats::median(gene_widths, na.rm = TRUE)
  med_gap <- stats::median(gap_widths, na.rm = TRUE)
  if (!is.finite(med_gene) || med_gene <= 0) return(3)
  if (!is.finite(med_gap) || med_gap <= 0) return(max(med_gene / 1000, 3))
  max(med_gene / med_gap, 0.1)
}

.strip_scale_merge_intervals <- function(starts, ends) {
  if (length(starts) == 0L) return(data.frame(start = numeric(0), end = numeric(0)))
  ord <- order(starts, ends)
  starts <- starts[ord]; ends <- ends[ord]
  out_starts <- numeric(0); out_ends <- numeric(0)
  cur_start <- starts[[1L]]; cur_end <- ends[[1L]]
  for (i in seq_along(starts)[-1L]) {
    if (starts[[i]] <= cur_end) {
      cur_end <- max(cur_end, ends[[i]])
    } else {
      out_starts <- c(out_starts, cur_start)
      out_ends   <- c(out_ends,   cur_end)
      cur_start  <- starts[[i]]; cur_end <- ends[[i]]
    }
  }
  out_starts <- c(out_starts, cur_start); out_ends <- c(out_ends, cur_end)
  data.frame(start = out_starts, end = out_ends)
}

# ── Homology-aligned transform building ─────────────────────────────────

.strip_scale_build_homo_transforms <- function(gene_intervals, ratio, spec, plot) {
  ref_species <- .strip_scale_resolve_ref(spec$homo_align, gene_intervals, plot)
  ref_genes <- gene_intervals[gene_intervals$track == ref_species, , drop = FALSE]
  ref_genes <- ref_genes[order(ref_genes$gene_start), , drop = FALSE]

  if (!"reference_gene" %in% names(gene_intervals)) {
    # No homology data at all — fall back to standard transform
    return(.strip_scale_build_transforms(gene_intervals, ratio, spec$align))
  }

  all_tracks <- unique(gene_intervals$track)
  species_order <- .strip_scale_phylo_order(plot, ref_species, all_tracks)

  g <- ratio; p <- 1
  N_ref <- nrow(ref_genes)
  total_span <- N_ref * g + max(N_ref - 1L, 0L) * p

  sp_ratio <- .strip_scale_resolve_sp_ratio(spec$species_ratio, ref_genes)

  # ── cumulative slot map ──────────────────────────────────────────
  # slot of each gene per processed species, keyed by reference_gene
  slot_map <- list()
  slot_map[[ref_species]] <- stats::setNames(
    seq_len(N_ref),
    as.character(ref_genes$reference_gene)
  )
  # master order: list of slot entries, each a named list species→gene_row
  master <- lapply(seq_len(N_ref), function(i) {
    s <- list(); s[[ref_species]] <- ref_genes[i, , drop = FALSE]; s
  })

  for (sp in species_order) {
    sp_genes <- gene_intervals[gene_intervals$track == sp, , drop = FALSE]
    sp_genes <- sp_genes[order(sp_genes$gene_start), , drop = FALSE]
    if (nrow(sp_genes) == 0L) next

    sp_slots <- rep(NA_integer_, nrow(sp_genes))

    # Cat A: reference homologs
    if (ref_species %in% names(slot_map)) {
      ref_map <- slot_map[[ref_species]]
      for (i in seq_len(nrow(sp_genes))) {
        rg <- as.character(sp_genes$reference_gene[[i]])
        if (!is.na(rg) && nzchar(rg) && rg %in% names(ref_map)) {
          sp_slots[[i]] <- ref_map[[rg]]
        }
      }
    }

    # Cat B: homologs in already-processed species
    for (prev_sp in setdiff(names(slot_map), c(ref_species, sp))) {
      prev_map <- slot_map[[prev_sp]]
      for (i in seq_len(nrow(sp_genes))) {
        if (!is.na(sp_slots[[i]])) next
        rg <- as.character(sp_genes$reference_gene[[i]])
        if (!is.na(rg) && nzchar(rg) && rg %in% names(prev_map)) {
          sp_slots[[i]] <- prev_map[[rg]]
        }
      }
    }

    # Cat C/D: species-specific — insert as new slots
    if (any(is.na(sp_slots))) {
      sp_genes$.idx <- seq_len(nrow(sp_genes))
      sp_genes$.slot <- sp_slots

      unplaced <- sp_genes[is.na(sp_genes$.slot), , drop = FALSE]
      placed   <- sp_genes[!is.na(sp_genes$.slot), , drop = FALSE]
      placed <- placed[order(placed$.slot), , drop = FALSE]

      for (i in seq_len(nrow(unplaced))) {
        gene <- unplaced[i, , drop = FALSE]
        gene_mid <- (gene$gene_start + gene$gene_end) / 2

        # find flanking placed slots in genomic order
        left_idx  <- max(which(placed$.idx < gene$.idx), 0L)
        right_idx <- min(which(placed$.idx > gene$.idx), nrow(placed) + 1L)

        if (left_idx > 0L && right_idx <= nrow(placed)) {
          left_slot  <- placed$.slot[[left_idx]]
          right_slot <- placed$.slot[[right_idx]]
          insert_pos <- left_slot + 1L
        } else if (left_idx > 0L) {
          insert_pos <- placed$.slot[[left_idx]] + 1L
        } else if (right_idx <= nrow(placed)) {
          insert_pos <- placed$.slot[[right_idx]]
        } else {
          insert_pos <- length(master) + 1L
        }

        # shift all subsequent slots +1
        for (sname in names(slot_map)) {
          sm <- slot_map[[sname]]
          to_shift <- sm >= insert_pos
          sm[to_shift] <- sm[to_shift] + 1L
          slot_map[[sname]] <- sm
        }
        # insert new empty slot into master
        master <- append(master, list(list()), after = insert_pos - 1L)

        # assign gene to this new slot
        sp_slots[[gene$.idx]] <- insert_pos
      }
    }

    # register this species' gene→slot mapping
    named_slots <- sp_slots
    names(named_slots) <- as.character(sp_genes$reference_gene)
    named_slots <- named_slots[!is.na(names(named_slots)) & nzchar(names(named_slots))]
    slot_map[[sp]] <- named_slots

    # place genes into master slots
    for (i in seq_len(nrow(sp_genes))) {
      s <- sp_slots[[i]]
      if (is.na(s)) next
      master[[s]][[sp]] <- sp_genes[i, , drop = FALSE]
    }
  }

  # ── build per-panel transforms ────────────────────────────────────
  transforms <- list()
  for (panel_id in unique(gene_intervals$PANEL)) {
    track_name <- unique(gene_intervals$track[gene_intervals$PANEL == panel_id])
    if (length(track_name) != 1L) next

    transform <- .strip_scale_homo_panel_transform(
      master, track_name, g, p, sp_ratio, total_span
    )
    if (!is.null(transform) && nrow(transform) > 0L) {
      transform$PANEL <- panel_id
      transforms[[as.character(panel_id)]] <- transform
    }
  }

  transforms
}

.strip_scale_homo_panel_transform <- function(master, track, g, p,
                                              sp_ratio, total_span) {
  pieces <- list()
  current_visual <- 0
  prev_gene_end <- NULL
  prev_visual_end <- 0

  for (slot_i in seq_along(master)) {
    slot <- master[[slot_i]]
    gene <- slot[[track]]

    if (is.null(gene) || nrow(gene) == 0L) {
      # no gene in this track for this slot → gap
      # gap visual width determined proportionally
      prev_gene_end <- NULL
      prev_visual_end <- current_visual
      current_visual <- current_visual + p
      next
    }

    gene <- gene[1L, , drop = FALSE]
    gw <- gene$gene_end - gene$gene_start
    is_homolog <- !is.null(slot[[names(slot)[1L]]])  # any other species has gene here

    # determine visual width for this gene block
    if (length(slot) > 1L || isTRUE(is_homolog)) {
      # homolog (or tandem group) → full width g
      block_width <- g
    } else {
      # species-specific → compressed width
      block_width <- .strip_scale_species_width(gw, sp_ratio, g)
    }

    if (gw > 0) {
      pieces[[length(pieces) + 1L]] <- data.frame(
        genomic_start = gene$gene_start,
        genomic_end   = gene$gene_end,
        plot_start    = current_visual,
        plot_end      = current_visual + block_width,
        slope         = block_width / gw,
        region_type   = "gene",
        stringsAsFactors = FALSE
      )
    }
    prev_gene_end <- gene$gene_end
    prev_visual_end <- current_visual + block_width
    current_visual <- current_visual + block_width

    # gap after this gene (to next slot)
    if (slot_i < length(master)) {
      # gap visual width = p, adjusted if the gene was compressed
      gap_extra <- g - block_width  # extra space from compression
      gap_width <- p + gap_extra

      # Check if between-gene genomic gap exists
      next_slot <- master[[slot_i + 1L]]
      next_gene <- next_slot[[track]]
      if (!is.null(next_gene) && nrow(next_gene) > 0L) {
        next_gene <- next_gene[1L, , drop = FALSE]
        gap_genomic <- next_gene$gene_start - gene$gene_end
        if (gap_genomic > 0) {
          pieces[[length(pieces) + 1L]] <- data.frame(
            genomic_start = gene$gene_end,
            genomic_end   = next_gene$gene_start,
            plot_start    = current_visual,
            plot_end      = current_visual + gap_width,
            slope         = gap_width / gap_genomic,
            region_type   = "gap",
            stringsAsFactors = FALSE
          )
        }
      }
      current_visual <- current_visual + gap_width
    }
  }

  if (length(pieces) == 0L) return(NULL)
  transform <- do.call(rbind, pieces)
  rownames(transform) <- NULL
  transform
}

.strip_scale_species_width <- function(gw, sp_ratio, g) {
  if (is.null(sp_ratio)) {
    # auto-scale: proportional, but clamped
    w <- max(gw / max(gw, 1), 0.1) * g
    return(min(w, g * 0.8))
  }
  g * sp_ratio
}

.strip_scale_resolve_sp_ratio <- function(species_ratio, ref_genes) {
  if (!is.null(species_ratio)) return(as.numeric(species_ratio))
  # auto: no fixed ratio; width determined per-gene in .strip_scale_species_width
  NULL
}

# ── Reference species resolution ──────────────────────────────────────

.strip_scale_resolve_ref <- function(homo_align, gene_intervals, plot) {
  if (is.character(homo_align) && length(homo_align) == 1L &&
      !is.na(homo_align) && nzchar(homo_align)) {
    if (!homo_align %in% gene_intervals$track) {
      stop("Reference species '", homo_align,
           "' not found in genelabel tracks.", call. = FALSE)
    }
    return(homo_align)
  }

  # auto-detect: species that is reference_species in most homology annotations
  if (methods::is(plot@data, "SynSpecies")) {
    hl <- tryCatch(homology_annotations(plot@data), error = function(e) list())
    if (length(hl) > 0L) {
      ref_counts <- table(vapply(hl, function(h) {
        if (methods::is(h, "HomologyAnnotation")) reference_species(h)
        else NA_character_
      }, character(1)))
      if (length(ref_counts) > 0L) {
        best <- names(which.max(ref_counts))
        if (best %in% gene_intervals$track) return(best)
      }
    }
  }

  # fallback: densest track
  gene_counts <- dplyr::count(gene_intervals, .data$track, name = "n")
  if (nrow(gene_counts) == 0L) {
    stop("No genelabel tracks found.", call. = FALSE)
  }
  gene_counts$track[which.max(gene_counts$n)]
}

.strip_scale_phylo_order <- function(plot, ref_species, all_tracks) {
  others <- setdiff(all_tracks, ref_species)
  if (length(others) <= 1L) return(others)

  tree <- NULL
  if (methods::is(plot@data, "SynSpecies")) {
    tree <- tryCatch(species_tree(plot@data), error = function(e) NULL)
  }
  if (is.null(tree) && !is.null(plot@genomic_tree)) {
    tree <- plot@genomic_tree$tree
  }

  if (!is.null(tree) && requireNamespace("ape", quietly = TRUE)) {
    tips <- tree$tip.label
    present <- intersect(tips, all_tracks)
    if (length(present) >= 2L) {
      # compute patristic distance from reference to each tip
      if (ref_species %in% tips) {
        dists <- ape::cophenetic.phylo(tree)
        ref_dist <- dists[ref_species, present]
        ordered <- names(sort(ref_dist))
        return(setdiff(ordered, ref_species))
      }
    }
  }

  # fallback: alphabetic
  sort(others)
}

# ── Standard (non-homology) transform building ──────────────────────────

.strip_scale_build_transforms <- function(gene_intervals, ratio, align = "left") {
  gene_intervals <- gene_intervals[
    order(gene_intervals$PANEL, gene_intervals$gene_start), , drop = FALSE
  ]
  gene_counts <- dplyr::count(gene_intervals, .data$PANEL, name = "n_genes")
  if (nrow(gene_counts) == 0L) return(list())
  N_max <- max(gene_counts$n_genes)
  g <- ratio; p <- 1
  total_span <- N_max * g + max(N_max - 1L, 0L) * p

  transforms <- list()
  for (panel_id in unique(gene_intervals$PANEL)) {
    panel_genes <- gene_intervals[
      gene_intervals$PANEL == panel_id, , drop = FALSE
    ]
    panel_genes <- panel_genes[order(panel_genes$gene_start), , drop = FALSE]
    merged <- .strip_scale_merge_intervals(panel_genes$gene_start,
                                            panel_genes$gene_end)
    M <- nrow(merged)
    if (M == 0L) next

    track_span <- M * g + max(M - 1L, 0L) * p
    offset <- switch(align,
      left = 0, right = total_span - track_span,
      center = (total_span - track_span) / 2
    )
    offset <- max(offset, 0)

    pieces <- list(); current_visual <- offset
    for (i in seq_len(M)) {
      block <- merged[i, , drop = FALSE]
      bw <- block$end - block$start
      if (bw > 0) {
        pieces[[length(pieces) + 1L]] <- data.frame(
          genomic_start = block$start, genomic_end = block$end,
          plot_start = current_visual, plot_end = current_visual + g,
          slope = g / bw, region_type = "gene", stringsAsFactors = FALSE
        )
      }
      current_visual <- current_visual + g
      if (i < M) {
        next_block <- merged[i + 1L, , drop = FALSE]
        gap_w <- next_block$start - block$end
        if (gap_w > 0) {
          pieces[[length(pieces) + 1L]] <- data.frame(
            genomic_start = block$end, genomic_end = next_block$start,
            plot_start = current_visual, plot_end = current_visual + p,
            slope = p / gap_w, region_type = "gap", stringsAsFactors = FALSE
          )
        }
        current_visual <- current_visual + p
      }
    }
    transform <- do.call(rbind, pieces)
    if (nrow(transform) > 0L) {
      transform$PANEL <- panel_id
      rownames(transform) <- NULL
      transforms[[as.character(panel_id)]] <- transform
    }
  }
  transforms
}

# ── Layer transform application ────────────────────────────────────────

.strip_scale_transform_layer <- function(layer_data, transforms) {
  if (!is.data.frame(layer_data) || length(transforms) == 0L) return(layer_data)
  x_columns <- intersect(c("x", "xmin", "xmax", "xend", "xintercept"),
                         names(layer_data))
  if (length(x_columns) == 0L || !"PANEL" %in% names(layer_data)) return(layer_data)
  panel_ids <- ggexon_panel_id(layer_data$PANEL)
  for (panel_id in unique(panel_ids)) {
    key <- as.character(panel_id)
    transform <- transforms[[key]]
    if (is.null(transform)) next
    idx <- which(panel_ids == panel_id)
    for (col in x_columns) {
      genomic_col <- paste0("genomic_", col)
      if (!genomic_col %in% names(layer_data)) {
        layer_data[[genomic_col]] <- layer_data[[col]]
      }
      layer_data[[col]][idx] <- strip_scale_to_plot_x(
        layer_data[[genomic_col]][idx], transform
      )
    }
  }
  layer_data
}

strip_scale_to_plot_x <- function(x, transform) {
  vapply(as.numeric(x), function(value) {
    if (!is.finite(value) || is.null(transform) || nrow(transform) == 0L)
      return(value)
    first <- transform[1L, , drop = FALSE]
    last  <- transform[nrow(transform), , drop = FALSE]
    if (value < first$genomic_start[[1L]])
      return(first$plot_start[[1L]] - (first$genomic_start[[1L]] - value))
    if (value > last$genomic_end[[1L]])
      return(last$plot_end[[1L]] + (value - last$genomic_end[[1L]]))
    idx <- which(value >= transform$genomic_start & value <= transform$genomic_end)
    if (length(idx) == 0L) {
      dists <- abs(value - transform$genomic_start)
      idx <- which.min(dists)
      if (length(idx) == 0L) return(value)
    }
    idx <- idx[[1L]]
    row <- transform[idx, , drop = FALSE]
    row$plot_start[[1L]] + (value - row$genomic_start[[1L]]) * row$slope[[1L]]
  }, numeric(1))
}

.strip_scale_force_fixed_x <- function(layout, transform_panels) {
  layout_df <- layout$layout
  if (!is.data.frame(layout_df) || !"PANEL" %in% names(layout_df)) return(layout)
  panel_ids <- as.character(unique(layout_df$PANEL))
  target_panels <- intersect(panel_ids, transform_panels)
  if (length(target_panels) == 0L) return(layout)
  layout_df$SCALE_X <- as.integer(layout_df$SCALE_X)
  layout_df$SCALE_X[as.character(layout_df$PANEL) %in% target_panels] <- 1L
  if (!is.null(layout$facet) && !is.null(layout$facet$params) &&
      !is.null(layout$facet$params$free)) {
    layout$facet$params$free$x <- FALSE
  }
  layout$layout <- layout_df
  layout
}