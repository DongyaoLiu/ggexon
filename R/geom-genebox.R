#' Draw fixed-size gene boxes with internal strand arrows
#'
#' `geom_genebox()` represents each gene with a square of fixed physical size
#' and a horizontal arrow indicating transcription direction. Because the box
#' dimensions are measured in millimetres, the symbol stays square under free
#' genomic scales and coordinate transformations.
#'
#' For ordinary data frames, map `x`, `y`, and `strand`. In a ggexon plot backed
#' by a [SynIndividual] or [SynSpecies] object, the layer selects, for each gene,
#' the protein-coding transcript with the greatest genomic span. `anchor = "start"`
#' uses the middle nucleotide of a complete annotated initiation codon,
#' `anchor = "end"` uses the middle nucleotide of a complete annotated stop
#' codon, and `anchor = "middle"` uses the genomic midpoint between those two
#' anchors. Associated `start_codon` or `stop_codon` records are complete only
#' when their total feature width is exactly three nucleotides. Otherwise, the
#' middle position of the corresponding terminal CDS triplet is used as a
#' positional proxy; this fallback does not verify an ATG or stop-codon sequence.
#'
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard
#'   ggplot2 layer arguments. When column names are literally `x`, `y`, and
#'   `strand`, they are mapped automatically.
#' @param box_size Side length of each square in millimetres. Defaults to `3`.
#' @param anchor Genomic point derived for Syn-backed data: the annotated
#'   initiation-codon centre or 5-prime terminal-CDS proxy (`"start"`), midpoint
#'   between the selected start and end anchors (`"middle"`), or annotated
#'   stop-codon centre or 3-prime terminal-CDS proxy (`"end"`). Defaults to
#'   `"middle"`.
#' @param arrow_colour Optional fixed colour for the internal arrow. `NULL`
#'   chooses black or white separately for each box to contrast with its fill.
#' @param arrow_linewidth Width of the internal arrow in millimetres.
#' @param arrow_head_size Length of the closed triangular arrow head in
#'   millimetres.
#' @param species Optional individual identifier for `SynSpecies` input.
#' @param chr Optional chromosome or seqname restriction for Syn-backed data.
#' @param subset Optional numeric length-two genomic window for Syn-backed data.
#'
#' @details
#' Unknown strands (`NA`, `"*"`, or values other than `"+"` and `"-"`) retain
#' their square but omit the internal arrow. Reversing the x scale or x
#' coordinate direction reverses the visible arrow direction as well.
#' Supplying a `slot` column allows [strip_scale_x()] with `slot_order` to align
#' curated comparison slots exactly. Slot membership is supplied metadata, not
#' an inference of one-to-one homology or evolutionary loss. When a track has
#' enough ordered genes to infer whether the synthetic template reverses its
#' raw genomic direction, the internal arrows are corrected automatically;
#' [strip_scale_x()] warns and keeps the raw direction when this is
#' underdetermined.
#'
#' Syn-derived data retain `genomic_x`, `anchor_start`, `anchor_middle`,
#' `anchor_end`, `anchor_mode`, `transcript_id`, transcript-span columns, coding
#' bounds, gene identifiers, and (for `SynSpecies`) injected homology metadata.
#' The per-end `initiation_anchor_source` and `stop_anchor_source` columns record
#' either an explicit codon feature or `"terminal_CDS_positional_proxy"`;
#' `initiation_anchor_fallback`, `stop_anchor_fallback`, and
#' `any_anchor_fallback` flag proxy use. These columns make transcript selection,
#' anchor interpretation, and downstream alignment auditable.
#'
#' @return A ggplot layer.
#' @export
geom_genebox <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         ...,
                         box_size = 3,
                         anchor = c("middle", "start", "end"),
                         arrow_colour = NULL,
                         arrow_linewidth = 0.45,
                         arrow_head_size = 0.7,
                         species = NULL,
                         chr = NULL,
                         subset = NULL,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = FALSE) {
  anchor <- match.arg(anchor)
  mapping <- .genebox_complete_mapping(mapping, data)
  params <- Filter(Negate(is.null), list(
    ...,
    box_size = box_size,
    anchor = anchor,
    arrow_colour = arrow_colour,
    arrow_linewidth = arrow_linewidth,
    arrow_head_size = arrow_head_size,
    species = species,
    chr = chr,
    subset = subset,
    na.rm = na.rm
  ))

  layer(
    data = data,
    mapping = mapping,
    geom = GeomGeneBox,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params,
    layer_class = LayerSyn
  )
}

#' Gene-box geom
#'
#' Public ggproto implementing [geom_genebox()].
#'
#' @export
GeomGeneBox <- ggproto(
  "GeomGeneBox",
  Geom,
  ggexon_panel_role = "annotation",
  required_aes = c("x", "y", "strand"),
  default_aes = aes(
    colour = "black",
    fill = "grey35",
    linewidth = 0.3,
    linetype = 1,
    alpha = NA_real_,
    arrow_colour = NA_character_,
    strip_x_direction = 1,
    slot = NA_character_,
    track = NA_character_,
    individual = NA_character_,
    species = NA_character_,
    gene_key = NA_character_,
    gene_id = NA_character_,
    gene_name = NA_character_,
    label = NA_character_,
    transcript_id = NA_character_,
    transcripts = NA_character_,
    genomic_x = NA_real_,
    genomic_xmin = NA_real_,
    genomic_xmax = NA_real_,
    anchor_start = NA_real_,
    anchor_middle = NA_real_,
    anchor_end = NA_real_,
    anchor_mode = NA_character_,
    initiation_anchor_source = NA_character_,
    stop_anchor_source = NA_character_,
    initiation_anchor_fallback = NA,
    stop_anchor_fallback = NA,
    any_anchor_fallback = NA,
    reference_gene = NA_character_,
    reference_gene_name = NA_character_,
    homology_hit = NA,
    homology_query_hit = NA,
    homology_reference_hit = NA
  ),
  extra_params = c(
    "na.rm", "box_size", "anchor", "arrow_colour", "arrow_linewidth",
    "arrow_head_size", "species", "chr", "subset"
  ),
  default_params = function() {
    list(
      box_size = 3,
      anchor = "middle",
      arrow_colour = NULL,
      arrow_linewidth = 0.45,
      arrow_head_size = 0.7,
      species = NULL,
      chr = NULL,
      subset = NULL,
      na.rm = FALSE
    )
  },
  setup_data = function(data, params) {
    .genebox_positive_number(params$box_size %||% 3, "box_size")
    .genebox_positive_number(params$arrow_linewidth %||% 0.45, "arrow_linewidth")
    .genebox_positive_number(params$arrow_head_size %||% 0.7, "arrow_head_size")
    data
  },
  handle_na = function(data, params) {
    missing <- !is.finite(data$x) | !is.finite(data$y)
    if (any(missing)) {
      if (!isTRUE(params$na.rm)) {
        warning(
          "Removed ", sum(missing),
          " row(s) containing missing positions in geom_genebox().",
          call. = FALSE
        )
      }
      data <- data[!missing, , drop = FALSE]
    }
    data
  },
  draw_panel = function(data,
                        panel_params,
                        coord,
                        box_size = 3,
                        arrow_colour = NULL,
                        arrow_linewidth = 0.45,
                        arrow_head_size = 0.7) {
    if (nrow(data) == 0L) {
      return(zeroGrob())
    }

    box_size <- .genebox_positive_number(box_size, "box_size")
    arrow_linewidth <- .genebox_positive_number(arrow_linewidth, "arrow_linewidth")
    arrow_head_size <- .genebox_positive_number(arrow_head_size, "arrow_head_size")
    coords <- coord$transform(data, panel_params)
    keep <- is.finite(coords$x) & is.finite(coords$y)
    if (!all(keep)) {
      coords <- coords[keep, , drop = FALSE]
      data <- data[keep, , drop = FALSE]
    }
    if (nrow(data) == 0L) {
      return(zeroGrob())
    }

    stack_offset <- .genebox_stack_offsets(data$x, data$y, box_size = box_size)
    x_unit <- grid::unit(coords$x, "native")
    y_unit <- .genebox_clamped_y_units(
      data = data,
      coords = coords,
      panel_params = panel_params,
      coord = coord,
      stack_offset = stack_offset,
      box_size = box_size
    )
    fill <- scales::alpha(data$fill, data$alpha)
    colour <- scales::alpha(data$colour, data$alpha)

    box_grob <- grid::rectGrob(
      x = x_unit,
      y = y_unit,
      width = grid::unit(box_size, "mm"),
      height = grid::unit(box_size, "mm"),
      gp = grid::gpar(
        col = colour,
        fill = fill,
        lwd = data$linewidth * ggplot2::.pt,
        lty = data$linetype,
        linejoin = "mitre"
      ),
      name = "genebox-boxes"
    )

    strand <- as.character(data$strand)
    arrow_rows <- which(!is.na(strand) & strand %in% c("+", "-"))
    arrow_grob <- zeroGrob()
    if (length(arrow_rows) > 0L) {
      scale_direction <- .genebox_x_orientation(panel_params, coord)
      strand_direction <- ifelse(strand[arrow_rows] == "+", 1, -1)
      strip_direction <- .genebox_strip_x_direction(
        data$strip_x_direction[arrow_rows]
      )
      direction <- strand_direction * scale_direction * strip_direction
      shaft_half <- box_size * 0.27
      mapped_arrow_colour <- as.character(data$arrow_colour[arrow_rows])
      auto_arrow_colour <- .genebox_contrast_colour(data$fill[arrow_rows])
      if (is.null(arrow_colour)) {
        use_mapped <- !is.na(mapped_arrow_colour) & nzchar(mapped_arrow_colour)
        arrow_col <- auto_arrow_colour
        arrow_col[use_mapped] <- mapped_arrow_colour[use_mapped]
      } else {
        arrow_col <- rep_len(as.character(arrow_colour), length(arrow_rows))
      }
      arrow_col <- scales::alpha(arrow_col, data$alpha[arrow_rows])

      arrow_grob <- grid::segmentsGrob(
        x0 = x_unit[arrow_rows] - grid::unit(direction * shaft_half, "mm"),
        y0 = y_unit[arrow_rows],
        x1 = x_unit[arrow_rows] + grid::unit(direction * shaft_half, "mm"),
        y1 = y_unit[arrow_rows],
        arrow = grid::arrow(
          angle = 28,
          length = grid::unit(arrow_head_size, "mm"),
          ends = "last",
          type = "closed"
        ),
        gp = grid::gpar(
          col = arrow_col,
          fill = arrow_col,
          lwd = arrow_linewidth * ggplot2::.pt,
          lineend = "round",
          linejoin = "mitre"
        ),
        name = "genebox-arrows"
      )
    }

    ggname("geom_genebox", gTree(children = gList(box_grob, arrow_grob)))
  },
  draw_key = function(data, params, size) {
    grid::rectGrob(
      width = grid::unit(0.65, "npc"),
      height = grid::unit(0.65, "npc"),
      gp = grid::gpar(
        col = scales::alpha(data$colour %||% "black", data$alpha %||% NA_real_),
        fill = scales::alpha(data$fill %||% "grey35", data$alpha %||% NA_real_),
        lwd = (data$linewidth %||% 0.3) * ggplot2::.pt,
        lty = data$linetype %||% 1
      )
    )
  },
  syn_data = function(x, layer) {
    params <- syn_layer_params(layer)
    context <- layer$syn_plot_context %||% NULL
    syn_to_genebox_df(
      x = x,
      species = params$species,
      chr = params$chr,
      subset = params$subset,
      anchor = params$anchor %||% "middle",
      na.rm = params$na.rm %||% FALSE,
      context = context
    )
  },
  syn_default_aes = c(
    "x", "y", "strand", "strip_x_direction", "track", "individual",
    "species", "group", "slot",
    "gene_key", "gene_id", "gene_name", "label", "transcript_id", "transcripts",
    "genomic_x", "genomic_xmin", "genomic_xmax", "anchor_start",
    "anchor_middle", "anchor_end", "anchor_mode", "initiation_anchor_source",
    "stop_anchor_source", "initiation_anchor_fallback", "stop_anchor_fallback",
    "any_anchor_fallback", "reference_gene",
    "reference_gene_name", "homology_hit", "homology_query_hit",
    "homology_reference_hit"
  )
)

.genebox_positive_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
    stop("`", arg, "` must be one positive, finite number.", call. = FALSE)
  }
  as.numeric(x)
}

.genebox_complete_mapping <- function(mapping, data) {
  mapping_exprs <- if (is.null(mapping)) list() else as.list(mapping)
  for (col in c("x", "y", "strand")) {
    if (!col %in% names(mapping_exprs)) {
      mapping_exprs[[col]] <- rlang::sym(col)
    }
  }

  if (is.data.frame(data) || methods::is(data, "DataFrame")) {
    pass_through <- c(
      "strip_x_direction", "slot", "track", "individual", "species",
      "gene_key", "gene_id",
      "gene_name", "label", "transcript_id", "transcripts", "genomic_x",
      "genomic_xmin", "genomic_xmax", "anchor_start", "anchor_middle",
      "anchor_end", "anchor_mode", "initiation_anchor_source",
      "stop_anchor_source", "initiation_anchor_fallback", "stop_anchor_fallback",
      "any_anchor_fallback", "reference_gene", "reference_gene_name",
      "homology_hit", "homology_query_hit", "homology_reference_hit"
    )
    for (col in intersect(pass_through, colnames(data))) {
      if (!col %in% names(mapping_exprs)) {
        mapping_exprs[[col]] <- rlang::sym(col)
      }
    }
  }

  rlang::inject(ggplot2::aes(!!!mapping_exprs))
}

.genebox_stack_offsets <- function(x, y, box_size, gap_fraction = 0.15) {
  if (length(x) == 0L) {
    return(numeric())
  }
  key <- paste(
    format(as.numeric(x), digits = 17, scientific = FALSE, trim = TRUE),
    format(as.numeric(y), digits = 17, scientific = FALSE, trim = TRUE),
    sep = "\r"
  )
  groups <- split(seq_along(key), key)
  out <- numeric(length(key))
  step <- box_size * (1 + gap_fraction)
  for (rows in groups) {
    if (length(rows) > 1L) {
      out[rows] <- (seq_along(rows) - (length(rows) + 1) / 2) * step
    }
  }
  out
}

.genebox_clamped_y_units <- function(data,
                                     coords,
                                     panel_params,
                                     coord,
                                     stack_offset,
                                     box_size) {
  centers <- grid::unit(coords$y, "native") +
    grid::unit(stack_offset, "mm")
  band_columns <- c(".ggexon_band_ymin", ".ggexon_band_ymax")
  if (!all(band_columns %in% names(data))) {
    return(centers)
  }

  band_ymin <- suppressWarnings(as.numeric(data$.ggexon_band_ymin))
  band_ymax <- suppressWarnings(as.numeric(data$.ggexon_band_ymax))
  valid <- is.finite(band_ymin) & is.finite(band_ymax)
  if (!any(valid)) {
    return(centers)
  }

  lower <- coord$transform(
    data.frame(x = data$x[valid], y = band_ymin[valid]),
    panel_params
  )$y
  upper <- coord$transform(
    data.frame(x = data$x[valid], y = band_ymax[valid]),
    panel_params
  )$y
  lower_native <- pmin(lower, upper)
  upper_native <- pmax(lower, upper)
  half_height <- grid::unit(box_size / 2, "mm")
  valid_rows <- which(valid)
  for (i in seq_along(valid_rows)) {
    row <- valid_rows[[i]]
    inset_lower <- grid::unit(lower_native[[i]], "native") + half_height
    inset_upper <- grid::unit(upper_native[[i]], "native") - half_height
    centers[row] <- grid::unit.pmin(
      grid::unit.pmax(centers[row], inset_lower),
      inset_upper
    )
  }
  centers
}

.genebox_x_orientation <- function(panel_params, coord) {
  direction <- 1
  transformation <- tryCatch(
    panel_params$x$scale$get_transformation(),
    error = function(...) NULL
  )
  if (!is.null(transformation) && is.function(transformation$transform)) {
    transformed <- suppressWarnings(tryCatch(
      transformation$transform(c(1, 2)),
      error = function(...) c(1, 2)
    ))
    if (length(transformed) == 2L && all(is.finite(transformed)) && diff(transformed) < 0) {
      direction <- -direction
    }
  }

  # Modern ggplot2 stores the effective coordinate reversal on each panel.
  # ggexon's selective facet reversal also updates this field, whereas
  # `coord$reverse` is global. Prefer the panel value and consult the coordinate
  # only for compatibility with panel parameter objects that lack `reverse`;
  # applying both would double-count coord_cartesian(reverse = "x").
  panel_reverse <- tryCatch(panel_params$reverse, error = function(...) NULL)
  effective_reverse <- if (is.null(panel_reverse)) {
    tryCatch(coord$reverse, error = function(...) NULL)
  } else {
    panel_reverse
  }
  if (!is.null(effective_reverse) &&
      any(as.character(effective_reverse) %in% c("x", "xy"))) {
    direction <- -direction
  }
  direction
}

.genebox_strip_x_direction <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  invalid <- !is.finite(x) | x == 0
  x[invalid] <- 1
  sign(x)
}

.genebox_contrast_colour <- function(fill) {
  fill <- as.character(fill)
  safe_fill <- fill
  safe_fill[is.na(safe_fill) | !nzchar(safe_fill)] <- "white"
  rgba <- tryCatch(
    farver::decode_colour(safe_fill, to = "rgb", alpha = TRUE),
    error = function(...) NULL
  )
  if (is.null(rgba)) {
    return(rep("black", length(fill)))
  }
  alpha <- rgba[, 4L]
  if (any(alpha > 1, na.rm = TRUE)) {
    alpha <- alpha / 255
  }
  red <- rgba[, 1L] * alpha + 255 * (1 - alpha)
  green <- rgba[, 2L] * alpha + 255 * (1 - alpha)
  blue <- rgba[, 3L] * alpha + 255 * (1 - alpha)
  luminance <- 0.2126 * red + 0.7152 * green + 0.0722 * blue
  ifelse(luminance < 145, "white", "black")
}

#' Prepare gene-box anchors from Syn feature annotations
#'
#' `syn_to_genebox_df()` is the data compiler used by [geom_genebox()] for
#' Syn-backed layers. It is also useful when the selected transcript and anchor
#' provenance need to be inspected or displayed as a table.
#'
#' @param x A [SynIndividual] or [SynSpecies] object.
#' @param species Optional individual selector for `SynSpecies` input.
#' @param chr Optional chromosome or seqname.
#' @param subset Optional numeric length-two genomic window.
#' @param anchor One of `"middle"`, `"start"`, or `"end"`.
#' @param na.rm Suppress the summary warning about transcripts without usable
#'   CDS anchors.
#' @param context Internal Syn plotting context.
#'
#' @return A data frame with one selected protein-coding transcript per gene.
#'   Per-end source and fallback columns distinguish complete, exactly
#'   three-nucleotide codon features from terminal-CDS positional proxies.
#' @keywords internal
syn_to_genebox_df <- function(x,
                              species = NULL,
                              chr = NULL,
                              subset = NULL,
                              anchor = c("middle", "start", "end"),
                              na.rm = FALSE,
                              context = NULL) {
  anchor <- match.arg(anchor)
  species <- resolve_context_species_params(x, species, context)

  if (methods::is(x, "SynSpecies") && length(species %||% character()) > 1L) {
    species <- unique(as.character(species))
    pieces <- lapply(species, function(species_name) {
      syn_to_genebox_df(
        x = x,
        species = species_name,
        chr = chr,
        subset = subset,
        anchor = anchor,
        na.rm = na.rm,
        context = context
      )
    })
    return(dplyr::bind_rows(pieces))
  }

  individual_name <- .context_individual_for_track(x, species, context = context)
  individual <- if (methods::is(x, "SynSpecies") && !individual_name %in% names(individuals(x))) {
    NULL
  } else {
    resolve_syn_individual(x, species = individual_name)
  }
  if (is.null(individual) || !has_syn_annotation_source(individual)) {
    return(data.frame())
  }

  window <- normalize_syn_window_request(
    x = x,
    species = species,
    chr = chr,
    subset = subset,
    allow_missing_subset = TRUE,
    context = context,
    geom = "geom_genebox"
  )
  seed_gr <- query_features(
    individual,
    chr = window$chr,
    start = window$start,
    end = window$end,
    feature_type = NULL,
    all = is_unrestricted_syn_window(window)
  )
  if (length(seed_gr) == 0L) {
    return(data.frame())
  }

  seed_type <- base::tolower(as.character(S4Vectors::mcols(seed_gr)$type))
  seed_tx <- seed_gr[seed_type %in% c("mrna", "transcript")]
  if (length(seed_tx) == 0L) {
    if (!isTRUE(na.rm) && any(seed_type == "gene")) {
      warning(
        "geom_genebox() found gene records but no transcript/mRNA records; ",
        "no gene boxes were produced.",
        call. = FALSE
      )
    }
    return(data.frame())
  }

  seed_tx_ids <- .genebox_primary_transcript_id(seed_tx)
  seed_tx_ids <- unique(seed_tx_ids[!is.na(seed_tx_ids) & nzchar(seed_tx_ids)])
  if (length(seed_tx_ids) == 0L) {
    if (!isTRUE(na.rm)) {
      warning(
        "geom_genebox() could not identify transcripts in the requested annotation window.",
        call. = FALSE
      )
    }
    return(data.frame())
  }

  feature_gr <- query_features(
    individual,
    transcripts = seed_tx_ids,
    feature_type = NULL
  )
  if (length(feature_gr) == 0L) {
    return(data.frame())
  }
  out <- .genebox_compile_coding_transcripts(
    feature_gr,
    anchor = anchor,
    track = as.character(species),
    individual = syn_id(individual),
    na.rm = na.rm
  )
  if (nrow(out) == 0L) {
    return(out)
  }

  if (methods::is(x, "SynSpecies")) {
    out <- .inject_homology_columns(out, homology_annotations(x))
  }
  out$group <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}

.genebox_compile_coding_transcripts <- function(feature_gr,
                                                 anchor = "middle",
                                                 track,
                                                 individual,
                                                 na.rm = FALSE) {
  feature_type <- base::tolower(as.character(S4Vectors::mcols(feature_gr)$type))
  tx_gr <- feature_gr[feature_type %in% c("mrna", "transcript")]
  cds_gr <- feature_gr[feature_type == "cds"]
  start_gr <- feature_gr[feature_type == "start_codon"]
  stop_gr <- feature_gr[feature_type == "stop_codon"]
  gene_gr <- feature_gr[feature_type == "gene"]
  if (length(tx_gr) == 0L) {
    return(data.frame())
  }

  tx_ids <- .genebox_primary_transcript_id(tx_gr)
  valid_id <- !is.na(tx_ids) & nzchar(tx_ids)
  tx_gr <- tx_gr[valid_id]
  tx_ids <- tx_ids[valid_id]
  if (length(tx_gr) == 0L) {
    return(data.frame())
  }

  tx_key <- .genebox_normalize_transcript_id(tx_ids)
  keep_unique <- !duplicated(tx_key)
  tx_gr <- tx_gr[keep_unique]
  tx_ids <- tx_ids[keep_unique]
  tx_key <- tx_key[keep_unique]
  tx_meta <- S4Vectors::mcols(tx_gr)
  gene_ids <- .coalesce_character_cols(tx_meta, c("gene_id", "Parent", "gene", "gene_name"))
  gene_names <- .coalesce_character_cols(tx_meta, c("gene_name", "gene", "Name"))
  missing_gene <- is.na(gene_ids) | !nzchar(gene_ids)
  gene_ids[missing_gene] <- tx_ids[missing_gene]

  cds_membership <- .genebox_transcript_membership(cds_gr)
  start_membership <- .genebox_transcript_membership(start_gr)
  stop_membership <- .genebox_transcript_membership(stop_gr)
  candidates <- vector("list", length(tx_gr))
  valid <- logical(length(tx_gr))

  for (i in seq_along(tx_gr)) {
    if (!.genebox_transcript_is_protein_coding(tx_meta, i)) {
      next
    }
    key <- tx_key[[i]]
    one_cds <- cds_gr[vapply(cds_membership, function(ids) key %in% ids, logical(1))]
    if (length(one_cds) == 0L) {
      next
    }
    strand <- unique(as.character(BiocGenerics::strand(one_cds)))
    strand <- strand[!is.na(strand) & strand %in% c("+", "-")]
    cds_seqnames <- unique(as.character(GenomeInfoDb::seqnames(one_cds)))
    tx_seqname <- as.character(GenomeInfoDb::seqnames(tx_gr))[[i]]
    if (length(strand) != 1L || length(cds_seqnames) != 1L ||
        !identical(cds_seqnames[[1L]], tx_seqname) ||
        sum(IRanges::width(one_cds)) < 3L) {
      next
    }
    strand <- strand[[1L]]

    one_start <- start_gr[vapply(start_membership, function(ids) key %in% ids, logical(1))]
    one_stop <- stop_gr[vapply(stop_membership, function(ids) key %in% ids, logical(1))]
    has_explicit_start <- .genebox_complete_codon_feature(one_start)
    has_explicit_stop <- .genebox_complete_codon_feature(one_stop)
    start_anchor <- if (has_explicit_start) {
      .genebox_nth_transcribed_base(one_start, 2L, strand = strand)
    } else {
      .genebox_nth_transcribed_base(one_cds, 2L, strand = strand)
    }
    stop_anchor <- if (has_explicit_stop) {
      .genebox_nth_transcribed_base(one_stop, 2L, strand = strand)
    } else {
      cds_width <- sum(IRanges::width(one_cds))
      .genebox_nth_transcribed_base(one_cds, cds_width - 1L, strand = strand)
    }
    if (!is.finite(start_anchor) || !is.finite(stop_anchor)) {
      next
    }

    coding_ranges <- c(
      one_cds,
      if (has_explicit_start) one_start else one_start[0L],
      if (has_explicit_stop) one_stop else one_stop[0L]
    )
    coding_xmin <- min(IRanges::start(coding_ranges))
    coding_xmax <- max(IRanges::end(coding_ranges))
    gene_info <- .genebox_gene_info(
      gene_gr,
      gene_id = gene_ids[[i]],
      fallback_name = gene_names[[i]]
    )
    middle_anchor <- (start_anchor + stop_anchor) / 2
    selected_x <- switch(
      anchor,
      start = start_anchor,
      middle = middle_anchor,
      end = stop_anchor
    )
    candidates[[i]] <- data.frame(
      x = as.numeric(selected_x),
      genomic_x = as.numeric(selected_x),
      anchor_start = as.numeric(start_anchor),
      anchor_middle = as.numeric(middle_anchor),
      anchor_end = as.numeric(stop_anchor),
      anchor_mode = anchor,
      initiation_anchor_source = if (has_explicit_start) {
        "explicit_start_codon"
      } else {
        "terminal_CDS_positional_proxy"
      },
      stop_anchor_source = if (has_explicit_stop) {
        "explicit_stop_codon"
      } else {
        "terminal_CDS_positional_proxy"
      },
      initiation_anchor_fallback = !has_explicit_start,
      stop_anchor_fallback = !has_explicit_stop,
      any_anchor_fallback = !has_explicit_start || !has_explicit_stop,
      y = 1,
      strand = strand,
      seqnames = as.character(GenomeInfoDb::seqnames(tx_gr))[[i]],
      genomic_xmin = as.numeric(IRanges::start(tx_gr)[[i]]),
      genomic_xmax = as.numeric(IRanges::end(tx_gr)[[i]]),
      coding_xmin = as.numeric(coding_xmin),
      coding_xmax = as.numeric(coding_xmax),
      gene_id = gene_info$gene_id,
      gene_key = gene_info$gene_id,
      gene_name = gene_info$gene_name,
      label = gene_info$gene_name,
      transcript_id = tx_ids[[i]],
      transcripts = tx_ids[[i]],
      transcript_start = as.numeric(IRanges::start(tx_gr)[[i]]),
      transcript_end = as.numeric(IRanges::end(tx_gr)[[i]]),
      transcript_span = as.numeric(IRanges::width(tx_gr)[[i]]),
      track = track,
      individual = individual,
      species = track,
      slot = .genebox_metadata_scalar(tx_meta, "slot", i),
      stringsAsFactors = FALSE
    )
    valid[[i]] <- TRUE
  }

  invalid_count <- sum(!valid)
  valid_gene_keys <- unique(gene_ids[valid])
  all_gene_keys <- unique(gene_ids)
  lost_gene_count <- length(setdiff(all_gene_keys, valid_gene_keys))
  if (invalid_count > 0L && !isTRUE(na.rm)) {
    warning(
      "geom_genebox() omitted ", invalid_count,
      " transcript(s) without usable CDS/codon anchors; ", lost_gene_count,
      " gene(s) had no usable coding transcript.",
      call. = FALSE
    )
  }
  candidates <- Filter(Negate(is.null), candidates)
  if (length(candidates) == 0L) {
    return(data.frame())
  }
  candidates <- do.call(rbind, candidates)

  candidates$.row_id <- seq_len(nrow(candidates))
  order_idx <- order(
    candidates$gene_key,
    -candidates$transcript_span,
    candidates$transcript_id,
    candidates$transcript_start,
    candidates$.row_id
  )
  candidates <- candidates[order_idx, , drop = FALSE]
  candidates <- candidates[!duplicated(candidates$gene_key), , drop = FALSE]
  candidates <- candidates[order(candidates$seqnames, candidates$x, candidates$gene_key), , drop = FALSE]
  candidates$.row_id <- NULL
  rownames(candidates) <- NULL
  candidates
}

.genebox_primary_transcript_id <- function(gr) {
  if (length(gr) == 0L) {
    return(character())
  }
  .coalesce_character_cols(
    S4Vectors::mcols(gr),
    c("transcript_id", "ID", "Name")
  )
}

.genebox_normalize_transcript_id <- function(x) {
  x <- trimws(as.character(x))
  sub("^(transcript:|mRNA:|rna:|transcript-|mRNA-|rna-)", "", x, perl = TRUE)
}

.genebox_transcript_is_protein_coding <- function(meta, i) {
  for (columns in list(
    c("transcript_biotype", "transcript_type"),
    "biotype",
    c("gene_biotype", "gene_type")
  )) {
    values <- unlist(lapply(intersect(columns, colnames(meta)), function(column) {
      unlist(meta[[column]][[i]], recursive = TRUE, use.names = FALSE)
    }), recursive = TRUE, use.names = FALSE)
    values <- base::tolower(trimws(as.character(values)))
    values <- values[!is.na(values) & nzchar(values)]
    if (length(values) > 0L) {
      values <- gsub("[- ]", "_", values)
      return(any(values == "protein_coding"))
    }
  }
  # GFF3 files often omit biotype attributes. In that case, downstream CDS
  # validation is the available evidence that the transcript is coding.
  TRUE
}

.genebox_transcript_membership <- function(gr) {
  if (length(gr) == 0L) {
    return(list())
  }
  meta <- S4Vectors::mcols(gr)
  lapply(seq_len(length(gr)), function(i) {
    values <- character()
    for (col in c("transcript_id", "Parent")) {
      if (!col %in% colnames(meta)) {
        next
      }
      value <- meta[[col]][[i]]
      value <- unlist(value, recursive = TRUE, use.names = FALSE)
      if (length(value) > 0L) {
        values <- c(values, as.character(value))
      }
    }
    values <- trimws(unlist(strsplit(values, ",", fixed = TRUE), use.names = FALSE))
    values <- values[!is.na(values) & nzchar(values)]
    unique(.genebox_normalize_transcript_id(values))
  })
}

.genebox_nth_transcribed_base <- function(gr, n, strand = NULL) {
  if (length(gr) == 0L || !is.numeric(n) || length(n) != 1L || !is.finite(n)) {
    return(NA_real_)
  }
  n <- as.integer(n)
  total_width <- sum(IRanges::width(gr))
  if (n < 1L || n > total_width) {
    return(NA_real_)
  }
  if (is.null(strand)) {
    strand <- unique(as.character(BiocGenerics::strand(gr)))
    strand <- strand[!is.na(strand) & strand %in% c("+", "-")]
    if (length(strand) != 1L) {
      return(NA_real_)
    }
    strand <- strand[[1L]]
  }

  order_idx <- if (identical(strand, "-")) {
    order(IRanges::end(gr), IRanges::start(gr), decreasing = TRUE)
  } else {
    order(IRanges::start(gr), IRanges::end(gr))
  }
  gr <- gr[order_idx]
  widths <- IRanges::width(gr)
  range_idx <- which(cumsum(widths) >= n)[[1L]]
  previous_width <- if (range_idx == 1L) 0L else sum(widths[seq_len(range_idx - 1L)])
  within_range <- n - previous_width
  if (identical(strand, "-")) {
    as.numeric(IRanges::end(gr)[[range_idx]] - within_range + 1L)
  } else {
    as.numeric(IRanges::start(gr)[[range_idx]] + within_range - 1L)
  }
}

.genebox_complete_codon_feature <- function(gr) {
  length(gr) > 0L && sum(IRanges::width(gr)) == 3L
}

.genebox_gene_info <- function(gene_gr, gene_id, fallback_name = NA_character_) {
  gene_id <- as.character(gene_id)
  fallback_name <- as.character(fallback_name)
  if (length(gene_gr) == 0L) {
    name <- if (!is.na(fallback_name) && nzchar(fallback_name)) fallback_name else gene_id
    return(list(gene_id = gene_id, gene_name = name))
  }

  meta <- S4Vectors::mcols(gene_gr)
  ids <- .coalesce_character_cols(meta, c("gene_id", "ID", "Name", "gene_name"))
  normalized_target <- .normalize_gene_id(gene_id)
  hit <- which(.normalize_gene_id(ids) == normalized_target)
  if (length(hit) == 0L) {
    name <- if (!is.na(fallback_name) && nzchar(fallback_name)) fallback_name else gene_id
    return(list(gene_id = gene_id, gene_name = name))
  }
  i <- hit[[1L]]
  resolved_id <- ids[[i]]
  names <- .coalesce_character_cols(meta, c("plot_label", "gene_name", "Name", "gene_id", "ID"))
  resolved_name <- names[[i]]
  if (is.na(resolved_id) || !nzchar(resolved_id)) {
    resolved_id <- gene_id
  }
  if (is.na(resolved_name) || !nzchar(resolved_name)) {
    resolved_name <- if (!is.na(fallback_name) && nzchar(fallback_name)) fallback_name else resolved_id
  }
  list(gene_id = resolved_id, gene_name = resolved_name)
}

.genebox_metadata_scalar <- function(meta, column, i) {
  if (!column %in% colnames(meta)) {
    return(NA_character_)
  }
  value <- unlist(meta[[column]][[i]], recursive = TRUE, use.names = FALSE)
  value <- as.character(value)
  value <- value[!is.na(value) & nzchar(value)]
  if (length(value) == 0L) NA_character_ else value[[1L]]
}
