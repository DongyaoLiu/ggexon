#' Compile gene-tag rows for a rectangular ggtree panel
#'
#' `compile_ggtree_genetag()` extracts rectangular-layout tip positions from a
#' ggtree plot or tree object, matches tip labels to `SynSpecies` individuals,
#' and returns one row per gene feature. The first column is `id`, matching
#' ggtree's `facet_plot()` convention.
#'
#' @param x A `SynSpecies` object.
#' @param tree Optional tree object accepted by `ggtree::ggtree()`.
#' @param tree_plot Optional existing `ggtree` plot. If supplied, `tree` is
#'   ignored. When both `tree` and `tree_plot` are omitted, stored values on
#'   `x` are used when present.
#' @param layout ggtree layout. Currently only `"rectangular"` is supported.
#' @param individual Optional individual selector. When named, names are tree
#'   tip labels and values are `SynSpecies` individual ids. When unnamed, values
#'   are used as both tip labels and individual ids. When `NULL`, matching uses
#'   identical tree-tip labels and individual ids.
#' @param chr Optional chromosome/seqname. May be a scalar or a named vector/list
#'   keyed by tree tip or individual id.
#' @param start,end Optional coordinate bounds. May be scalar or named by tree
#'   tip or individual id.
#' @param subset Optional numeric length-2 bounds. May be a scalar vector or a
#'   named list keyed by tree tip or individual id. Overrides `start` and `end`.
#' @param feature_type Feature type passed to [query_features()]. Defaults to
#'   `"gene"`.
#' @param inter_genetic,exon_length Deprecated x-layout arguments. Only
#'   `"scaled"` is supported. Use [strip_scale_x()] for gene-tag x-coordinate
#'   normalization.
#' @param include_y Logical; when `TRUE`, also include a `y` column copied from
#'   `tree_y`. Keep the default `FALSE` for `ggtree::facet_plot()`, which
#'   injects its own `y` column after tip matching.
#'
#' @return A `data.frame` ready for `ggtree::facet_plot()` and
#'   [geom_genetag()]. It contains `id`, `individual`, `tree_y`, `xmin`,
#'   `xmax`, `strand`, and gene metadata columns.
#' @export
compile_ggtree_genetag <- function(x,
                                   tree = NULL,
                                   tree_plot = NULL,
                                   layout = "rectangular",
                                   individual = NULL,
                                   chr = NULL,
                                   start = NULL,
                                   end = NULL,
                                   subset = NULL,
                                   feature_type = "gene",
                                   inter_genetic = c("scaled", "union"),
                                   exon_length = c("scaled", "union"),
                                   include_y = FALSE) {
  if (!methods::is(x, "SynSpecies")) {
    stop("`compile_ggtree_genetag()` expects a SynSpecies object.", call. = FALSE)
  }
  if (!identical(layout, "rectangular")) {
    stop("Only `layout = \"rectangular\"` is currently supported.", call. = FALSE)
  }
  inter_genetic <- match.arg(inter_genetic)
  exon_length <- match.arg(exon_length)
  .genetag_abort_layout_mode(inter_genetic = inter_genetic, exon_length = exon_length)
  tree_inputs <- .resolve_synspecies_tree_inputs(x, tree = tree, tree_plot = tree_plot)
  tree <- tree_inputs$tree
  tree_plot <- tree_inputs$tree_plot

  tip_data <- .ggtree_rectangular_tip_data(tree = tree, tree_plot = tree_plot, layout = layout)
  tip_map <- .genetag_tip_individual_map(
    tip_labels = tip_data$label,
    available_individuals = names(individuals(x)),
    individual = individual
  )
  if (nrow(tip_map) == 0L) {
    return(.empty_ggtree_genetag_df(include_y = include_y))
  }

  rows <- vector("list", nrow(tip_map))
  for (i in seq_len(nrow(tip_map))) {
    tip_id <- tip_map$id[[i]]
    individual_id <- tip_map$individual[[i]]
    tip_row <- tip_data[match(tip_id, tip_data$label), , drop = FALSE]
    individual_obj <- individuals(x)[[individual_id]]
    window <- .genetag_window_for_individual(
      individual = individual_obj,
      tip_id = tip_id,
      individual_id = individual_id,
      chr = chr,
      start = start,
      end = end,
      subset = subset
    )

    gene_gr <- query_features(
      individual_obj,
      chr = window$chr,
      start = window$start,
      end = window$end,
      feature_type = feature_type,
      all = is.null(window$chr) && is.null(window$start) && is.null(window$end)
    )
    homology_query_aliases <- if (.genetag_feature_type_is_gene(feature_type)) {
      .genetag_homology_query_aliases(
        individual = individual_obj,
        gene_gr = gene_gr,
        window = window
      )
    } else {
      rep(NA_character_, length(gene_gr))
    }
    rows[[i]] <- .genetag_gr_to_df(
      gene_gr = gene_gr,
      id = tip_id,
      individual = individual_id,
      tree_node = tip_row$node[[1L]],
      tree_x = tip_row$x[[1L]],
      tree_y = tip_row$y[[1L]],
      include_y = include_y,
      homology_query_aliases = homology_query_aliases
    )
  }

  out <- do.call(rbind, rows)
  if (is.null(out) || nrow(out) == 0L) {
    return(.empty_ggtree_genetag_df(include_y = include_y))
  }
  rownames(out) <- NULL
  out <- .inject_homology_columns(out, homology_annotations(x))
  out$group <- seq_len(nrow(out))
  out
}

#' Compile rectangular ggtree branches for a ggexon tree panel
#'
#' `compile_ggtree_rectangular_segments()` converts a rectangular ggtree plot or
#' tree object into plain segment rows. The returned data can be drawn with
#' `ggplot2::geom_segment()` inside `ggexon()` and aligned to a gene-tag panel
#' with [facet_genomics()].
#'
#' @param tree Optional tree object accepted by `ggtree::ggtree()`.
#' @param tree_plot Optional existing `ggtree` plot. If supplied, `tree` is
#'   ignored.
#' @param layout ggtree layout. Currently only `"rectangular"` is supported.
#' @param track Facet-track value assigned to all branch segments.
#'
#' @return A `data.frame` with `track`, `x`, `xend`, `y`, `yend`, `node`,
#'   `parent`, `segment`, `isTip`, and `label` columns.
#' @export
compile_ggtree_rectangular_segments <- function(tree = NULL,
                                                tree_plot = NULL,
                                                layout = "rectangular",
                                                track = "Tree") {
  if (!identical(layout, "rectangular")) {
    stop("Only `layout = \"rectangular\"` is currently supported.", call. = FALSE)
  }
  tree_data <- .ggtree_rectangular_plot_data(tree = tree, tree_plot = tree_plot, layout = layout)
  .ggtree_rectangular_segments_from_data(tree_data, track = track)
}

.genetag_abort_layout_mode <- function(inter_genetic = "scaled", exon_length = "scaled") {
  if (!identical(inter_genetic, "scaled") || !identical(exon_length, "scaled")) {
    stop(
      "`inter_genetic = \"union\"` and `exon_length = \"union\"` are no longer ",
      "handled by `compile_ggtree_genetag()`. Use `strip_scale_x()` to modify ",
      "gene-tag x coordinates.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' @export
ggplot_add.ggtree <- function(object, plot, ...) {
  if (!inherits(plot, "ggexon")) {
    stop(
      "Adding a ggtree object with `+` is supported only for `ggexon()` plots. ",
      "Use ggtree's own layer grammar inside ggtree plots.",
      call. = FALSE
    )
  }

  tree_segments <- compile_ggtree_rectangular_segments(tree_plot = object)
  plot + ggplot2::geom_segment(
    data = tree_segments,
    mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE
  )
}

#' Draw gene tags as exon bodies with strand-direction triangles
#'
#' `geom_genetag()` draws each stranded gene as a rectangular exon-like body
#' plus a symmetric terminal triangle. The triangle apex points toward the
#' strand-specific end of the feature. It is designed for gene-level summaries,
#' including ggtree side panels generated with `ggtree::facet_plot()`.
#'
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard
#'   ggplot2 layer arguments. `inherit.aes` defaults to `FALSE` so the layer can
#'   be used cleanly in `ggtree::facet_plot()` side panels.
#' @param exon_height Total tag height in y-axis units. Defaults to `0.8`.
#' @param height Deprecated-compatible alias for `exon_height`.
#' @param arrow_width Optional width of the terminal triangle in x-axis units.
#'   When `NULL`, width is calculated from `arrow_fraction`.
#' @param arrow_fraction Fraction of each gene span used for the terminal
#'   triangle when `arrow_width` is `NULL`.
#' @param tag_arrow_fill,tag_arrow_colour Optional fixed fill and outline for
#'   the terminal strand-direction triangle. When `NULL`, the triangle uses the
#'   same inherited aesthetics as the gene tag body.
#' @param gene_layout Gene-body overlap layout. `"single"` keeps all gene tags
#'   on one baseline. `"stack"` assigns overlapping gene spans to separate
#'   vertical lanes. `"nested"` also assigns lanes, ordering containing spans
#'   before contained spans so embedded genes are visible inside broad
#'   gene-level annotations.
#' @param gene_lane_gap Gap between stacked gene-body lanes, as a fraction of
#'   `exon_height`.
#' @param species Optional species / individual identifier when `data` is a
#'   `SynSpecies`.
#' @param chr Optional chromosome / seqname restriction when `data` is
#'   Syn-backed.
#' @param subset Optional numeric length-2 genomic window to keep.
#' @param feature_type Feature type passed to [query_features()]. Defaults to
#'   `"gene"`.
#' @param show_label Logical; draw gene labels. Defaults to `TRUE`.
#' @param label_position Label placement mode. `"auto"` draws labels inside
#'   tags when they fit and falls back outside otherwise; `"inside"` keeps the
#'   previous inside-only behavior; `"outside"` draws all labels outside the
#'   tag; `"none"` suppresses labels.
#' @param label_direction Outside label position. Accepts `"top"`, `"bottom"`,
#'   `"center"`, or colon-delimited combinations such as `"top:bottom"`.
#'   Outside fallback treats `"center"` labels that do not fit as `"top"`.
#' @param label_offset_fraction Distance between the tag and outside label line,
#'   as a fraction of `exon_height`.
#' @param label_link Logical; draw leader links for outside labels.
#' @param label_link_type Leader line style: `"straight"`, `"elbow"`, or
#'   `"spline"`.
#' @param collapse_tandem When `TRUE`, consecutive outside labels with the same
#'   displayed `label` in a track are collapsed into one label.
#' @param check_overlap Logical passed to text drawing for opt-in label overlap
#'   suppression.
#' @param label_max_lanes Maximum number of vertical lanes available for
#'   outside labels on each side of a track. Defaults to `3`.
#' @param label_panel_width Panel width used for label layout. The default
#'   `"auto"` measures the final panel viewport at draw time. A positive
#'   numeric value is interpreted as millimetres.
#' @param label_genes Optional gene selector for partial labeling. A character
#'   vector applies globally; a named list applies per track.
#' @param label_filter Semantic label filter. Multiple values are OR-combined.
#'   `"all"` preserves the default behavior; `"homology_hit"` labels both
#'   query-side hits and matching visible reference genes; `"homology_query_hit"`
#'   and `"homology_reference_hit"` label only one side; `"species_specific"`
#'   labels non-homologous non-reference genes; `"homology_anchor"`,
#'   `"homology_visible"`, and `"homology_offtrack"` require strip-scale
#'   metadata.
#' @param label_match_by Columns used to match `label_genes`. `"auto"` checks
#'   common gene and reference-gene identifier columns.
#' @param label_match Matching mode for `label_genes`: `"exact"` or `"regex"`.
#' @param label_size,label_colour,label_alpha,label_family,label_fontface,label_lineheight
#'   Fixed label styling used when `show_label = TRUE`. These can also be mapped
#'   as aesthetics with names such as `aes(label_colour = ...)`.
#' @param label_link_colour,label_link_linewidth,label_link_linetype,label_link_alpha
#'   Fixed leader-link styling for outside labels. These can also be mapped as
#'   aesthetics with the same names.
#' @param panel_width_mm,panel_width_inch Optional panel width for estimating
#'   whether labels fit inside transformed gene tags.
#'
#' @return A ggplot layer.
#' @export
geom_genetag <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         ...,
                         exon_height = NULL,
	                         height = NULL,
	                         arrow_width = NULL,
	                         arrow_fraction = 0.18,
                         tag_arrow_fill = NULL,
                         tag_arrow_colour = NULL,
                         gene_layout = "single",
                         gene_lane_gap = 0.15,
	                         species = NULL,
	                         chr = NULL,
	                         subset = NULL,
	                         feature_type = "gene",
                         show_label = TRUE,
                         label_position = NULL,
                         label_direction = NULL,
                         label_offset_fraction = NULL,
                         label_link = NULL,
                         label_link_type = NULL,
                         collapse_tandem = NULL,
                         check_overlap = FALSE,
                         label_max_lanes = NULL,
                         label_panel_width = NULL,
                         label_genes = NULL,
                         label_filter = NULL,
                         label_match_by = NULL,
                         label_match = NULL,
                         label_size = NULL,
                         label_colour = NULL,
                         label_alpha = NULL,
                         label_family = NULL,
                         label_fontface = NULL,
                         label_lineheight = NULL,
                         label_link_colour = NULL,
                         label_link_linewidth = NULL,
                         label_link_linetype = NULL,
                         label_link_alpha = NULL,
                         panel_width_mm = NULL,
                         panel_width_inch = NULL,
	                         na.rm = FALSE,
	                         show.legend = NA,
	                         inherit.aes = FALSE) {
  mapping <- .genetag_complete_mapping(mapping, data, show_label = show_label)
  params <- Filter(Negate(is.null), list(
    ...,
    exon_height = exon_height,
	    height = height,
	    arrow_width = arrow_width,
	    arrow_fraction = arrow_fraction,
    tag_arrow_fill = tag_arrow_fill,
    tag_arrow_colour = tag_arrow_colour,
    gene_layout = gene_layout,
    gene_lane_gap = gene_lane_gap,
	    species = species,
    chr = chr,
    subset = subset,
    feature_type = feature_type,
    show_label = show_label,
    label_position = label_position,
    label_direction = label_direction,
    label_offset_fraction = label_offset_fraction,
    label_link = label_link,
    label_link_type = label_link_type,
    collapse_tandem = collapse_tandem,
    check_overlap = check_overlap,
    label_max_lanes = label_max_lanes,
    label_panel_width = label_panel_width,
    label_genes = label_genes,
    label_filter = label_filter,
    label_match_by = label_match_by,
    label_match = label_match,
    label_size = label_size,
    label_colour = label_colour,
    label_alpha = label_alpha,
    label_family = label_family,
    label_fontface = label_fontface,
    label_lineheight = label_lineheight,
    label_link_colour = label_link_colour,
    label_link_linewidth = label_link_linewidth,
    label_link_linetype = label_link_linetype,
    label_link_alpha = label_link_alpha,
    panel_width_mm = panel_width_mm,
    panel_width_inch = panel_width_inch,
    na.rm = na.rm
  ))
  layer(
    data = data,
    mapping = mapping,
    geom = GeomGeneTag,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params,
    layer_class = LayerSyn
  )
}

GeomGeneTag <- ggproto(
  "GeomGeneTag",
  Geom,
  ggexon_panel_role = "annotation",
  required_aes = c("xmin", "xmax", "y", "strand"),
	  default_aes = aes(
	    colour = "black",
	    fill = "grey35",
	    linewidth = 0.25,
	    linetype = 1,
	    alpha = NA,
    ymin = NA_real_,
    ymax = NA_real_,
    label = NA_character_,
    label_colour = "black",
    label_size = 3,
    label_alpha = NA_real_,
    label_family = "sans",
    label_fontface = 1,
    label_lineheight = 1.2,
    label_link_colour = "grey60",
    label_link_linewidth = 0.5,
    label_link_linetype = "solid",
    label_link_alpha = NA_real_,
    track = NA_character_,
    individual = NA_character_,
    id = NA_character_,
    species = NA_character_,
    gene_key = NA_character_,
    gene_id = NA_character_,
    gene_name = NA_character_,
    gene = NA_character_,
    Name = NA_character_,
    ID = NA_character_,
    transcripts = NA_character_,
    genomic_xmin = NA_real_,
    genomic_xmax = NA_real_,
    slot = NA_character_,
    reference_gene = NA_character_,
    reference_gene_name = NA_character_,
    homology_hit = NA,
    homology_query_hit = NA,
    homology_reference_hit = NA,
    is_homology_reference_track = NA,
    homology_anchor = NA,
    visual_class = NA_character_,
    slot_type = NA_character_
	  ),
		  extra_params = c(
		    "na.rm", "exon_height", "height", "arrow_width", "arrow_fraction",
    "tag_arrow_fill", "tag_arrow_colour",
    "gene_layout", "gene_lane_gap",
		    "species", "chr", "subset", "feature_type", "show_label",
    "label_position", "label_direction", "label_offset_fraction",
    "label_link", "label_link_type", "collapse_tandem", "check_overlap",
    "label_max_lanes", "label_panel_width", "label_genes", "label_filter",
    "label_match_by", "label_match", "panel_width_mm", "panel_width_inch",
    "ggexon_output_size"
	  ),
  default_params = function() {
    list(
      exon_height = NULL,
	      height = NULL,
	      arrow_width = NULL,
	      arrow_fraction = 0.18,
      tag_arrow_fill = NULL,
      tag_arrow_colour = NULL,
      gene_layout = "single",
      gene_lane_gap = 0.15,
	      species = NULL,
      chr = NULL,
	      subset = NULL,
	      feature_type = "gene",
      show_label = TRUE,
      label_position = "auto",
      label_direction = "top",
      label_offset_fraction = 0.3,
      label_link = TRUE,
      label_link_type = "straight",
      collapse_tandem = FALSE,
      check_overlap = FALSE,
      label_max_lanes = 3L,
      label_panel_width = "auto",
      label_genes = NULL,
      label_filter = "all",
      label_match_by = "auto",
      label_match = "exact",
      panel_width_mm = NULL,
      panel_width_inch = NULL,
      ggexon_output_size = NULL,
	      na.rm = FALSE
	    )
	  },
	  setup_data = function(data, params) {
		    exon_height <- params$exon_height %||% 0.8
    body_height <- .genetag_effective_height(
      exon_height = params$exon_height,
      height = params$height
    )
    gene_layout <- .genetag_gene_layout(params$gene_layout %||% "single")
    gene_lane_gap <- .genetag_gene_lane_gap(params$gene_lane_gap %||% 0.15)
	    if (!"genomic_xmin" %in% names(data)) data$genomic_xmin <- data$xmin
    if (!"genomic_xmax" %in% names(data)) data$genomic_xmax <- data$xmax
    if (!"genomic_start" %in% names(data)) data$genomic_start <- data$genomic_xmin
    if (!"genomic_end" %in% names(data)) data$genomic_end <- data$genomic_xmax
    if (!"gene_key" %in% names(data)) data$gene_key <- .genetag_gene_key(data)
    if (!"label" %in% names(data)) data$label <- .genetag_label(data)
	    label_position <- .genetag_label_position(
	      params$label_position %||% "auto",
	      show_label = params$show_label %||% TRUE
	    )
    data <- .genetag_apply_gene_lanes(
      data = data,
      gene_layout = gene_layout,
      exon_height = exon_height,
      gene_lane_gap = gene_lane_gap
    )
	    data$ymin <- data$y - exon_height / 2
    data$ymax <- data$y + exon_height / 2
    data$.ggexon_body_ymin <- data$y - body_height / 2
    data$.ggexon_body_ymax <- data$y + body_height / 2
    if (!identical(label_position, "none") && !identical(label_position, "inside")) {
      label_space <- .genetag_label_reserved_space(
        exon_height = exon_height,
        label_offset_fraction = params$label_offset_fraction %||% 0.3,
        label_max_lanes = params$label_max_lanes %||% 3L
      )
      positions <- .parse_label_positions(params$label_direction %||% "top")
      if ("top" %in% positions || "center" %in% positions) {
        data$ymax <- data$ymax + label_space
      }
      if ("bottom" %in% positions) {
        data$ymin <- data$ymin - label_space
      }
    }
	    data
	  },
  handle_na = function(data, params) {
    missing <- is.na(data$xmin) | is.na(data$xmax) | is.na(data$y) | is.na(data$strand)
    if (any(missing)) {
      if (!isTRUE(params$na.rm)) {
        warning(
          "Removed ", sum(missing), " row(s) containing missing values in geom_genetag().",
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
                        flipped_aes = FALSE,
                        exon_height = NULL,
	                         height = NULL,
	                         arrow_width = NULL,
		                        arrow_fraction = 0.18,
                         tag_arrow_fill = NULL,
                         tag_arrow_colour = NULL,
                         gene_layout = "single",
                         gene_lane_gap = 0.15,
	                         show_label = TRUE,
                         label_position = "auto",
                         label_direction = "top",
                         label_offset_fraction = 0.3,
                         label_link = TRUE,
                         label_link_type = "straight",
                         collapse_tandem = FALSE,
                         check_overlap = FALSE,
                         label_max_lanes = 3L,
                         label_panel_width = "auto",
                         label_genes = NULL,
                         label_filter = "all",
                         label_match_by = "auto",
                         label_match = "exact",
                         panel_width_mm = NULL,
                         panel_width_inch = NULL,
                         ggexon_output_size = NULL) {
	    if (nrow(data) == 0L) {
	      return(zeroGrob())
	    }
    tag_data <- .genetag_polygon_data(
      data = data,
      exon_height = exon_height,
      height = height,
      arrow_width = arrow_width,
	      arrow_fraction = arrow_fraction
	    )
    tag_grob <- GeomPolygon$draw_panel(tag_data, panel_params, coord)
    arrow_data <- if (!is.null(tag_arrow_fill) || !is.null(tag_arrow_colour)) {
      .genetag_arrow_polygon_data(
        data = data,
        exon_height = exon_height,
        height = height,
        arrow_width = arrow_width,
        arrow_fraction = arrow_fraction
      ) |>
        .apply_transcript_backbone_aes(
          fill = tag_arrow_fill,
          colour = tag_arrow_colour
        )
    } else {
      data.frame()
    }
    arrow_grob <- if (nrow(arrow_data) > 0L) {
      GeomPolygon$draw_panel(arrow_data, panel_params, coord)
    } else {
      zeroGrob()
    }
    label_grob <- .genetag_label_grob(
      data = data,
      panel_params = panel_params,
      coord = coord,
      show_label = show_label,
      label_position = label_position,
      label_direction = label_direction,
      label_offset_fraction = label_offset_fraction,
      label_link = label_link,
      label_link_type = label_link_type,
      collapse_tandem = collapse_tandem,
      check_overlap = check_overlap,
      label_max_lanes = label_max_lanes,
      label_panel_width = label_panel_width,
      label_genes = label_genes,
      label_filter = label_filter,
      label_match_by = label_match_by,
      label_match = label_match,
      exon_height = .genetag_effective_height(exon_height = exon_height, height = height),
      panel_width_mm = panel_width_mm,
      panel_width_inch = panel_width_inch,
      ggexon_output_size = ggexon_output_size
    )
	    ggname("geom_genetag", gTree(children = gList(tag_grob, arrow_grob, label_grob)))
	  },
  draw_key = draw_key_polygon,
  syn_data = function(x, layer) {
    params <- syn_layer_params(layer)
    context <- layer$syn_plot_context %||% NULL
    syn_to_genetag_df(
      x = x,
      species = params$species,
      chr = params$chr,
      subset = params$subset,
      feature_type = params$feature_type %||% "gene",
      context = context
    )
  },
	syn_default_aes = c(
    "xmin", "xmax", "y", "strand", "track", "individual", "id", "species",
    "group", "label", "gene_key", "gene_name", "slot", "reference_gene", "reference_gene_name",
    "homology_hit", "homology_query_hit", "homology_reference_hit",
    "is_homology_reference_track", "homology_anchor", "visual_class",
    "slot_type", "gene_id", "gene",
    "label_colour", "label_size", "label_alpha", "label_family",
    "label_fontface", "label_lineheight", "label_link_colour",
    "label_link_linewidth", "label_link_linetype", "label_link_alpha"
  )
)

.genetag_tip_individual_map <- function(tip_labels, available_individuals, individual = NULL) {
  tip_labels <- unique(as.character(tip_labels))
  available_individuals <- unique(as.character(available_individuals))

  if (is.null(individual)) {
    matched <- intersect(tip_labels, available_individuals)
    return(data.frame(id = matched, individual = matched, stringsAsFactors = FALSE))
  }

  individual <- as.character(individual)
  if (anyNA(individual) || any(!nzchar(individual))) {
    stop("`individual` must contain only non-empty values.", call. = FALSE)
  }
  if (!is.null(names(individual)) && any(nzchar(names(individual)))) {
    ids <- names(individual)
    ids[!nzchar(ids)] <- individual[!nzchar(ids)]
  } else {
    ids <- individual
  }

  missing_tips <- setdiff(ids, tip_labels)
  if (length(missing_tips) > 0L) {
    stop("Tree tip labels not found: ", paste(missing_tips, collapse = ", "), call. = FALSE)
  }
  missing_individuals <- setdiff(individual, available_individuals)
  if (length(missing_individuals) > 0L) {
    stop(
      "SynSpecies individuals not found: ",
      paste(missing_individuals, collapse = ", "),
      call. = FALSE
    )
  }

  data.frame(id = ids, individual = individual, stringsAsFactors = FALSE)
}

.genetag_window_for_individual <- function(individual,
                                           tip_id,
                                           individual_id,
                                           chr = NULL,
                                           start = NULL,
                                           end = NULL,
                                           subset = NULL) {
  subset_value <- .genetag_pick_value(subset, tip_id = tip_id, individual_id = individual_id)
  start_value <- .genetag_pick_value(start, tip_id = tip_id, individual_id = individual_id)
  end_value <- .genetag_pick_value(end, tip_id = tip_id, individual_id = individual_id)
  chr_value <- .genetag_pick_value(chr, tip_id = tip_id, individual_id = individual_id)

  if (!is.null(subset_value)) {
    if (!is.numeric(subset_value) || length(subset_value) != 2L || anyNA(subset_value)) {
      stop("`subset` values must be numeric vectors of length 2.", call. = FALSE)
    }
    start_value <- min(subset_value)
    end_value <- max(subset_value)
  }

  if (!is.null(start_value)) {
    start_value <- .genetag_scalar_numeric(start_value, "start")
  }
  if (!is.null(end_value)) {
    end_value <- .genetag_scalar_numeric(end_value, "end")
  }
  if (!is.null(chr_value)) {
    if (!is.character(chr_value) || length(chr_value) != 1L || is.na(chr_value) || !nzchar(chr_value)) {
      stop("`chr` values must be single non-empty character values.", call. = FALSE)
    }
    chr_value <- resolve_syn_seqname_or_raw(individual, chr_value)
  }

  list(chr = chr_value, start = start_value, end = end_value)
}

.genetag_pick_value <- function(x, tip_id, individual_id) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.list(x) && !is.data.frame(x)) {
    if (!is.null(names(x))) {
      if (tip_id %in% names(x)) {
        return(x[[tip_id]])
      }
      if (individual_id %in% names(x)) {
        return(x[[individual_id]])
      }
    }
    if (length(x) == 1L) {
      return(x[[1L]])
    }
    stop("Named per-tip/per-individual list values are required for multi-value inputs.", call. = FALSE)
  }

  if (!is.null(names(x))) {
    if (tip_id %in% names(x)) {
      return(unname(x[[tip_id]]))
    }
    if (individual_id %in% names(x)) {
      return(unname(x[[individual_id]]))
    }
  }
  if (length(x) == 1L || (is.numeric(x) && length(x) == 2L)) {
    return(unname(x))
  }
  stop("Named per-tip/per-individual values are required for multi-value inputs.", call. = FALSE)
}

.genetag_scalar_numeric <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    stop("`", name, "` values must be single numeric values.", call. = FALSE)
  }
  as.numeric(x)
}

.genetag_gr_to_df <- function(gene_gr,
                              id,
                              individual,
                              tree_node,
                              tree_x,
                              tree_y,
                              include_y = FALSE,
                              homology_query_aliases = NULL) {
  if (length(gene_gr) == 0L) {
    return(.empty_ggtree_genetag_df(include_y = include_y))
  }

  meta <- S4Vectors::mcols(gene_gr)
  gene_ids <- .coalesce_character_cols(meta, c("gene_id", "gene_name", "ID", "Name"))
  gene_names <- .coalesce_character_cols(meta, c("gene_name"))
  gene_labels <- .coalesce_character_cols(meta, c("plot_label", "gene_name", "gene_id", "Name", "ID"))
  gene_ids[is.na(gene_ids) | !nzchar(gene_ids)] <- paste0("gene_", seq_len(length(gene_ids)))[
    is.na(gene_ids) | !nzchar(gene_ids)
  ]
  gene_labels[is.na(gene_labels) | !nzchar(gene_labels)] <- gene_ids[
    is.na(gene_labels) | !nzchar(gene_labels)
  ]
  if (is.null(homology_query_aliases)) {
    homology_query_aliases <- rep(NA_character_, length(gene_gr))
  } else {
    homology_query_aliases <- as.character(homology_query_aliases)
    if (length(homology_query_aliases) != length(gene_gr)) {
      stop("`homology_query_aliases` must match the number of gene ranges.", call. = FALSE)
    }
  }

		  out <- data.frame(
		    id = rep(id, length(gene_gr)),
	    individual = rep(individual, length(gene_gr)),
    node = rep(as.integer(tree_node), length(gene_gr)),
    tree_x = rep(as.numeric(tree_x), length(gene_gr)),
    tree_y = rep(as.numeric(tree_y), length(gene_gr)),
	    chr = as.character(GenomeInfoDb::seqnames(gene_gr)),
	    xmin = IRanges::start(gene_gr),
	    xmax = IRanges::end(gene_gr),
    genomic_xmin = IRanges::start(gene_gr),
    genomic_xmax = IRanges::end(gene_gr),
    genomic_start = IRanges::start(gene_gr),
    genomic_end = IRanges::end(gene_gr),
	    start = IRanges::start(gene_gr),
	    end = IRanges::end(gene_gr),
	    strand = as.character(BiocGenerics::strand(gene_gr)),
	    gene_id = gene_ids,
	    gene_key = gene_ids,
    gene_name = gene_names,
		    gene = gene_labels,
		    label = gene_labels,
    homology_query_aliases = homology_query_aliases,
	    track = rep(individual, length(gene_gr)),
	    stringsAsFactors = FALSE
	  )
  if (isTRUE(include_y)) {
    out$y <- out$tree_y
    out <- out[, c("id", setdiff(names(out), "id")), drop = FALSE]
  }
  out[order(out$xmin, out$xmax, out$gene_id), , drop = FALSE]
}

syn_to_genetag_df <- function(x,
                              species = NULL,
                              chr = NULL,
                              subset = NULL,
                              feature_type = "gene",
                              context = NULL) {
  species <- resolve_context_species_params(x, species, context)

  if (methods::is(x, "SynSpecies") && length(species %||% character()) > 1L) {
    species <- unique(as.character(species))
    return(dplyr::bind_rows(lapply(species, function(species_name) {
      syn_to_genetag_df(
        x = x,
        species = species_name,
        chr = chr,
        subset = subset,
        feature_type = feature_type,
        context = context
      )
    })))
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
    geom = "geom_genetag"
  )

  gene_gr <- query_features(
    individual,
    chr = window$chr,
    start = window$start,
    end = window$end,
    feature_type = feature_type,
    all = is_unrestricted_syn_window(window)
  )
  if (length(gene_gr) == 0L) {
    return(data.frame())
  }
  homology_query_aliases <- if (.genetag_feature_type_is_gene(feature_type)) {
    .genetag_homology_query_aliases(
      individual = individual,
      gene_gr = gene_gr,
      window = window
    )
  } else {
    rep(NA_character_, length(gene_gr))
  }

  out <- .genetag_gr_to_df(
    gene_gr = gene_gr,
    id = species,
    individual = syn_id(individual),
    tree_node = NA_integer_,
    tree_x = NA_real_,
    tree_y = 1,
    include_y = TRUE,
    homology_query_aliases = homology_query_aliases
  )
  out$track <- species
  if (methods::is(x, "SynSpecies")) {
    out <- .inject_homology_columns(out, homology_annotations(x))
  }
  out$group <- seq_len(nrow(out))
  out
}

.genetag_apply_layout_modes <- function(data,
                                        inter_genetic = c("scaled", "union"),
                                        exon_length = c("scaled", "union")) {
  inter_genetic <- match.arg(inter_genetic)
  exon_length <- match.arg(exon_length)
  if (nrow(data) == 0L || (identical(inter_genetic, "scaled") && identical(exon_length, "scaled"))) {
    return(data)
  }
  if (!all(c("xmin", "xmax") %in% names(data))) {
    stop("Gene-tag layout data must contain `xmin` and `xmax` columns.", call. = FALSE)
  }
  if (any(!is.finite(data$xmin)) || any(!is.finite(data$xmax))) {
    stop("Gene-tag layout coordinates must be finite.", call. = FALSE)
  }

  track_col <- .genetag_layout_track_col(data)
  out <- data
  out$genomic_xmin <- out$xmin
  out$genomic_xmax <- out$xmax
  out$genomic_start <- if ("start" %in% names(out)) out$start else out$xmin
  out$genomic_end <- if ("end" %in% names(out)) out$end else out$xmax
  out$.row_id <- seq_len(nrow(out))
  out$.layout_track <- as.character(out[[track_col]])

  sorted <- out[order(out$.layout_track, out$xmin, out$xmax, out$.row_id), , drop = FALSE]
  split_rows <- split(seq_len(nrow(sorted)), sorted$.layout_track)

  sorted$.layout_index <- NA_integer_
  sorted$.feature_width <- pmax(0, sorted$xmax - sorted$xmin)
  sorted$.gap_after <- 0

  for (rows in split_rows) {
    sorted$.layout_index[rows] <- seq_along(rows)
    if (length(rows) > 1L) {
      gaps <- sorted$xmin[rows[-1L]] - sorted$xmax[rows[-length(rows)]]
      sorted$.gap_after[rows[-length(rows)]] <- pmax(0, gaps)
    }
  }

  width_by_index <- tapply(sorted$.feature_width, sorted$.layout_index, max, na.rm = TRUE)
  gap_by_index <- tapply(sorted$.gap_after, sorted$.layout_index, max, na.rm = TRUE)
  base_x <- min(sorted$xmin, na.rm = TRUE)

  for (rows in split_rows) {
    current_x <- base_x
    for (j in seq_along(rows)) {
      row <- rows[[j]]
      layout_index <- sorted$.layout_index[[row]]
      width <- if (identical(exon_length, "union")) {
        width_by_index[[as.character(layout_index)]]
      } else {
        sorted$.feature_width[[row]]
      }
      sorted$xmin[[row]] <- current_x
      sorted$xmax[[row]] <- current_x + width

      if (j < length(rows)) {
        gap <- if (identical(inter_genetic, "union")) {
          gap_by_index[[as.character(layout_index)]]
        } else {
          sorted$.gap_after[[row]]
        }
        current_x <- sorted$xmax[[row]] + gap
      }
    }
  }

  sorted$layout_index <- sorted$.layout_index
  sorted$layout_inter_genetic <- inter_genetic
  sorted$layout_exon_length <- exon_length
  sorted <- sorted[order(sorted$.row_id), , drop = FALSE]
  sorted$.row_id <- NULL
  sorted$.layout_track <- NULL
  sorted$.layout_index <- NULL
  sorted$.feature_width <- NULL
  sorted$.gap_after <- NULL
  rownames(sorted) <- NULL
  sorted
}

.genetag_layout_track_col <- function(data) {
  for (col in c("id", "track", "individual")) {
    if (col %in% names(data)) {
      return(col)
    }
  }
  stop("Gene-tag layout data must contain one of `id`, `track`, or `individual`.", call. = FALSE)
}

.empty_ggtree_genetag_df <- function(include_y = FALSE) {
  out <- data.frame(
    id = character(),
    individual = character(),
    node = integer(),
    tree_x = numeric(),
    tree_y = numeric(),
    chr = character(),
    xmin = numeric(),
    xmax = numeric(),
    genomic_xmin = numeric(),
    genomic_xmax = numeric(),
    genomic_start = numeric(),
    genomic_end = numeric(),
    start = numeric(),
    end = numeric(),
    strand = character(),
    gene_id = character(),
    gene_key = character(),
    gene_name = character(),
    gene = character(),
    label = character(),
    homology_query_aliases = character(),
    track = character(),
    group = integer(),
    stringsAsFactors = FALSE
  )
  if (isTRUE(include_y)) {
    out$y <- numeric()
    out <- out[, c("id", setdiff(names(out), "id")), drop = FALSE]
  }
  out
}

.genetag_feature_type_is_gene <- function(feature_type) {
  feature_type <- as.character(feature_type)
  length(feature_type) == 1L &&
    !is.na(feature_type) &&
    identical(base::tolower(feature_type), "gene")
}

.genetag_homology_query_aliases <- function(individual, gene_gr, window) {
  if (length(gene_gr) == 0L || is.null(window)) {
    return(rep(NA_character_, length(gene_gr)))
  }

  transcript_gr <- .genetag_query_transcript_children(
    individual = individual,
    window = window
  )
  if (length(transcript_gr) == 0L) {
    return(rep(NA_character_, length(gene_gr)))
  }

  gene_meta <- S4Vectors::mcols(gene_gr)
  gene_ids <- .coalesce_character_cols(gene_meta, c("gene_id", "ID", "Name", "gene_name"))
  gene_norm <- .normalize_gene_id(gene_ids)

  tx_meta <- S4Vectors::mcols(transcript_gr)
  parent_ids <- .coalesce_character_cols(tx_meta, c("Parent", "gene_id"))
  parent_norm <- .normalize_gene_id(parent_ids)
  tx_aliases <- .genetag_transcript_alias_values(tx_meta)

  valid <- !is.na(parent_norm) & nzchar(parent_norm) & lengths(tx_aliases) > 0L
  if (!any(valid)) {
    return(rep(NA_character_, length(gene_gr)))
  }

  alias_by_parent <- split(tx_aliases[valid], parent_norm[valid])
  alias_by_parent <- lapply(alias_by_parent, function(x) {
    aliases <- unique(unlist(x, use.names = FALSE))
    aliases[!is.na(aliases) & nzchar(aliases)]
  })

  out <- rep(NA_character_, length(gene_gr))
  for (i in seq_along(gene_norm)) {
    key <- gene_norm[[i]]
    if (is.na(key) || !nzchar(key) || is.null(alias_by_parent[[key]])) {
      next
    }
    out[[i]] <- paste(alias_by_parent[[key]], collapse = "\r")
  }
  out
}

.genetag_query_transcript_children <- function(individual, window) {
  feature_types <- c("mRNA", "transcript")
  pieces <- lapply(feature_types, function(feature_type) {
    query_features(
      individual,
      chr = window$chr,
      start = window$start,
      end = window$end,
      feature_type = feature_type,
      all = is.null(window$chr) && is.null(window$start) && is.null(window$end)
    )
  })
  pieces <- Filter(function(x) length(x) > 0L, pieces)
  if (length(pieces) == 0L) {
    return(GenomicRanges::GRanges())
  }
  if (length(pieces) == 1L) {
    return(pieces[[1L]])
  }
  do.call(c, unname(pieces))
}

.genetag_transcript_alias_values <- function(meta) {
  alias_columns <- intersect(c("transcript_id", "ID", "Name", "transcript_name"), colnames(meta))
  if (length(alias_columns) == 0L) {
    return(rep(list(character()), nrow(meta)))
  }

  lapply(seq_len(nrow(meta)), function(i) {
    values <- unique(unlist(lapply(alias_columns, function(col) {
      as.character(meta[[col]][[i]])
    }), use.names = FALSE))
    values[!is.na(values) & nzchar(values)]
  })
}

.genetag_complete_mapping <- function(mapping, data, show_label = TRUE) {
  mapping_exprs <- if (is.null(mapping)) {
    list()
  } else {
    as.list(mapping)
  }
  for (col in c("xmin", "xmax", "y", "strand")) {
    if (!col %in% names(mapping_exprs)) {
      mapping_exprs[[col]] <- rlang::sym(col)
    }
  }
  if (isTRUE(show_label) && !"label" %in% names(mapping_exprs)) {
    label_col <- .genetag_label_column(data)
    if (!is.null(label_col)) {
      mapping_exprs[["label"]] <- rlang::sym(label_col)
    }
  }
    if (is.data.frame(data)) {
    for (col in c(
      "track", "gene_key", "gene_id", "gene_name", "gene", "Name", "ID", "transcripts",
      "slot",
      "genomic_xmin", "genomic_xmax",
      "individual", "id", "species", "reference_gene", "reference_gene_name",
      "homology_hit", "homology_query_hit", "homology_reference_hit",
      "is_homology_reference_track", "homology_anchor", "visual_class", "slot_type",
      .genetag_label_aesthetics()
    )) {
      if (col %in% names(data) && !col %in% names(mapping_exprs)) {
        mapping_exprs[[col]] <- rlang::sym(col)
      }
    }
  }
  if (length(mapping_exprs) == 0L) {
    return(mapping)
  }
  rlang::inject(ggplot2::aes(!!!mapping_exprs))
}

.genetag_label_column <- function(data) {
  if (!is.data.frame(data)) return(NULL)
  for (col in c("label", "gene", "gene_name", "gene_id", "gene_key", "Name", "ID", "transcripts")) {
    if (col %in% names(data)) return(col)
  }
  NULL
}

.genetag_gene_key <- function(data) {
  key <- .coalesce_character_cols(data, c("gene_key", "gene_id", "ID", "Name", "gene", "label", "transcripts"))
  missing <- is.na(key) | !nzchar(key)
  key[missing] <- paste0("gene_", seq_len(length(key)))[missing]
  key
}

.genetag_label <- function(data) {
  label <- .coalesce_character_cols(data, c("label", "gene", "gene_id", "gene_key", "Name", "ID", "transcripts"))
  missing <- is.na(label) | !nzchar(label)
  label[missing] <- ""
  label
}

.genetag_label_aesthetics <- function() {
  c(
    "label_colour", "label_size", "label_alpha", "label_family",
    "label_fontface", "label_lineheight", "label_link_colour",
    "label_link_linewidth", "label_link_linetype", "label_link_alpha"
  )
}

.genetag_apply_label_selection <- function(data,
                                           label_genes = NULL,
                                           label_filter = "all",
                                           label_match_by = "auto",
                                           label_match = "exact") {
  if (!is.data.frame(data) || nrow(data) == 0L || !"label" %in% names(data)) {
    return(data)
  }

  filter_mask <- .genetag_label_filter_mask(data, label_filter = label_filter)
  genes_mask <- .genetag_label_genes_mask(
    data,
    label_genes = label_genes,
    label_match_by = label_match_by,
    label_match = label_match
  )
  keep <- filter_mask & genes_mask
  data$label[!keep] <- NA_character_
  data
}

.genetag_label_filter_values <- function(label_filter = "all") {
  label_filter <- label_filter %||% "all"
  label_filter <- as.character(label_filter)
  label_filter <- label_filter[!is.na(label_filter) & nzchar(label_filter)]
  if (length(label_filter) == 0L) {
    label_filter <- "all"
  }
  allowed <- c(
    "all",
    "homology_hit",
    "homology_query_hit",
    "homology_reference_hit",
    "species_specific",
    "homology_anchor",
    "homology_visible",
    "homology_offtrack"
  )
  unknown <- setdiff(label_filter, allowed)
  if (length(unknown) > 0L) {
    stop(
      "`label_filter` must be one or more of: ",
      paste(allowed, collapse = ", "),
      ". Unknown value(s): ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  unique(label_filter)
}

.genetag_label_filter_mask <- function(data, label_filter = "all") {
  filters <- .genetag_label_filter_values(label_filter)
  n <- nrow(data)
  out <- rep(FALSE, n)
  for (filter in filters) {
    out <- out | switch(
      filter,
      all = rep(TRUE, n),
      homology_hit = .genetag_homology_hit_mask(data),
      homology_query_hit = .genetag_homology_query_mask(data),
      homology_reference_hit = .genetag_homology_reference_mask(data),
      species_specific = !.genetag_homology_hit_mask(data) &
        !.genetag_logical_column(data, "is_homology_reference_track"),
      homology_anchor = .genetag_strip_label_filter(data, filter),
      homology_visible = .genetag_strip_label_filter(data, filter),
      homology_offtrack = .genetag_strip_label_filter(data, filter)
    )
  }
  out
}

.genetag_logical_column <- function(data, col) {
  if (!col %in% names(data)) {
    return(rep(FALSE, nrow(data)))
  }
  data[[col]] %in% TRUE
}

.genetag_homology_hit_mask <- function(data) {
  .genetag_homology_query_mask(data) | .genetag_homology_reference_mask(data)
}

.genetag_homology_query_mask <- function(data) {
  if (.genetag_has_label_metadata(data, "homology_query_hit")) {
    return(.genetag_logical_column(data, "homology_query_hit"))
  }
  .genetag_logical_column(data, "homology_hit")
}

.genetag_homology_reference_mask <- function(data) {
  .genetag_logical_column(data, "homology_reference_hit")
}

.genetag_has_label_metadata <- function(data, col) {
  if (!col %in% names(data)) {
    return(FALSE)
  }
  values <- data[[col]]
  if (is.character(values) || is.factor(values)) {
    return(any(!is.na(values) & nzchar(as.character(values))))
  }
  any(!is.na(values))
}

.genetag_strip_label_filter <- function(data, filter) {
  n <- nrow(data)
  if (identical(filter, "homology_anchor") || identical(filter, "homology_visible")) {
    if (!.genetag_has_label_metadata(data, "homology_anchor")) {
      warning(
        "`label_filter = \"", filter, "\"` requires `strip_scale_x()` metadata; ",
        "no labels were selected by this filter.",
        call. = FALSE
      )
      return(rep(FALSE, n))
    }
    return(.genetag_logical_column(data, "homology_anchor"))
  }

  if (identical(filter, "homology_offtrack")) {
    if (.genetag_has_label_metadata(data, "visual_class")) {
      return(as.character(data$visual_class) %in% "homologous_offtrack")
    }
    if (.genetag_has_label_metadata(data, "homology_anchor")) {
      return(.genetag_homology_query_mask(data) &
        !.genetag_logical_column(data, "homology_anchor"))
    }
    warning(
      "`label_filter = \"homology_offtrack\"` requires `strip_scale_x()` metadata; ",
      "no labels were selected by this filter.",
      call. = FALSE
    )
    return(rep(FALSE, n))
  }

  rep(FALSE, n)
}

.genetag_label_match_mode <- function(label_match = "exact") {
  label_match <- label_match %||% "exact"
  label_match <- as.character(label_match)
  if (length(label_match) != 1L || is.na(label_match) || !nzchar(label_match)) {
    label_match <- "exact"
  }
  match.arg(label_match, c("exact", "regex"))
}

.genetag_label_match_columns <- function(data, label_match_by = "auto") {
  label_match_by <- label_match_by %||% "auto"
  label_match_by <- as.character(label_match_by)
  if (length(label_match_by) == 1L && identical(label_match_by, "auto")) {
    label_match_by <- c(
      "gene", "label", "gene_name", "gene_id", "gene_key", "Name", "ID", "transcripts",
      "reference_gene", "reference_gene_name"
    )
  }
  label_match_by <- unique(label_match_by[!is.na(label_match_by) & nzchar(label_match_by)])
  intersect(label_match_by, names(data))
}

.genetag_label_genes_mask <- function(data,
                                      label_genes = NULL,
                                      label_match_by = "auto",
                                      label_match = "exact") {
  if (is.null(label_genes)) {
    return(rep(TRUE, nrow(data)))
  }
  label_match <- .genetag_label_match_mode(label_match)
  columns <- .genetag_label_match_columns(data, label_match_by)
  if (length(columns) == 0L) {
    warning(
      "`label_match_by` did not resolve any columns; no labels were selected by `label_genes`.",
      call. = FALSE
    )
    return(rep(FALSE, nrow(data)))
  }

  if (is.list(label_genes) && !is.data.frame(label_genes)) {
    return(.genetag_label_genes_list_mask(data, label_genes, columns, label_match))
  }

  selectors <- .genetag_label_selector_values(label_genes)
  matched <- .genetag_match_rows(data, selectors, columns, label_match)
  .genetag_warn_unmatched_label_genes(selectors[!matched$selector_matched])
  matched$row_mask
}

.genetag_label_selector_values <- function(x) {
  x <- as.character(unlist(x, use.names = FALSE))
  unique(x[!is.na(x) & nzchar(x)])
}

.genetag_label_genes_list_mask <- function(data, label_genes, columns, label_match) {
  if (length(label_genes) == 0L) {
    return(rep(FALSE, nrow(data)))
  }
  list_names <- names(label_genes)
  if (is.null(list_names) || any(is.na(list_names)) || any(!nzchar(list_names))) {
    stop("List-style `label_genes` must be named by track, individual, id, or species.", call. = FALSE)
  }

  out <- rep(FALSE, nrow(data))
  unmatched_tracks <- character()
  unmatched_selectors <- character()
  warn_unmatched_tracks <- !.genetag_single_visible_track(data)
  for (i in seq_along(label_genes)) {
    track_name <- list_names[[i]]
    rows <- .genetag_label_track_rows(data, track_name)
    selectors <- .genetag_label_selector_values(label_genes[[i]])
    if (length(rows) == 0L) {
      if (isTRUE(warn_unmatched_tracks)) {
        unmatched_tracks <- c(unmatched_tracks, track_name)
        unmatched_selectors <- c(
          unmatched_selectors,
          .genetag_prefix_label_selectors(track_name, selectors)
        )
      }
      next
    }
    matched <- .genetag_match_rows(data, selectors, columns, label_match, rows = rows)
    out <- out | matched$row_mask
    unmatched <- selectors[!matched$selector_matched]
    unmatched_selectors <- c(
      unmatched_selectors,
      .genetag_prefix_label_selectors(track_name, unmatched)
    )
  }

  if (length(unmatched_tracks) > 0L) {
    warning(
      "Named `label_genes` entries did not match any visible track: ",
      paste(unique(unmatched_tracks), collapse = ", "),
      call. = FALSE
    )
  }
  .genetag_warn_unmatched_label_genes(unmatched_selectors)
  out
}

.genetag_single_visible_track <- function(data) {
  track_cols <- intersect(c("track", "individual", "id", "species"), names(data))
  if (length(track_cols) == 0L || nrow(data) == 0L) {
    return(FALSE)
  }
  keys <- unique(apply(data[track_cols], 1L, function(row) {
    values <- as.character(row)
    values <- values[!is.na(values) & nzchar(values)]
    paste(values, collapse = "\r")
  }))
  keys <- keys[nzchar(keys)]
  length(keys) == 1L
}

.genetag_prefix_label_selectors <- function(track_name, selectors) {
  selectors <- .genetag_label_selector_values(selectors)
  if (length(selectors) == 0L) {
    return(character())
  }
  paste0(track_name, ":", selectors)
}

.genetag_label_track_rows <- function(data, track_name) {
  track_cols <- intersect(c("track", "individual", "id", "species"), names(data))
  if (length(track_cols) == 0L) {
    return(integer())
  }
  rows <- rep(FALSE, nrow(data))
  for (col in track_cols) {
    rows <- rows | (!is.na(data[[col]]) & as.character(data[[col]]) == track_name)
  }
  which(rows)
}

.genetag_warn_unmatched_label_genes <- function(values) {
  values <- unique(values[!is.na(values) & nzchar(values)])
  if (length(values) == 0L) {
    return(invisible(NULL))
  }
  warning(
    "`label_genes` did not match any visible gene-tag row: ",
    paste(values, collapse = ", "),
    call. = FALSE
  )
  invisible(NULL)
}

.genetag_match_rows <- function(data,
                                selectors,
                                columns,
                                label_match = "exact",
                                rows = seq_len(nrow(data))) {
  row_mask <- rep(FALSE, nrow(data))
  selectors <- .genetag_label_selector_values(selectors)
  selector_matched <- rep(FALSE, length(selectors))
  if (length(selectors) == 0L || length(rows) == 0L) {
    return(list(row_mask = row_mask, selector_matched = selector_matched))
  }

  for (row in rows) {
    row_values <- .genetag_row_match_values(data, columns, row)
    if (length(row_values) == 0L) {
      next
    }
    one_match <- .genetag_values_match(row_values, selectors, label_match)
    if (any(one_match)) {
      row_mask[[row]] <- TRUE
      selector_matched <- selector_matched | one_match
    }
  }

  list(row_mask = row_mask, selector_matched = selector_matched)
}

.genetag_row_match_values <- function(data, columns, row) {
  values <- unlist(lapply(columns, function(col) {
    value <- as.character(data[[col]][[row]])
    unlist(strsplit(value, "\r", fixed = TRUE), use.names = FALSE)
  }), use.names = FALSE)
  unique(values[!is.na(values) & nzchar(values)])
}

.genetag_values_match <- function(values, selectors, label_match = "exact") {
  if (identical(label_match, "exact")) {
    return(selectors %in% values)
  }
  vapply(selectors, function(pattern) {
    any(grepl(pattern, values, perl = TRUE))
  }, logical(1))
}

.genetag_label_position <- function(label_position = "auto", show_label = TRUE) {
  if (!isTRUE(show_label)) {
    return("none")
  }
  label_position <- label_position %||% "auto"
  if (length(label_position) != 1L || is.na(label_position) || !nzchar(label_position)) {
    label_position <- "auto"
  }
  match.arg(label_position, c("auto", "inside", "outside", "none"))
}

.genetag_gene_layout <- function(gene_layout = "single") {
  gene_layout <- gene_layout %||% "single"
  if (length(gene_layout) != 1L || is.na(gene_layout) || !nzchar(gene_layout)) {
    gene_layout <- "single"
  }
  gene_layout <- as.character(gene_layout)
  allowed <- c("single", "stack", "nested")
  if (!gene_layout %in% allowed) {
    stop(
      "`gene_layout` must be one of: ",
      paste(allowed, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  gene_layout
}

.genetag_gene_lane_gap <- function(gene_lane_gap = 0.15) {
  gap <- suppressWarnings(as.numeric(gene_lane_gap %||% 0.15))
  if (length(gap) != 1L || is.na(gap) || !is.finite(gap) || gap < 0) {
    stop("`gene_lane_gap` must be one non-negative numeric value.", call. = FALSE)
  }
  gap
}

.genetag_apply_gene_lanes <- function(data,
                                      gene_layout = "single",
                                      exon_height = 0.8,
                                      gene_lane_gap = 0.15) {
  n <- nrow(data)
  data$gene_lane <- rep(1L, n)
  data$gene_lane_count <- rep(1L, n)
  data$gene_layout <- rep(gene_layout, n)
  if (n == 0L) {
    data$y <- numeric()
    return(data)
  }

  if (identical(gene_layout, "single")) {
    data$y <- exon_height / 2
    return(data)
  }

  xmin <- suppressWarnings(as.numeric(pmin(data$xmin, data$xmax, na.rm = TRUE)))
  xmax <- suppressWarnings(as.numeric(pmax(data$xmin, data$xmax, na.rm = TRUE)))
  valid <- is.finite(xmin) & is.finite(xmax)
  track_key <- .genetag_track_key(data)
  lane_step <- exon_height * (1 + gene_lane_gap)

  for (key in unique(track_key)) {
    idx <- which(track_key == key & valid)
    if (length(idx) == 0L) next
    width <- xmax[idx] - xmin[idx]
    ord <- if (identical(gene_layout, "nested")) {
      idx[order(xmin[idx], -width, xmax[idx], idx)]
    } else {
      idx[order(xmin[idx], xmax[idx], idx)]
    }
    lane_end <- numeric()
    for (i in ord) {
      available <- which(xmin[[i]] > lane_end)
      if (length(available) > 0L) {
        lane <- available[[1L]]
      } else {
        lane <- length(lane_end) + 1L
        lane_end[[lane]] <- -Inf
      }
      data$gene_lane[[i]] <- lane
      lane_end[[lane]] <- xmax[[i]]
    }
    data$gene_lane_count[idx] <- length(lane_end)
  }

  data$y <- exon_height / 2 + (data$gene_lane - 1L) * lane_step
  data
}

.genetag_label_max_lanes <- function(label_max_lanes = 3L) {
  lanes <- suppressWarnings(as.numeric(label_max_lanes %||% 3L))
  if (length(lanes) != 1L || is.na(lanes) || !is.finite(lanes) ||
      lanes < 1 || lanes != floor(lanes)) {
    stop("`label_max_lanes` must be a positive whole number.", call. = FALSE)
  }
  as.integer(lanes)
}

.genetag_label_offset <- function(exon_height = 0.8, label_offset_fraction = 0.3) {
  exon_height <- suppressWarnings(as.numeric(exon_height %||% 0.8))
  if (length(exon_height) != 1L || is.na(exon_height) || !is.finite(exon_height) ||
      exon_height <= 0) {
    exon_height <- 0.8
  }
  fraction <- suppressWarnings(as.numeric(label_offset_fraction %||% 0.3))
  if (length(fraction) != 1L || is.na(fraction) || !is.finite(fraction) || fraction < 0) {
    fraction <- 0.3
  }
  exon_height * fraction
}

.genetag_label_lane_step <- function(exon_height = 0.8, label_offset_fraction = 0.3) {
  offset <- .genetag_label_offset(exon_height, label_offset_fraction)
  exon_height <- suppressWarnings(as.numeric(exon_height %||% 0.8))
  if (length(exon_height) != 1L || is.na(exon_height) || !is.finite(exon_height) ||
      exon_height <= 0) {
    exon_height <- 0.8
  }
  max(offset, exon_height * 0.35)
}

.genetag_label_reserved_space <- function(exon_height = 0.8,
                                          label_offset_fraction = 0.3,
                                          label_max_lanes = 3L) {
  lanes <- .genetag_label_max_lanes(label_max_lanes)
  .genetag_label_offset(exon_height, label_offset_fraction) +
    (lanes - 1L) * .genetag_label_lane_step(exon_height, label_offset_fraction)
}

.genetag_panel_mm <- function(panel_width_mm = NULL,
                              panel_width_inch = NULL,
                              label_panel_width = "auto",
                              viewport_width_mm = NULL,
                              ggexon_output_size = NULL) {
  panel_mm <- if (!is.null(panel_width_inch)) {
    panel_width_inch * 25.4
  } else if (!is.null(panel_width_mm)) {
    panel_width_mm
  } else if (is.numeric(label_panel_width)) {
    if (length(label_panel_width) != 1L || is.na(label_panel_width) ||
        !is.finite(label_panel_width) || label_panel_width <= 0) {
      stop("`label_panel_width` must be \"auto\" or a positive width in millimetres.", call. = FALSE)
    }
    label_panel_width
  } else {
    label_panel_width <- label_panel_width %||% "auto"
    if (length(label_panel_width) != 1L || is.na(label_panel_width) ||
        !identical(as.character(label_panel_width), "auto")) {
      stop("`label_panel_width` must be \"auto\" or a positive width in millimetres.", call. = FALSE)
    }
    viewport_width_mm %||% ggexon_output_width_mm(ggexon_output_size) %||% 300
  }
  if (!is.numeric(panel_mm) || length(panel_mm) != 1L ||
      is.na(panel_mm) || panel_mm <= 0) {
    panel_mm <- 300
  }
  panel_mm
}

.genetag_label_defaults <- list(
  label_colour = "black",
  label_size = 3,
  label_alpha = NA_real_,
  label_family = "sans",
  label_fontface = 1,
  label_lineheight = 1.2,
  label_link_colour = "grey60",
  label_link_linewidth = 0.5,
  label_link_linetype = "solid",
  label_link_alpha = NA_real_
)

.genetag_apply_label_defaults <- function(data) {
  for (name in names(.genetag_label_defaults)) {
    if (!name %in% names(data) || is.null(data[[name]])) {
      data[[name]] <- rep(.genetag_label_defaults[[name]], nrow(data))
    }
  }
  data
}

.genetag_label_width <- function(labels, label_size, data_range, panel_mm) {
  label_size <- suppressWarnings(as.numeric(label_size))
  label_size[!is.finite(label_size)] <- .genetag_label_defaults$label_size
  nchar(as.character(labels)) * 0.5 * label_size * data_range / panel_mm
}

.genetag_label_grob <- function(data,
                                panel_params,
                                coord,
                                show_label = TRUE,
                                label_position = "auto",
                                label_direction = "top",
                                label_offset_fraction = 0.3,
                                label_link = TRUE,
                                label_link_type = "straight",
                                collapse_tandem = FALSE,
                                check_overlap = FALSE,
                                label_max_lanes = 3L,
                                label_panel_width = "auto",
                                label_genes = NULL,
                                label_filter = "all",
                                label_match_by = "auto",
                                label_match = "exact",
                                exon_height = 0.8,
                                panel_width_mm = NULL,
                                panel_width_inch = NULL,
                                ggexon_output_size = NULL) {
  label_position <- .genetag_label_position(label_position, show_label = show_label)
  if (identical(label_position, "none") || !"label" %in% names(data) || nrow(data) == 0L) {
    return(zeroGrob())
  }

  label_link_type <- match.arg(label_link_type %||% "straight", c("straight", "elbow", "spline"))
  label_max_lanes <- .genetag_label_max_lanes(label_max_lanes)
  ggexon_genetag_label_grob(
    data = data,
    panel_params = panel_params,
    coord = coord,
    label_position = label_position,
    label_direction = label_direction,
    label_offset_fraction = label_offset_fraction,
    label_link = label_link,
    label_link_type = label_link_type,
    collapse_tandem = collapse_tandem,
    check_overlap = check_overlap,
    label_max_lanes = label_max_lanes,
    label_panel_width = label_panel_width,
    label_genes = label_genes,
    label_filter = label_filter,
    label_match_by = label_match_by,
    label_match = label_match,
    exon_height = exon_height,
    panel_width_mm = panel_width_mm,
    panel_width_inch = panel_width_inch,
    ggexon_output_size = ggexon_output_size
  )
}

prepare_genetag_label_layer <- function(data, params = list()) {
  data <- .genetag_init_label_layer_columns(data)
  empty <- list(data = data, layout = NULL)
  if (!is.data.frame(data) || nrow(data) == 0L) {
    return(empty)
  }

  label_position <- .genetag_label_position(
    params$label_position %||% "auto",
    show_label = params$show_label %||% TRUE
  )
  if (identical(label_position, "none") || !"label" %in% names(data)) {
    return(empty)
  }
  if (!.genetag_can_precompute_label_layout(params)) {
    return(empty)
  }

  source_col <- ".ggexon_genetag_source_row"
  layout_data <- data
  layout_data[[source_col]] <- seq_len(nrow(layout_data))
  label_layout <- .genetag_label_layout(
    data = layout_data,
    label_position = label_position,
    label_direction = params$label_direction %||% "top",
    label_offset_fraction = params$label_offset_fraction %||% 0.3,
    collapse_tandem = params$collapse_tandem %||% FALSE,
    exon_height = .genetag_effective_height(
      exon_height = params$exon_height,
      height = params$height
    ),
    label_max_lanes = params$label_max_lanes %||% 3L,
    label_panel_width = params$label_panel_width %||% "auto",
    label_genes = params$label_genes,
    label_filter = params$label_filter %||% "all",
    label_match_by = params$label_match_by %||% "auto",
    label_match = params$label_match %||% "exact",
    panel_width_mm = params$panel_width_mm,
    panel_width_inch = params$panel_width_inch,
    ggexon_output_size = params$ggexon_output_size
  )

  data$genetag_label_precomputed <- TRUE
  data <- .genetag_write_label_layer_columns(data, label_layout, source_col = source_col)
  list(data = data, layout = label_layout)
}

.genetag_can_precompute_label_layout <- function(params = list()) {
  if (isTRUE(params$collapse_tandem)) {
    return(FALSE)
  }
  label_panel_width <- params$label_panel_width %||% "auto"
  is.numeric(label_panel_width) ||
    !is.null(params$panel_width_mm) ||
    !is.null(params$panel_width_inch) ||
    inherits(params$ggexon_output_size, "ggexon_output_size_spec")
}

.genetag_init_label_layer_columns <- function(data) {
  if (!is.data.frame(data)) {
    return(data)
  }
  n <- nrow(data)
  data$genetag_label_precomputed <- rep(FALSE, n)
  data$genetag_label_draw <- rep(FALSE, n)
  data$genetag_label_kind <- rep(NA_character_, n)
  data$genetag_label_x <- rep(NA_real_, n)
  data$genetag_label_y <- rep(NA_real_, n)
  data$genetag_label_anchor_y <- rep(NA_real_, n)
  data$genetag_label_orig_x_mid <- rep(NA_real_, n)
  data$genetag_label_gene_xmin <- rep(NA_real_, n)
  data$genetag_label_gene_xmax <- rep(NA_real_, n)
  data$genetag_label_gene_ymin <- rep(NA_real_, n)
  data$genetag_label_gene_ymax <- rep(NA_real_, n)
  data$genetag_label_pos <- rep(NA_character_, n)
  data$genetag_label_lane <- rep(NA_integer_, n)
  data$genetag_label_vjust <- rep(NA_real_, n)
  data$genetag_label_tandem_id <- rep(NA_integer_, n)
  data$genetag_label_unresolved_collision <- rep(FALSE, n)
  data
}

.genetag_write_label_layer_columns <- function(data, label_layout, source_col) {
  unresolved <- isTRUE(attr(label_layout, "unresolved_collision", exact = TRUE))
  data$genetag_label_unresolved_collision <- unresolved
  data <- .genetag_write_one_label_set(data, label_layout$inside, "inside", source_col)
  data <- .genetag_write_one_label_set(data, label_layout$outside, "outside", source_col)
  data
}

.genetag_write_one_label_set <- function(data, labels, kind, source_col) {
  if (!is.data.frame(labels) || nrow(labels) == 0L || !source_col %in% names(labels)) {
    return(data)
  }
  rows <- as.integer(labels[[source_col]])
  keep <- is.finite(rows) & rows >= 1L & rows <= nrow(data)
  if (!any(keep)) {
    return(data)
  }

  rows <- rows[keep]
  labels <- labels[keep, , drop = FALSE]
  data$genetag_label_draw[rows] <- TRUE
  data$genetag_label_kind[rows] <- kind
  data$genetag_label_x[rows] <- labels$label_x %||% labels$x
  data$genetag_label_y[rows] <- labels$label_y %||% labels$y
  data$genetag_label_anchor_y[rows] <- labels$anchor_y %||% labels$gene_ymid
  data$genetag_label_orig_x_mid[rows] <- labels$orig_x_mid %||% labels$x
  data$genetag_label_gene_xmin[rows] <- labels$gene_xmin %||% NA_real_
  data$genetag_label_gene_xmax[rows] <- labels$gene_xmax %||% NA_real_
  data$genetag_label_gene_ymin[rows] <- labels$gene_ymin %||% NA_real_
  data$genetag_label_gene_ymax[rows] <- labels$gene_ymax %||% NA_real_
  data$genetag_label_pos[rows] <- labels$label_pos %||% kind
  data$genetag_label_lane[rows] <- labels$label_lane %||% NA_integer_
  data$genetag_label_vjust[rows] <- labels$vjust %||% 0.5
  if ("tandem_id" %in% names(labels)) {
    data$genetag_label_tandem_id[rows] <- labels$tandem_id
  }
  data
}

ggexon_genetag_label_grob <- function(data,
                                      panel_params,
                                      coord,
                                      label_position = "auto",
                                      label_direction = "top",
                                      label_offset_fraction = 0.3,
                                      label_link = TRUE,
                                      label_link_type = "straight",
                                      collapse_tandem = FALSE,
                                      check_overlap = FALSE,
                                      label_max_lanes = 3L,
                                      label_panel_width = "auto",
                                      label_genes = NULL,
                                      label_filter = "all",
                                      label_match_by = "auto",
                                      label_match = "exact",
                                      exon_height = 0.8,
                                      panel_width_mm = NULL,
                                      panel_width_inch = NULL,
                                      ggexon_output_size = NULL,
                                      name = NULL) {
  grid::grob(
    data = data,
    panel_params = panel_params,
    coord = coord,
    label_position = label_position,
    label_direction = label_direction,
    label_offset_fraction = label_offset_fraction,
    label_link = label_link,
    label_link_type = label_link_type,
    collapse_tandem = collapse_tandem,
    check_overlap = check_overlap,
    label_max_lanes = label_max_lanes,
    label_panel_width = label_panel_width,
    label_genes = label_genes,
    label_filter = label_filter,
    label_match_by = label_match_by,
    label_match = label_match,
    exon_height = exon_height,
    panel_width_mm = panel_width_mm,
    panel_width_inch = panel_width_inch,
    ggexon_output_size = ggexon_output_size,
    name = name %||% "ggexon-genetag-labels",
    cl = "ggexonGenetagLabelGrob"
  )
}

#' @export
drawDetails.ggexonGenetagLabelGrob <- function(x, recording = TRUE) {
  label_layout <- .genetag_precomputed_label_layout(x$data)
  if (is.null(label_layout)) {
    viewport_width_mm <- suppressWarnings(
      grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
    )
    if (length(viewport_width_mm) != 1L || is.na(viewport_width_mm) ||
        !is.finite(viewport_width_mm) || viewport_width_mm <= 0) {
      viewport_width_mm <- NULL
    }
    panel_mm <- .genetag_panel_mm(
      panel_width_mm = x$panel_width_mm,
      panel_width_inch = x$panel_width_inch,
      label_panel_width = x$label_panel_width,
      viewport_width_mm = viewport_width_mm,
      ggexon_output_size = x$ggexon_output_size
    )
    label_layout <- .genetag_label_layout(
      data = x$data,
      label_position = x$label_position,
      label_direction = x$label_direction,
      label_offset_fraction = x$label_offset_fraction,
      collapse_tandem = x$collapse_tandem,
      exon_height = x$exon_height,
      label_max_lanes = x$label_max_lanes,
      label_genes = x$label_genes,
      label_filter = x$label_filter,
      label_match_by = x$label_match_by,
      label_match = x$label_match,
      panel_width_mm = panel_mm
    )
  }

  if (isTRUE(attr(label_layout, "unresolved_collision", exact = TRUE))) {
    warning(
      "`geom_genetag()` could not place all outside labels without overlap ",
      "within `label_max_lanes = ", x$label_max_lanes, "`. Increase output ",
      "width, reduce label size, or raise `label_max_lanes`.",
      call. = FALSE
    )
  }

  grobs <- list()
  if (nrow(label_layout$outside) > 0L && isTRUE(x$label_link)) {
    grobs[[length(grobs) + 1L]] <- .genetag_as_grob(.genetag_outside_link_grob(
      label_layout$outside,
      tandem_anchors = label_layout$tandem_anchors,
      panel_params = x$panel_params,
      coord = x$coord,
      label_link_type = x$label_link_type
    ))
  }
  if (nrow(label_layout$inside) > 0L) {
    grobs[[length(grobs) + 1L]] <- .genetag_as_grob(ggplot2::GeomText$draw_panel(
      label_layout$inside,
      x$panel_params,
      x$coord,
      check_overlap = x$check_overlap
    ))
  }
  if (nrow(label_layout$outside) > 0L) {
    grobs[[length(grobs) + 1L]] <- .genetag_as_grob(ggplot2::GeomText$draw_panel(
      label_layout$outside,
      x$panel_params,
      x$coord,
      check_overlap = x$check_overlap
    ))
  }

  grobs <- Filter(function(x) !inherits(x, "zeroGrob"), grobs)
  if (length(grobs) == 0L) {
    return(invisible())
  }
  grid::grid.draw(gTree(children = do.call(gList, grobs)))
  invisible()
}

.genetag_precomputed_label_layout <- function(data) {
  required <- c(
    "genetag_label_precomputed", "genetag_label_draw", "genetag_label_kind",
    "genetag_label_x", "genetag_label_y"
  )
  if (!is.data.frame(data) || !all(required %in% names(data))) {
    return(NULL)
  }

  precomputed <- isTRUE(any(data$genetag_label_precomputed, na.rm = TRUE))
  if (!precomputed) {
    return(NULL)
  }

  empty <- list(
    inside = data[0, , drop = FALSE],
    outside = data[0, , drop = FALSE],
    tandem_anchors = list()
  )

  draw <- data$genetag_label_draw &
    is.finite(data$genetag_label_x) &
    is.finite(data$genetag_label_y)
  if (!any(draw, na.rm = TRUE)) {
    attr(empty, "unresolved_collision") <- isTRUE(any(
      data$genetag_label_unresolved_collision %||% FALSE,
      na.rm = TRUE
    ))
    return(empty)
  }

  labels <- data[draw, , drop = FALSE]
  labels$label_x <- labels$genetag_label_x
  labels$label_y <- labels$genetag_label_y
  labels$anchor_y <- labels$genetag_label_anchor_y
  labels$orig_x_mid <- labels$genetag_label_orig_x_mid
  labels$gene_xmin <- labels$genetag_label_gene_xmin
  labels$gene_xmax <- labels$genetag_label_gene_xmax
  labels$gene_ymin <- labels$genetag_label_gene_ymin
  labels$gene_ymax <- labels$genetag_label_gene_ymax
  labels$label_pos <- labels$genetag_label_pos
  labels$label_lane <- labels$genetag_label_lane
  labels$tandem_id <- labels$genetag_label_tandem_id

  label_kind <- as.character(labels$genetag_label_kind)
  label_kind[is.na(label_kind)] <- ""
  inside <- labels[label_kind == "inside", , drop = FALSE]
  outside <- labels[label_kind == "outside", , drop = FALSE]
  if (nrow(inside) > 0L) {
    inside <- .genetag_text_data(
      inside,
      x = inside$label_x,
      y = inside$label_y,
      vjust = inside$genetag_label_vjust
    )
  }
  if (nrow(outside) > 0L) {
    outside <- .genetag_text_data(
      outside,
      x = outside$label_x,
      y = outside$label_y,
      vjust = outside$genetag_label_vjust
    )
  }

  out <- list(inside = inside, outside = outside, tandem_anchors = list())
  attr(out, "unresolved_collision") <- isTRUE(any(
    data$genetag_label_unresolved_collision %||% FALSE,
    na.rm = TRUE
  ))
  out
}

.genetag_label_layout <- function(data,
                                  label_position = "auto",
                                  label_direction = "top",
                                  label_offset_fraction = 0.3,
                                  collapse_tandem = FALSE,
                                  exon_height = 0.8,
                                  label_max_lanes = 3L,
                                  label_panel_width = "auto",
                                  label_genes = NULL,
                                  label_filter = "all",
                                  label_match_by = "auto",
                                  label_match = "exact",
                                  panel_width_mm = NULL,
                                  panel_width_inch = NULL,
                                  viewport_width_mm = NULL,
                                  ggexon_output_size = NULL) {
  data <- .genetag_apply_label_selection(
    data = data,
    label_genes = label_genes,
    label_filter = label_filter,
    label_match_by = label_match_by,
    label_match = label_match
  )
  labels <- as.character(data$label)
  keep <- !is.na(labels) & nzchar(labels)
  empty <- list(inside = data[0, , drop = FALSE], outside = data[0, , drop = FALSE], tandem_anchors = list())
  if (!any(keep)) {
    return(empty)
  }

  data2 <- .genetag_apply_label_defaults(data[keep, , drop = FALSE])
  labels <- labels[keep]
  data_range <- diff(range(c(data$xmin, data$xmax), na.rm = TRUE))
  if (!is.finite(data_range) || data_range <= 0) data_range <- 1
  panel_mm <- .genetag_panel_mm(
    panel_width_mm = panel_width_mm,
    panel_width_inch = panel_width_inch,
    label_panel_width = label_panel_width,
    viewport_width_mm = viewport_width_mm,
    ggexon_output_size = ggexon_output_size
  )
  label_max_lanes <- .genetag_label_max_lanes(label_max_lanes)
  gene_xmin <- pmin(data2$xmin, data2$xmax)
  gene_xmax <- pmax(data2$xmin, data2$xmax)
  tag_width <- gene_xmax - gene_xmin
  est_width <- .genetag_label_width(labels, data2$label_size, data_range, panel_mm)

  data2$label <- labels
  data2$gene_xmin <- gene_xmin
  data2$gene_xmax <- gene_xmax
  data2$orig_x_mid <- (gene_xmin + gene_xmax) / 2
  body_bounds <- .genetag_body_y_bounds(data2, exon_height)
  data2$gene_ymin <- body_bounds$ymin
  data2$gene_ymax <- body_bounds$ymax
  data2$gene_ymid <- data2$y
  data2$tag_width <- tag_width
  data2$est_width <- est_width
  data2$fits_inside <- is.finite(tag_width) & is.finite(est_width) & est_width <= tag_width

  inside_idx <- switch(
    label_position,
    inside = which(data2$fits_inside),
    auto = which(data2$fits_inside),
    outside = integer(),
    none = integer()
  )
  outside_idx <- switch(
    label_position,
    inside = integer(),
    auto = which(!data2$fits_inside),
    outside = seq_len(nrow(data2)),
    none = integer()
  )

  inside <- data2[inside_idx, , drop = FALSE]
  if (nrow(inside) > 0L) {
    inside <- .genetag_text_data(
      inside,
      x = inside$orig_x_mid,
      y = inside$gene_ymid,
      vjust = 0.5
    )
  }

  outside <- data2[outside_idx, , drop = FALSE]
  tandem_anchors <- list()
  if (nrow(outside) > 0L) {
    outside <- outside[order(.genetag_track_key(outside), outside$orig_x_mid), , drop = FALSE]
    if (isTRUE(collapse_tandem)) {
      outside <- .collapse_tandem_labels(outside)
      tandem_anchors <- attr(outside, "tandem_anchors") %||% list()
    }
    body_height <- abs(outside$gene_ymax - outside$gene_ymin)
    outside <- .genetag_outside_label_data(
      outside,
      label_direction = label_direction,
      label_offset = body_height * .genetag_label_offset(1, label_offset_fraction),
      lane_step = body_height * .genetag_label_lane_step(1, label_offset_fraction),
      label_max_lanes = label_max_lanes,
      data_xmin = min(data2$gene_xmin, na.rm = TRUE),
      data_xmax = max(data2$gene_xmax, na.rm = TRUE),
      data_range = data_range
    )
  }

  out <- list(inside = inside, outside = outside, tandem_anchors = tandem_anchors)
  attr(out, "unresolved_collision") <- isTRUE(attr(outside, "unresolved_collision", exact = TRUE))
  out
}

.genetag_text_data <- function(data, x, y, vjust = 0.5) {
  data$x <- x
  data$y <- y
  data$colour <- data$label_colour
  data$size <- data$label_size
  data$alpha <- data$label_alpha
  data$family <- data$label_family
  data$fontface <- data$label_fontface
  data$lineheight <- data$label_lineheight
  data$angle <- 0
  data$hjust <- 0.5
  data$vjust <- vjust
  data
}

.genetag_track_key <- function(data) {
  key <- if ("track" %in% names(data)) as.character(data$track) else rep("", nrow(data))
  key[is.na(key)] <- ""
  key
}

.genetag_outside_label_data <- function(data,
                                        label_direction = "top",
                                        label_offset = 0.24,
                                        lane_step = 0.28,
                                        label_max_lanes = 3L,
                                        data_xmin,
                                        data_xmax,
                                        data_range) {
  positions <- .parse_label_positions(label_direction)
  positions[positions == "center"] <- "top"
  if (length(positions) == 0L) {
    positions <- "top"
  }

  data$label_x <- data$orig_x_mid
  data$label_pos <- "top"
  data$label_lane <- 1L
  track_key <- .genetag_track_key(data)
  for (key in unique(track_key)) {
    idx <- which(track_key == key)
    pos_idx <- (seq_along(idx) - 1L) %% length(positions) + 1L
    data$label_pos[idx] <- positions[pos_idx]
  }

  data <- .genetag_assign_label_lanes(
    data,
    data_range = data_range,
    label_max_lanes = label_max_lanes
  )

  data$label_y <- data$gene_ymax + label_offset + (data$label_lane - 1L) * lane_step
  data$anchor_y <- data$gene_ymax
  data$vjust <- 1
  for (key in unique(track_key)) {
    idx <- which(track_key == key)
    top_y <- max(data$gene_ymax[idx], na.rm = TRUE)
    bottom_y <- min(data$gene_ymin[idx], na.rm = TRUE)
    top_idx <- idx[data$label_pos[idx] == "top"]
    bottom_idx <- idx[data$label_pos[idx] == "bottom"]
    if (length(top_idx) > 0L) {
      data$label_y[top_idx] <- top_y + label_offset[top_idx] +
        (data$label_lane[top_idx] - 1L) * lane_step[top_idx]
      data$anchor_y[top_idx] <- data$gene_ymax[top_idx]
      data$vjust[top_idx] <- 1
    }
    if (length(bottom_idx) > 0L) {
      data$label_y[bottom_idx] <- bottom_y - label_offset[bottom_idx] -
        (data$label_lane[bottom_idx] - 1L) * lane_step[bottom_idx]
      data$anchor_y[bottom_idx] <- data$gene_ymin[bottom_idx]
      data$vjust[bottom_idx] <- 0
    }
  }

  data <- .genetag_spread_outside_labels(
    data,
    data_xmin = data_xmin,
    data_xmax = data_xmax,
    data_range = data_range
  )
  unresolved <- isTRUE(attr(data, "unresolved_collision", exact = TRUE))
  data <- .genetag_text_data(data, x = data$label_x, y = data$label_y, vjust = data$vjust)
  attr(data, "unresolved_collision") <- unresolved
  data
}

.genetag_assign_label_lanes <- function(data, data_range, label_max_lanes = 3L) {
  label_max_lanes <- .genetag_label_max_lanes(label_max_lanes)
  if (nrow(data) <= 1L) {
    data$label_lane <- 1L
    attr(data, "unresolved_collision") <- FALSE
    return(data)
  }

  min_gap <- data_range * 0.005
  group_key <- paste(.genetag_track_key(data), data$label_pos, sep = "\r")
  lane_overflow <- FALSE
  data$label_lane <- 1L
  for (key in unique(group_key)) {
    idx <- which(group_key == key)
    if (length(idx) <= 1L) next
    idx <- idx[order(data$label_x[idx])]
    lane_right <- rep(-Inf, label_max_lanes)
    for (i in idx) {
      halfw <- data$est_width[[i]] / 2
      left <- data$label_x[[i]] - halfw
      right <- data$label_x[[i]] + halfw
      available <- which(left >= lane_right + min_gap)
      if (length(available) > 0L) {
        lane <- available[[1L]]
      } else {
        lane <- which.min(lane_right)
        lane_overflow <- TRUE
      }
      data$label_lane[[i]] <- lane
      lane_right[[lane]] <- max(lane_right[[lane]], right, na.rm = TRUE)
    }
  }
  attr(data, "lane_overflow") <- lane_overflow
  data
}

.genetag_spread_outside_labels <- function(data, data_xmin, data_xmax, data_range) {
  if (nrow(data) <= 1L) {
    return(.genetag_constrain_label_x(data, data_xmin, data_xmax))
  }
  min_gap <- data_range * 0.005
  group_key <- paste(.genetag_track_key(data), data$label_pos, data$label_lane, sep = "\r")
  for (key in unique(group_key)) {
    idx <- which(group_key == key)
    n <- length(idx)
    if (n <= 1L) next
    idx <- idx[order(data$label_x[idx])]
    ideal <- data$label_x[idx]
    halfw <- data$est_width[idx] / 2
    dist <- halfw[-n] + halfw[-1L] + min_gap
    x <- ideal
    changed <- TRUE
    iter <- 0L
    while (changed && iter < 50L) {
      changed <- FALSE
      iter <- iter + 1L
      for (j in 2L:n) {
        d <- dist[[j - 1L]]
        if (x[[j]] < x[[j - 1L]] + d) {
          delta <- (x[[j - 1L]] + d - x[[j]]) / 2
          x[[j - 1L]] <- x[[j - 1L]] - delta
          x[[j]] <- x[[j]] + delta
          changed <- TRUE
        }
      }
      for (j in seq.int(n - 1L, 1L)) {
        d <- dist[[j]]
        if (x[[j + 1L]] < x[[j]] + d) {
          delta <- (x[[j]] + d - x[[j + 1L]]) / 2
          x[[j]] <- x[[j]] - delta
          x[[j + 1L]] <- x[[j + 1L]] + delta
          changed <- TRUE
        }
      }
    }
    data$label_x[idx] <- x
  }
  data <- .genetag_constrain_label_x(data, data_xmin, data_xmax)
  attr(data, "unresolved_collision") <- .genetag_has_outside_label_collisions(
    data,
    data_range = data_range
  )
  data
}

.genetag_has_outside_label_collisions <- function(data, data_range) {
  if (nrow(data) <= 1L || !"label_lane" %in% names(data)) {
    return(FALSE)
  }
  min_gap <- data_range * 0.005
  group_key <- paste(.genetag_track_key(data), data$label_pos, data$label_lane, sep = "\r")
  for (key in unique(group_key)) {
    idx <- which(group_key == key)
    if (length(idx) <= 1L) next
    idx <- idx[order(data$label_x[idx])]
    halfw <- data$est_width[idx] / 2
    right <- data$label_x[idx] + halfw
    left <- data$label_x[idx] - halfw
    if (any(left[-1L] < right[-length(right)] + min_gap, na.rm = TRUE)) {
      return(TRUE)
    }
  }
  FALSE
}

.genetag_constrain_label_x <- function(data, data_xmin, data_xmax) {
  halfw <- data$est_width / 2
  lower <- data_xmin + halfw
  upper <- data_xmax - halfw
  midpoint <- (data_xmin + data_xmax) / 2
  ok <- is.finite(lower) & is.finite(upper) & lower <= upper
  data$label_x[ok] <- pmax(data$label_x[ok], lower[ok])
  data$label_x[ok] <- pmin(data$label_x[ok], upper[ok])
  data$label_x[!ok] <- midpoint
  data
}

.genetag_outside_link_grob <- function(data,
                                       tandem_anchors = list(),
                                       panel_params,
                                       coord,
                                       label_link_type = "straight") {
  if (nrow(data) == 0L) {
    return(zeroGrob())
  }
  if (!"tandem_id" %in% names(data)) {
    data$tandem_id <- NA_integer_
  }
  singletons <- data[is.na(data$tandem_id), , drop = FALSE]
  tandems <- data[!is.na(data$tandem_id), , drop = FALSE]
  grobs <- list()

  if (nrow(singletons) > 0L) {
    link_data <- .genetag_link_data(
      singletons,
      x = singletons$orig_x_mid,
      y = singletons$anchor_y,
      xend = singletons$label_x,
      yend = singletons$label_y
    )
    grobs[[length(grobs) + 1L]] <- .genetag_as_grob(.draw_link_grobs_raw(
      coord$transform(link_data, panel_params),
      label_link_type
    ))
  }

  if (nrow(tandems) > 0L) {
    for (i in seq_len(nrow(tandems))) {
      row <- tandems[i, , drop = FALSE]
      members <- tandem_anchors[[as.character(row$tandem_id[[1L]])]]
      if (is.null(members) || nrow(members) < 2L) {
        link_data <- .genetag_link_data(
          row,
          x = row$orig_x_mid,
          y = row$anchor_y,
          xend = row$label_x,
          yend = row$label_y
        )
        grobs[[length(grobs) + 1L]] <- .genetag_as_grob(.draw_link_grobs_raw(
          coord$transform(link_data, panel_params),
          label_link_type
        ))
        next
      }

      members$anchor_y <- if (identical(row$label_pos[[1L]], "bottom")) {
        members$gene_ymin
      } else {
        members$gene_ymax
      }
      bracket_y <- mean(range(members$anchor_y, na.rm = TRUE))
      bracket <- .genetag_link_data(
        row,
        x = min(members$x, na.rm = TRUE),
        y = bracket_y,
        xend = max(members$x, na.rm = TRUE),
        yend = bracket_y
      )
      bracket_t <- coord$transform(bracket, panel_params)
      grobs[[length(grobs) + 1L]] <- segmentsGrob(
        x0 = bracket_t$x, y0 = bracket_t$y,
        x1 = bracket_t$xend, y1 = bracket_t$yend,
        default.units = "native",
        gp = gpar(
          col = alpha(bracket_t$colour, bracket_t$alpha),
          lwd = bracket_t$linewidth,
          lty = bracket_t$linetype
        )
      )

      drop_data <- .genetag_link_data(
        row[rep(1L, nrow(members)), , drop = FALSE],
        x = members$x,
        y = bracket_y,
        xend = members$x,
        yend = members$anchor_y
      )
      grobs[[length(grobs) + 1L]] <- .draw_link_grobs_raw(
        coord$transform(drop_data, panel_params),
        "straight"
      )

      main_data <- .genetag_link_data(
        row,
        x = mean(range(members$x, na.rm = TRUE)),
        y = bracket_y,
        xend = row$label_x,
        yend = row$label_y
      )
      grobs[[length(grobs) + 1L]] <- .genetag_as_grob(.draw_link_grobs_raw(
        coord$transform(main_data, panel_params),
        label_link_type
      ))
    }
  }

  grobs <- Filter(function(x) !inherits(x, "zeroGrob"), grobs)
  if (length(grobs) == 0L) {
    return(zeroGrob())
  }
  gTree(children = do.call(gList, lapply(grobs, .genetag_as_grob)))
}

.genetag_link_data <- function(data, x, y, xend, yend) {
  data.frame(
    x = x,
    y = y,
    xend = xend,
    yend = yend,
    colour = data$label_link_colour,
    linewidth = data$label_link_linewidth,
    linetype = data$label_link_linetype,
    alpha = data$label_link_alpha,
    stringsAsFactors = FALSE
  )
}

.genetag_as_grob <- function(x) {
  if (inherits(x, "gList")) {
    return(gTree(children = x))
  }
  x
}

.genetag_polygon_data <- function(data,
                                  exon_height = NULL,
                                  height = NULL,
                                  arrow_width = NULL,
                                  arrow_fraction = 0.18) {
  exon_height <- .genetag_effective_height(exon_height = exon_height, height = height)
  arrow_fraction <- .genetag_positive_number(arrow_fraction, "arrow_fraction")
  if (arrow_fraction > 0.5) {
    stop("`arrow_fraction` must be no larger than 0.5.", call. = FALSE)
  }
  if (!is.null(arrow_width) && !is.na(arrow_width)) {
    arrow_width <- .genetag_positive_number(arrow_width, "arrow_width")
  } else {
    arrow_width <- NULL
  }

  body_bounds <- .genetag_body_y_bounds(data, exon_height)
  pieces <- vector("list", nrow(data))
  for (i in seq_len(nrow(data))) {
    xmin <- min(data$xmin[[i]], data$xmax[[i]])
    xmax <- max(data$xmin[[i]], data$xmax[[i]])
    y <- data$y[[i]]
    strand <- .genetag_normalize_strand(data$strand[[i]])
    width <- xmax - xmin
    head_width <- if (is.null(arrow_width)) {
      width * arrow_fraction
    } else {
      min(arrow_width, width)
    }
    y_min <- body_bounds$ymin[[i]]
    y_max <- body_bounds$ymax[[i]]

    coords <- switch(
      strand,
      "+" = data.frame(
        x = c(xmin, xmax - head_width, xmax, xmax - head_width, xmin),
        y = c(y_min, y_min, y, y_max, y_max)
      ),
      "-" = data.frame(
        x = c(xmin + head_width, xmax, xmax, xmin + head_width, xmin),
        y = c(y_min, y_min, y_max, y_max, y)
      ),
      data.frame(
        x = c(xmin, xmax, xmax, xmin),
        y = c(y_min, y_min, y_max, y_max)
      )
    )

    row <- data[rep(i, nrow(coords)), , drop = FALSE]
    row$x <- coords$x
    row$y <- coords$y
    row$group <- i
    pieces[[i]] <- row
  }

  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

.genetag_arrow_polygon_data <- function(data,
                                        exon_height = NULL,
                                        height = NULL,
                                        arrow_width = NULL,
                                        arrow_fraction = 0.18) {
  exon_height <- .genetag_effective_height(exon_height = exon_height, height = height)
  arrow_fraction <- .genetag_positive_number(arrow_fraction, "arrow_fraction")
  if (arrow_fraction > 0.5) {
    stop("`arrow_fraction` must be no larger than 0.5.", call. = FALSE)
  }
  if (!is.null(arrow_width) && !is.na(arrow_width)) {
    arrow_width <- .genetag_positive_number(arrow_width, "arrow_width")
  } else {
    arrow_width <- NULL
  }

  body_bounds <- .genetag_body_y_bounds(data, exon_height)
  pieces <- vector("list", nrow(data))
  for (i in seq_len(nrow(data))) {
    xmin <- min(data$xmin[[i]], data$xmax[[i]])
    xmax <- max(data$xmin[[i]], data$xmax[[i]])
    y <- data$y[[i]]
    strand <- .genetag_normalize_strand(data$strand[[i]])
    if (!strand %in% c("+", "-")) {
      next
    }
    width <- xmax - xmin
    head_width <- if (is.null(arrow_width)) {
      width * arrow_fraction
    } else {
      min(arrow_width, width)
    }
    y_min <- body_bounds$ymin[[i]]
    y_max <- body_bounds$ymax[[i]]

    coords <- switch(
      strand,
      "+" = data.frame(
        x = c(xmax - head_width, xmax, xmax - head_width),
        y = c(y_min, y, y_max)
      ),
      "-" = data.frame(
        x = c(xmin + head_width, xmin, xmin + head_width),
        y = c(y_min, y, y_max)
      )
    )

    row <- data[rep(i, nrow(coords)), , drop = FALSE]
    row$x <- coords$x
    row$y <- coords$y
    row$group <- i
    pieces[[i]] <- row
  }

  pieces <- Filter(Negate(is.null), pieces)
  if (length(pieces) == 0L) {
    return(data.frame())
  }
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

.genetag_body_y_bounds <- function(data, exon_height) {
  ymin <- data$y - exon_height / 2
  ymax <- data$y + exon_height / 2
  body_columns <- c(".ggexon_body_ymin", ".ggexon_body_ymax")
  if (all(body_columns %in% names(data))) {
    body_ymin <- suppressWarnings(as.numeric(data$.ggexon_body_ymin))
    body_ymax <- suppressWarnings(as.numeric(data$.ggexon_body_ymax))
    body_center <- (body_ymin + body_ymax) / 2
    center_tolerance <- sqrt(.Machine$double.eps) * pmax(
      1,
      abs(body_center),
      abs(data$y)
    )
    use_body <- is.finite(body_ymin) & is.finite(body_ymax) &
      is.finite(data$y) & abs(body_center - data$y) <= center_tolerance
    ymin[use_body] <- body_ymin[use_body]
    ymax[use_body] <- body_ymax[use_body]
  }
  data.frame(ymin = ymin, ymax = ymax)
}

.genetag_effective_height <- function(exon_height = NULL, height = NULL) {
  if (!is.null(exon_height)) {
    return(.genetag_positive_number(exon_height, "exon_height"))
  }
  if (!is.null(height)) {
    return(.genetag_positive_number(height, "height"))
  }
  0.8
}

.genetag_positive_number <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
    stop("`", name, "` must be one positive numeric value.", call. = FALSE)
  }
  as.numeric(x)
}

.genetag_normalize_strand <- function(x) {
  x <- as.character(x)
  if (length(x) == 0L || is.na(x) || !nzchar(x)) {
    return("*")
  }
  x <- base::tolower(x[[1L]])
  if (x %in% c("+", "plus", "forward", "1")) {
    return("+")
  }
  if (x %in% c("-", "minus", "reverse", "-1")) {
    return("-")
  }
  "*"
}
