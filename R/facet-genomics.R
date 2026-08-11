#' Facet genomic tracks and link panels
#'
#' `facet_genomics()` is ggexon's Syn-aware faceting interface. It behaves like
#' a wrap-style facet for ordinary data, but it can also arrange comparative
#' genomic panels for `SynSpecies` plots, including annotation panels and
#' intermediate link panels used by `geom_nuclink()`.
#'
#' For `SynSpecies` inputs, the facet chooses among three layout sources:
#'
#' - an explicit layout override attached during build
#' - a stored [`SynLayout`] on the `SynSpecies` object
#' - a newly derived chain layout computed from the annotation and link layers
#'
#' When link panels are present, `facet_genomics()` also annotates the final
#' panel table with source-panel metadata (`t_panel`, `q_panel`) and vertical
#' link anchors so that `geom_nuclink()` can borrow x ranges from the correct
#' annotation panels while drawing inside the link panel.
#'
#' @param facets Faceting variables, usually `ggplot2::vars(track)` for Syn
#'   layouts.
#' @param nrow,ncol Number of rows and columns in the wrapped layout.
#' @param scales One of `"fixed"`, `"free_x"`, `"free_y"`, or `"free"`.
#'   Ordinary data follows ggplot2 wrap-facet semantics. For first-class Syn
#'   coverage panels, the y component is the coverage-policy fallback only:
#'   `"fixed"`/`"free_x"` share a coverage scale and
#'   `"free_y"`/`"free"` give each coverage panel its own scale. Annotation
#'   remains `"fixed_y"` by default. Explicit [`scale_panel_annotation()`] and
#'   [`scale_panel_coverage()`] specifications take precedence.
#' @param shrink Passed through to the facet.
#' @param labeller A labeller specification.
#' @param as.table Logical; whether panels are laid out like a table.
#' @param switch Deprecated ggplot2 argument.
#' @param drop Logical; drop unused facet levels?
#' @param dir Wrapping direction. Single-letter values are normalized using
#'   `as.table`.
#' @param strip.position Position of facet strips.
#' @param axes Which axes to draw.
#' @param axis.labels Which axis labels to draw.
#' @param link_panel_height Optional relative height for link panels. Supply a
#'   single positive number to use a `null` unit relative to ordinary panel
#'   rows, or a single grid unit. When `NULL`, link panels keep the default
#'   ggplot2 facet row height.
#' @param link_axis Link-panel axis handling. `"inherit"` keeps the axes drawn
#'   by the facet. `"none"` removes both x and y axes from link panels. `"x"`
#'   keeps only x axes, and `"y"` keeps only y axes.
#' @param link_strip Link-panel strip handling. `"inherit"` keeps link-panel
#'   strips. `"blank"` removes link-panel strip grobs and collapses horizontal
#'   strip rows when they contain only link panels.
#' @param annotation_axis Annotation-panel x-axis handling for stacked
#'   (single-column) genomic layouts. `"all"` (default) keeps the per-panel
#'   x-axis that `scales = "free_x"` draws on every panel. `"bottom"` keeps the
#'   x-axis only on the bottom-most annotation panel of each column and blanks
#'   the interior ones, collapsing the reclaimed axis rows so the panels sit
#'   compactly. The per-panel free scales are preserved either way.
#' @param vertical Annotation-panel vertical alignment. `"default"` preserves
#'   the trained y-ranges. `"center"` symmetrizes each annotation panel's
#'   y-range around its visible annotation bodies. Link panels keep their fixed
#'   vertical range in either mode. For Syn-backed plots this compatibility
#'   argument is equivalent to adding [`center_panel_annotation()`] and does
#'   not change coverage panels, layer positions, or scale training. Its legacy
#'   ordinary-data behavior is retained; the dedicated wrapper is Syn-only.
#' @param reverse_x Annotation panels whose x axis should be drawn in reverse.
#'   Use `NULL` or `FALSE` for no panel reversal, `TRUE` to reverse all
#'   annotation panels, or a character vector matched against panel layout
#'   columns such as `track`, `species`, `strain`, or `id`. Link panels are not
#'   reversed directly; link x positions inherit the transform from their source
#'   annotation panels.
#' @param reverse_x_match_by Panel-layout column used to match `reverse_x`
#'   character values. `"auto"` checks common layout columns such as `species`,
#'   `strain`, `id`, and `track`.
#' @param xlim Optional panel-specific x limits for annotation panels, or for
#'   standalone coverage panels when no annotation panel is present. Supply a
#'   named list of numeric length-2 vectors keyed by individual / panel name.
#'   If the plot contains only one eligible panel, a single numeric length-2
#'   vector is also accepted.
#' @param xlim_chr Optional chromosome / seqname for `xlim`. Supply one
#'   character value for a single panel, or a named character vector/list keyed
#'   by individual when `xlim` contains multiple panels. When omitted, ggexon
#'   tries to infer the seqname from attached alignments or single-seqname
#'   annotations. Link layers can only be filtered by panel limits when the
#'   seqname can be resolved.
#'
#' @return A `FacetGenomics` ggproto object.
#'
#' @section SynSpecies behavior:
#' In Syn-aware builds, `facet_genomics()` is responsible for deciding the panel
#' structure used by `Layout2`. The returned panel table may include:
#'
#' - annotation panels for each species track
#' - first-class coverage panels for attached BigWig tracks
#' - link panels inserted between paired species tracks
#' - `panel_type`, `tspecies`, `qspecies`, `t_panel`, and `q_panel` metadata
#'   used later by `geom_nuclink()`
#'
#' Coverage, annotation, and link rows are role-qualified even when they share
#' the same public `track` label. Coverage panels precede the existing
#' annotation/link chain and inherit ordinary genomic x windows and direction
#' from an annotation source when one exists. A standalone coverage panel can
#' instead use its own coordinates or named facet limits. Coverage panels never
#' become `t_panel` or `q_panel` link sources.
#'
#' `SCALE_Y` in the resolved layout is the inherited scale-object identity:
#' panels with the same value train through one y-scale object, while different
#' values use independent objects. Annotation and coverage role families never
#' share an identity. Use [`scale_panel_annotation()`] and
#' [`scale_panel_coverage()`] to choose fixed or per-panel inheritance for each
#' role independently.
#'
#' If no Syn-specific layout is available, the facet falls back to ordinary
#' wrap-style panel generation. Valid but unused panel-scale specifications are
#' no-ops. Ordinary non-Syn behavior and [`facet_genomictree()`] are unchanged.
#'
#' @seealso [SynLayout], [`scale_panel_annotation()`],
#'   [`scale_panel_coverage()`], [`center_panel_annotation()`]
#' @export
facet_genomics <- function(facets, nrow = NULL, ncol = NULL, scales = "fixed",
                       shrink = TRUE, labeller = "label_value", as.table = TRUE,
                       switch = deprecated(), drop = TRUE, dir = "h",
                       strip.position = 'top', axes = "margins",
                       axis.labels = "all", link_panel_height = NULL,
                       link_axis = "inherit", link_strip = "inherit",
                       annotation_axis = "all",
                       vertical = c("default", "center"),
                       reverse_x = NULL,
                       reverse_x_match_by = c("auto", "species", "strain", "id", "track"),
                       xlim = NULL, xlim_chr = NULL) {
  scales <- arg_match0(scales %||% "fixed", c("fixed", "free_x", "free_y", "free"))
  dir <- arg_match0(dir, c("h", "v", "lt", "tl", "lb", "bl", "rt", "tr", "rb", "br"))
  link_axis <- arg_match0(link_axis %||% "inherit", c("inherit", "none", "x", "y"))
  link_strip <- arg_match0(link_strip %||% "inherit", c("inherit", "blank"))
  annotation_axis <- arg_match0(annotation_axis %||% "all", c("all", "bottom"))
  vertical <- arg_match0(vertical %||% "default", c("default", "center"))
  reverse_x <- .validate_facet_reverse_x(reverse_x)
  reverse_x_match_by <- match.arg(reverse_x_match_by)
  link_panel_height <- .validate_link_panel_height(link_panel_height)

  if (nchar(dir) == 1) {
    dir <- base::switch(
      dir,
      h = if (as.table) "lt" else "lb",
      v = if (as.table) "tl" else "tr"
    )
  }
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )

  # If scales are free, always draw the axes
  requested_axes <- arg_match0(
    axes,
    c("margins", "all_x", "all_y", "all")
  )
  draw_axes <- list(
    x = free$x || any(requested_axes %in% c("all_x", "all")),
    y = free$y || any(requested_axes %in% c("all_y", "all"))
  )

  # Omitting labels is special-cased internally, so only omit labels if
  # scales are not free and the axis is to be drawn
  requested_axis_labels <- arg_match0(
    axis.labels,
    c("margins", "all_x", "all_y", "all")
  )
  axis_labels <- list(
    x = free$x || !draw_axes$x ||
      any(requested_axis_labels %in% c("all_x", "all")),
    y = free$y || !draw_axes$y ||
      any(requested_axis_labels %in% c("all_y", "all"))
  )

  # Check for deprecated labellers
  labeller <- ggplot2:::validate_labeller(labeller)

  # Flatten all facets dimensions into a single one
  facets <- ggplot2:::compact_facets(facets)


  strip.position <- arg_match0(strip.position, c("top", "bottom", "left", "right"))

  check_number_whole(ncol, allow_null = TRUE, min = 1)
  check_number_whole(nrow, allow_null = TRUE, min = 1)

  if (identical(dir, "v")) {
    # swap
    tmp <- ncol
    ncol <- nrow
    nrow <- tmp
  }

  ggproto(NULL, FacetGenomics,
    shrink = shrink,
    params = list(
      facets = facets,
      free = free,
      as.table = as.table,
      strip.position = strip.position,
      drop = drop,
      ncol = ncol,
      nrow = nrow,
      labeller = labeller,
      dir = dir,
      draw_axes = draw_axes,
      axis_labels = axis_labels,
      requested_axes = requested_axes,
      requested_axis_labels = requested_axis_labels,
      panel_xlim = xlim,
      panel_xlim_chr = xlim_chr,
      link_panel_height = link_panel_height,
      link_axis = link_axis,
      link_strip = link_strip,
      annotation_axis = annotation_axis,
      vertical = vertical,
      reverse_x = reverse_x,
      reverse_x_match_by = reverse_x_match_by
    )
  )
}

.validate_facet_reverse_x <- function(reverse_x) {
  if (is.null(reverse_x)) {
    return(NULL)
  }

  if (is.logical(reverse_x)) {
    if (length(reverse_x) != 1L || is.na(reverse_x)) {
      cli::cli_abort("{.arg reverse_x} must be {.code NULL}, one logical value, or a character vector.")
    }
    return(if (isTRUE(reverse_x)) TRUE else NULL)
  }

  if (is.factor(reverse_x)) {
    reverse_x <- as.character(reverse_x)
  }
  if (is.character(reverse_x)) {
    reverse_x <- unique(reverse_x[!is.na(reverse_x) & nzchar(reverse_x)])
    if (length(reverse_x) == 0L) {
      return(NULL)
    }
    return(reverse_x)
  }

  cli::cli_abort("{.arg reverse_x} must be {.code NULL}, one logical value, or a character vector.")
}

.validate_link_panel_height <- function(link_panel_height) {
  if (is.null(link_panel_height)) {
    return(NULL)
  }
  if (inherits(link_panel_height, "unit")) {
    if (length(link_panel_height) != 1L) {
      cli::cli_abort("{.arg link_panel_height} must be a single grid unit.")
    }
    return(link_panel_height)
  }

  value <- suppressWarnings(as.numeric(link_panel_height))
  if (length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0) {
    cli::cli_abort("{.arg link_panel_height} must be one positive number or one grid unit.")
  }
  value
}

#' ggproto backend for `facet_genomics()`
#'
#' `FacetGenomics` extends ggplot2's `FacetWrap` with Syn-aware layout logic.
#' The main custom responsibilities are:
#'
#' - deciding whether to use a stored `SynLayout`, derive a new comparative
#'   chain layout, or fall back to standard wrap-style faceting
#' - inserting first-class coverage rows ahead of annotation and link rows
#' - mapping layers to role-qualified `(panel_type, track)` panels
#' - inheriting genomic x windows, reversal, and scales from resolved
#'   annotation sources while supporting standalone coverage coordinates
#' - allocating role-aware inherited y-scale identities
#' - reordering link panels so they sit between the relevant annotation panels
#' - annotating link panels with source panel ids (`t_panel`, `q_panel`)
#' - assigning vertical anchor directions (`target_anchor_y`,
#'   `query_anchor_y`) for link layers
#'
#' These panel-level decisions are consumed later by `Layout2` and
#' `geom_nuclink()`.
#'
#' @section Key methods:
#' \describe{
#'   \item{`compute_layout()`}{Chooses the panel table. For `SynSpecies`
#'   data it prefers an explicit layout override, then a stored `SynLayout`
#'   whenever one is available, then a derived chain layout, and finally a
#'   standard wrap layout. Coverage requests are inserted as first-class panel
#'   rows before the comparative layout is finalized.}
#'   \item{`compute_alignment_layout()`}{Reorders link panels relative to their
#'   neighboring annotation panels and annotates the resulting layout with
#'   source panel ids.}
#'   \item{`map_link_direction()`}{Adds vertical link anchor metadata to link
#'   layer data based on whether the target species sits above or below the link
#'   panel in the resolved layout.}
#' }
#'
#' @seealso [SynLayout]
#' @export
FacetGenomics <- ggproto("FacetGenomics", FacetWrap,

  # Custom helper used by ggexon to classify layers as annotation vs link-like
  # from their columns before a Syn-aware panel layout is assembled.
    compute_layer_type = function(data, params) {
    layer_type <- list(lapply(data, function(df) {

    if (is_waiver(df)){ return(NA) }

    if (.is_link_like_df(df)) {
    return("link")
    } else if (.is_annotation_like_df(df)) {
    return("annotation")
    } else {
    cli::cli_abort(c("Can not detect whether the layer data is annotation-like or link-like."))
    return(NA)
    }
    }))

    params$.layer_type = c(unlist(layer_type))
    params
    },



  # Override the standard facet layout selection so SynSpecies plots can reuse
  # a stored SynLayout or derive a comparative chain layout with link panels.
  compute_layout = function(self, data, params) {
    vars <- params$facets

    if (methods::is(params$plot_data, "SynSpecies")) {
      coverage_tracks <- .ordered_coverage_tracks_from_layers(data, params)
      coverage_windows <- .syn_context_coverage_windows(params)

      if (!is.null(params$layout_override)) {
        # Highest priority: use an explicit layout override attached during
        # build, e.g. from species_layout(sp) or a layer-provided override.
        role_layout <- .prepend_synspecies_coverage_rows(
          params$layout_override,
          coverage_tracks,
          coverage_windows
        )
        role_layout <- .apply_layer_panel_metadata(role_layout, data)
        finalized <- .finalize_synspecies_layout_scales(
          role_layout,
          free = params$free,
          panel_scale_specs = params$panel_scale_specs
        )
        return(
          syn_layout_panels(
            .apply_facet_panel_xlim_to_layout(
              finalized,
              plot_data = params$plot_data,
              params = params
            )
          )
        )
      }

      stored_layout <- species_layout(params$plot_data)
      if (!is.null(stored_layout)) {
        stored_layout <- .filter_stored_syn_layout(
          stored_layout,
          annotation_species = .annotation_species_from_layers(data),
          link_pairs = .link_pairs_from_layers(data),
          coverage_tracks = coverage_tracks
        )
        role_layout <- .prepend_synspecies_coverage_rows(
          stored_layout,
          coverage_tracks,
          coverage_windows
        )
        role_layout <- .apply_layer_panel_metadata(role_layout, data)
        finalized <- .finalize_synspecies_layout_scales(
          role_layout,
          free = params$free,
          panel_scale_specs = params$panel_scale_specs
        )
        return(
          syn_layout_panels(
            .apply_facet_panel_xlim_to_layout(
              finalized,
              plot_data = params$plot_data,
              params = params
            )
          )
        )
      }

      # Otherwise derive a SynSpecies layout from the layers participating in
      # this plot (annotation species and requested link pairs).
      plot_layout <- synspecies_chain_layout(
        x = params$plot_data,
        vars = vars,
        free = params$free,
        annotation_species = .annotation_species_from_layers(data),
        link_pairs = .link_pairs_from_layers(data),
        allow_annotation_only = length(coverage_tracks) > 0L,
        panel_scale_specs = params$panel_scale_specs
      )
      if (!is.null(plot_layout) || length(coverage_tracks) > 0L) {
        if (is.null(plot_layout)) {
          plot_layout <- .empty_synspecies_layout_panels()
        }
        plot_layout <- .prepend_synspecies_coverage_rows(
          plot_layout,
          coverage_tracks,
          coverage_windows
        )
        plot_layout <- .apply_layer_panel_metadata(plot_layout, data)
        plot_layout <- .finalize_synspecies_layout_scales(
          plot_layout,
          free = params$free,
          layout_type = "chain",
          panel_scale_specs = params$panel_scale_specs
        )
        return(
          syn_layout_panels(
            .apply_facet_panel_xlim_to_layout(
              plot_layout,
              plot_data = params$plot_data,
              params = params
            )
          )
        )
      }

      annotation_scale_specified <- !is.null(
        params$panel_scale_specs$annotation
      )
      standard_layout <- if (annotation_scale_specified) {
        .compute_role_aware_standard_genomics_layout(data, params, self)
      } else {
        .compute_standard_genomics_layout(data, params, self)
      }
      if (.has_facet_panel_xlim(params)) {
        return(
          syn_layout_panels(
            .apply_facet_panel_xlim_to_layout(
              as_syn_layout(standard_layout, free = params$free),
              plot_data = params$plot_data,
              params = params
            )
          )
        )
      }

      if (annotation_scale_specified) {
        standard_layout <- .assign_role_aware_y_scales(
          standard_layout,
          free = params$free,
          panel_scale_specs = params$panel_scale_specs
        )
      }
      return(standard_layout)
    }

    individual_coverage_tracks <- .ordered_coverage_tracks_from_layers(
      data,
      params
    )
    if (methods::is(params$plot_data, "SynIndividual") &&
        (length(individual_coverage_tracks) > 0L ||
          !is.null(params$panel_scale_specs$annotation))) {
      role_layout <- .compute_role_aware_standard_genomics_layout(
        data,
        params,
        self,
        coverage_tracks = individual_coverage_tracks
      )
      role_layout <- .apply_manual_facet_panel_xlim_to_layout(
        role_layout,
        params
      )
      role_layout <- .annotate_coverage_x_source_panels(role_layout)
      return(.assign_role_aware_y_scales(
        role_layout,
        free = params$free,
        panel_scale_specs = params$panel_scale_specs
      ))
    }

    if (length(vars) == 0) {
      return(layout_null())
    }
    standard_layout <- .compute_standard_genomics_layout(data, params, self)
    .apply_manual_facet_panel_xlim_to_layout(standard_layout, params)
  },

  # Syn-aware layers map only to panels with the same semantic role. The
  # public `track` label remains unchanged; a private role-qualified value is
  # substituted only while FacetWrap performs its panel join.
  map_data = function(data, layout, params) {
    standard_map <- function(data, layout) {
      ggplot2::FacetWrap$map_data(data, layout, params)
    }

    role_aware_syn_input <- methods::is(params$plot_data, "SynSpecies") ||
      methods::is(params$plot_data, "SynIndividual")
    if (!role_aware_syn_input ||
        !all(c("panel_type", "track") %in% names(layout))) {
      return(standard_map(data, layout))
    }

    has_explicit_role <- !is.null(
      attr(data, "ggexon_panel_role", exact = TRUE)
    ) || ".ggexon_panel_role" %in% names(data)
    if (!has_explicit_role &&
        !.is_link_like_df(data) &&
        !.is_annotation_like_df(data)) {
      return(standard_map(data, layout))
    }

    role <- if (!has_explicit_role && .is_link_like_df(data)) {
      "link"
    } else {
      .syn_layer_panel_role(data)
    }
    role_rows <- as.character(layout$panel_type) == role
    role_rows[is.na(role_rows)] <- FALSE
    role_layout <- layout[role_rows, , drop = FALSE]

    if (nrow(role_layout) == 0L) {
      data$PANEL <- rep(NA_integer_, nrow(data))
      return(data)
    }
    if (nrow(data) == 0L) {
      return(standard_map(data, role_layout))
    }

    role_layout$track <- .syn_role_panel_key(
      role_layout$panel_type,
      role_layout$track
    )
    if (!"track" %in% names(data)) {
      return(standard_map(data, role_layout))
    }

    public_track_col <- ".ggexon_public_track"
    while (public_track_col %in% names(data)) {
      public_track_col <- paste0(public_track_col, "_")
    }
    data[[public_track_col]] <- data$track
    data$track <- .syn_role_panel_key(role, data$track)
    mapped <- standard_map(data, role_layout)
    mapped$track <- mapped[[public_track_col]]
    mapped[[public_track_col]] <- NULL
    mapped
  },

  # ggexon-specific post-processing step: reorder link panels so they sit
  # between the relevant annotation panels and annotate each link row with the
  # source annotation panels it should borrow x ranges from.
  compute_alignment_layout = function(self, data, layout){

    # compuate link data panel number
    species_aln_list = lapply(data, function(df){
      if("tspecies" %in% colnames(df)){
        species_df = unique(df[ ,c("tspecies", "qspecies", "track")])
      }
    })
    species_aln_list <- do.call(rbind, Filter(Negate(is.null), species_aln_list))

    # PANEL ROW COL track SCALE_X SCALE_Y tspecies qspecies
    layout2 = left_join(layout, species_aln_list, join_by(track == track))

    # Sort each column so link panels sit between the annotation panels they
    # connect, rather than where wrap-layout ordering happened to place them.
    split_by_col = split(layout2, layout2$COL)

    split_by_col = lapply(split_by_col, function(df){
      if (nrow(df) == 0L) {
        return(df)
      }

      df$.row_id <- seq_len(nrow(df))
      annotation_rows <- df[
        !stringr::str_detect(df$track, "link"),
        c("track", ".row_id"),
        drop = FALSE
      ]

      sort_key <- df$.row_id
      is_link_row <- stringr::str_detect(df$track, "link")

      if (nrow(annotation_rows) > 0L && any(is_link_row)) {
        upper_pos <- pmin(
          match(df$tspecies, annotation_rows$track),
          match(df$qspecies, annotation_rows$track),
          na.rm = TRUE
        )
        valid_links <- is_link_row & is.finite(upper_pos)
        sort_key[valid_links] <- annotation_rows$.row_id[upper_pos[valid_links]] + 0.5
      }

      df <- df[order(sort_key, df$.row_id), , drop = FALSE]
      df$.row_id <- NULL
      df
    })

    link_layout = do.call(rbind, Filter(Negate(is.null), split_by_col))
    rownames(link_layout) = 1:nrow(link_layout)
    link_layout$PANEL = 1:nrow(link_layout)
    link_layout$ROW = 1:nrow(link_layout)


    if (length(unique(link_layout$SCALE_X)) == 1){
      link_layout$SCALE_X = 1
    }else{
      link_layout$SCALE_X = sort(link_layout$SCALE_X)
    }

    if (length(unique(link_layout$SCALE_Y)) == 1){
      link_layout$SCALE_Y = 1
    }else{
      link_layout$SCALE_Y = sort(link_layout$SCALE_Y)
    }

    link_layout <- .annotate_link_source_panels(link_layout)

    #print(link_layout)
    link_layout

  },

  # Assign top/bottom anchors inside each link panel based on the resolved
  # panel order, so geom_nuclink() knows which species should attach to the
  # upper vs lower edge of the link panel.
  map_link_direction = function(self, data, layout){

    link_rows <- if ("panel_type" %in% names(layout)) {
      link_panel_type(layout) == "link"
    } else {
      stringr::str_detect(layout$track, "link")
    }
    link_rows[is.na(link_rows)] <- FALSE
    link_layout = layout[link_rows, , drop = FALSE]


    link_y_list = list()
    for (i in 1:nrow(link_layout)) {
      link_index = match(link_layout$PANEL[i], layout$PANEL)


      tspecies = link_layout[i,"tspecies"]
      qspecies = link_layout[i, "qspecies"]
      target_source <- if ("t_panel" %in% names(link_layout)) {
        match(link_layout$t_panel[[i]], layout$PANEL)
      } else {
        NA_integer_
      }
      query_source <- if ("q_panel" %in% names(link_layout)) {
        match(link_layout$q_panel[[i]], layout$PANEL)
      } else {
        NA_integer_
      }
      source_order_known <- !is.na(target_source) && !is.na(query_source)
      target_is_upper <- if (source_order_known && "ROW" %in% names(layout)) {
        layout$ROW[[target_source]] < layout$ROW[[query_source]]
      } else if (source_order_known) {
        target_source < query_source
      } else {
        upper_panel_species <- if (!is.na(link_index) && link_index > 1L) {
          layout[link_index - 1L, "track"]
        } else {
          NA_character_
        }
        !is.na(upper_panel_species) && upper_panel_species == tspecies
      }

      if (isTRUE(target_is_upper)) {
        target_anchor_y = 1
        query_anchor_y = 0
      }else{
        target_anchor_y = 0
        query_anchor_y = 1
      }
      link_y_list[[length(link_y_list) + 1L]] <- data.frame(
        link = as.character(link_layout[i, "track"]),
        target_anchor_y = target_anchor_y,
        query_anchor_y = query_anchor_y,
        stringsAsFactors = FALSE
      )

    }
    link_y_table = bind_rows(link_y_list)

    data = lapply(data, function(df){
      if (is_waiver(df)){ df }

      if (.is_link_like_df(df)){
        #print(df)
        df$track <- as.character(df$track)
        df = left_join(df, link_y_table, join_by(track == link))
        #print(df)
        #print(df)
        df
      }else{df}

    })
    #lapply(data, function(df) {print(colnames(df))})
    data
  }
)

.has_facet_panel_xlim <- function(params) {
  !is.null(params$panel_xlim) || !is.null(params$panel_xlim_chr)
}

.facet_panel_xlim_individuals <- function(params, available = NULL) {
  xlim <- params$panel_xlim
  xlim_chr <- params$panel_xlim_chr

  if (is.null(xlim) && !is.null(xlim_chr)) {
    cli::cli_abort("{.arg xlim} must be supplied when {.arg xlim_chr} is supplied to {.fn facet_genomics}.")
  }

  named_from_xlim <- if (is.list(xlim)) {
    names(xlim)
  } else {
    character()
  }
  named_from_chr <- if (is.list(xlim_chr) || (is.character(xlim_chr) && !is.null(names(xlim_chr)))) {
    names(xlim_chr)
  } else {
    character()
  }
  individual <- unique(c(named_from_xlim, named_from_chr))
  individual <- individual[!is.na(individual) & nzchar(individual)]

  if (length(individual) > 0L) {
    return(individual)
  }

  if (is.numeric(xlim) && length(xlim) == 2L) {
    available <- unique(as.character(available %||% character()))
    available <- available[!is.na(available) & nzchar(available)]
    if (length(available) == 1L) {
      return(available)
    }
  }

  cli::cli_abort(
    c(
      "{.arg xlim} in {.fn facet_genomics} must name eligible panel limits by panel name.",
      "i" = "Use a named list such as {.code list(N2 = c(20450000, 20470000), XZ1516 = c(21574000, 21585000))}."
    )
  )
}

.layout_annotation_individuals <- function(layout) {
  panels <- syn_layout_panels(layout)
  if (!is.data.frame(panels) || nrow(panels) == 0L) {
    return(character())
  }

  annotation_rows <- if ("panel_type" %in% names(panels)) {
    is.na(panels$panel_type) | panels$panel_type == "annotation"
  } else {
    rep(TRUE, nrow(panels))
  }
  species_col <- if ("species" %in% names(panels)) {
    as.character(panels$species)
  } else {
    as.character(panels$track)
  }

  unique(species_col[annotation_rows & !is.na(species_col) & nzchar(species_col)])
}

.apply_facet_panel_xlim_to_layout <- function(layout, plot_data, params) {
  if (!.has_facet_panel_xlim(params)) {
    return(layout)
  }
  if (!methods::is(plot_data, "SynSpecies")) {
    cli::cli_abort("{.arg xlim} in {.fn facet_genomics} currently requires a {.cls SynSpecies} plot.")
  }

  layout <- as_syn_layout(layout, free = params$free)
  panels <- syn_layout_panels(layout)
  panel_roles <- link_panel_type(panels)
  has_annotation <- any(panel_roles == "annotation", na.rm = TRUE)
  has_coverage <- any(panel_roles == "coverage", na.rm = TRUE)
  if (!has_annotation && has_coverage) {
    panels <- .apply_manual_facet_panel_xlim_to_layout(panels, params)
    resolved_free <- params$free
    complete_windows <- !is.na(panels$xlim_min) &
      !is.na(panels$xlim_max) &
      is.finite(panels$xlim_min) &
      is.finite(panels$xlim_max)
    if (sum(complete_windows) > 1L) {
      window_keys <- paste(
        as.character(panels$xlim_chr[complete_windows]),
        panels$xlim_min[complete_windows],
        panels$xlim_max[complete_windows],
        sep = "\r"
      )
      resolved_free$x <- isTRUE(resolved_free$x) ||
        length(unique(window_keys)) > 1L
    }
    updated_layout <- SynLayout(
      panels = panels,
      layout_type = layout@layout_type,
      free = resolved_free,
      exon_height = layout@exon_height,
      x_translation = layout@x_translation,
      metadata = layout@metadata
    )
    return(.finalize_synspecies_layout_scales(
      updated_layout,
      free = resolved_free,
      layout_type = updated_layout@layout_type,
      panel_scale_specs = params$panel_scale_specs
    ))
  }
  individual <- .facet_panel_xlim_individuals(
    params,
    available = .layout_annotation_individuals(layout)
  )

  syn_data <- plot_data
  species_layout(syn_data) <- layout
  syn_data <- .set_panel_xlim_on_synspecies_or_layout(
    syn_data,
    individual = individual,
    xlim = params$panel_xlim,
    xlim_chr = params$panel_xlim_chr,
    seed_other_panels = FALSE
  )

  updated_layout <- species_layout(syn_data)
  refinalize_free <- updated_layout@free
  refinalize_free$y <- params$free$y
  .finalize_synspecies_layout_scales(
    updated_layout,
    free = refinalize_free,
    layout_type = updated_layout@layout_type,
    panel_scale_specs = params$panel_scale_specs
  )
}

.apply_manual_facet_panel_xlim_to_layout <- function(layout, params) {
  if (!.has_facet_panel_xlim(params)) {
    return(layout)
  }
  if (!is.data.frame(layout) || nrow(layout) == 0L || !"track" %in% names(layout)) {
    return(layout)
  }

  panel_type <- link_panel_type(layout)
  target_rows <- panel_type == "annotation"
  target_rows[is.na(target_rows)] <- FALSE
  if (!any(target_rows)) {
    target_rows <- panel_type == "coverage"
    target_rows[is.na(target_rows)] <- FALSE
  }
  identity_columns <- intersect(
    c("track", "individual", "species"),
    names(layout)
  )
  available <- unique(unlist(lapply(identity_columns, function(column) {
    as.character(layout[[column]][target_rows])
  }), use.names = FALSE))
  available <- available[!is.na(available) & nzchar(available)]
  individual <- .facet_panel_xlim_individuals(params, available = available)

  xlim_map <- .facet_panel_xlim_map(individual, params$panel_xlim)
  xlim_chr_map <- .facet_panel_xlim_chr_map(individual, params$panel_xlim_chr)

  if (!"xlim_chr" %in% names(layout)) {
    layout$xlim_chr <- NA_character_
  }
  if (!"xlim_min" %in% names(layout)) {
    layout$xlim_min <- NA_real_
  }
  if (!"xlim_max" %in% names(layout)) {
    layout$xlim_max <- NA_real_
  }

  for (name in names(xlim_map)) {
    hit <- rep(FALSE, nrow(layout))
    for (column in identity_columns) {
      hit <- hit | (
        target_rows & as.character(layout[[column]]) == name
      )
    }
    hit[is.na(hit)] <- FALSE
    if (!any(hit)) {
      cli::cli_abort("{.arg xlim} names must match panel names in {.fn facet_genomics}.")
    }
    limits <- xlim_map[[name]]
    layout$xlim_min[hit] <- min(limits)
    layout$xlim_max[hit] <- max(limits)
    layout$xlim_chr[hit] <- xlim_chr_map[[name]] %||% NA_character_
  }

  layout
}

.facet_panel_xlim_map <- function(individual, xlim) {
  if (is.numeric(xlim) && length(xlim) == 2L && length(individual) == 1L) {
    xlim <- stats::setNames(list(as.numeric(xlim)), individual)
  }
  if (!is.list(xlim) || is.null(names(xlim))) {
    cli::cli_abort(
      c(
        "{.arg xlim} in {.fn facet_genomics} must name eligible panel limits by panel name.",
        "i" = "Use a named list such as {.code list(human = c(1, 100), mouse = c(500, 900))}."
      )
    )
  }
  if (!all(names(xlim) %in% individual)) {
    cli::cli_abort("{.arg xlim} contains names that do not match eligible panels.")
  }

  lapply(xlim, function(limits) {
    if (!is.numeric(limits) || length(limits) != 2L || anyNA(limits)) {
      cli::cli_abort("Each {.arg xlim} entry must be a numeric vector of length 2.")
    }
    as.numeric(limits)
  })
}

.facet_panel_xlim_chr_map <- function(individual, xlim_chr) {
  if (is.null(xlim_chr)) {
    return(stats::setNames(as.list(rep(NA_character_, length(individual))), individual))
  }
  if (is.character(xlim_chr) && length(xlim_chr) == 1L && is.null(names(xlim_chr)) && length(individual) == 1L) {
    return(stats::setNames(list(xlim_chr), individual))
  }
  if (is.character(xlim_chr) && !is.null(names(xlim_chr))) {
    xlim_chr <- as.list(xlim_chr)
  }
  if (!is.list(xlim_chr) || is.null(names(xlim_chr)) || !all(names(xlim_chr) %in% individual)) {
    cli::cli_abort("{.arg xlim_chr} must be NULL, one character value for one panel, or a named vector/list keyed by panel name.")
  }
  out <- stats::setNames(as.list(rep(NA_character_, length(individual))), individual)
  for (name in names(xlim_chr)) {
    value <- xlim_chr[[name]]
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
      cli::cli_abort("Each {.arg xlim_chr} entry must be one non-empty character value.")
    }
    out[[name]] <- value
  }
  out
}

.panel_role_tracks_from_layers <- function(data, role) {
  tracks <- unlist(lapply(data, function(df) {
    if (!is.data.frame(df) || !"track" %in% names(df)) {
      return(character())
    }
    layer_role <- if (.is_link_like_df(df)) {
      "link"
    } else {
      .syn_layer_panel_role(df)
    }
    if (!identical(layer_role, role)) {
      return(character())
    }
    as.character(df$track)
  }), use.names = FALSE)
  tracks <- tracks[!is.na(tracks) & nzchar(tracks)]
  unique(tracks)
}

.ordered_coverage_tracks_from_layers <- function(data, params) {
  requests <- params$syn_plot_context$coverage_requests %||% list()
  request_indices <- vapply(requests, function(request) {
    as.integer(request$layer_index %||% NA_integer_)[[1L]]
  }, integer(1))
  valid_indices <- request_indices[!is.na(request_indices) & request_indices > 0L]
  layer_count <- max(
    c(length(data) - 1L, valid_indices, 0L),
    na.rm = TRUE
  )

  tracks <- character()
  if (layer_count > 0L) {
    for (layer_index in seq_len(layer_count)) {
      request_tracks <- unlist(lapply(
        requests[request_indices == layer_index],
        function(request) request$tracks %||% character()
      ), use.names = FALSE)
      data_index <- layer_index + 1L
      layer_tracks <- if (data_index <= length(data)) {
        .panel_role_tracks_from_layers(
          list(data[[data_index]]),
          "coverage"
        )
      } else {
        character()
      }
      tracks <- c(tracks, request_tracks, layer_tracks)
    }
  }

  tracks <- c(tracks, .syn_context_coverage_tracks(params))
  tracks <- as.character(tracks)
  unique(tracks[!is.na(tracks) & nzchar(tracks)])
}

.layer_panel_metadata <- function(data) {
  rows <- lapply(data, function(df) {
    if (!is.data.frame(df) || nrow(df) == 0L || !"track" %in% names(df)) {
      return(NULL)
    }
    role <- if (.is_link_like_df(df)) "link" else .syn_layer_panel_role(df)
    if (!role %in% c("annotation", "coverage")) {
      return(NULL)
    }
    out <- data.frame(
      panel_type = role,
      track = as.character(df$track),
      individual = NA_character_,
      species = NA_character_,
      stringsAsFactors = FALSE
    )
    for (column in c("individual", "species")) {
      if (column %in% names(df)) {
        out[[column]] <- as.character(df[[column]])
      }
    }
    out
  })
  rows <- dplyr::bind_rows(Filter(Negate(is.null), rows))
  if (nrow(rows) == 0L) {
    return(rows)
  }
  rows <- rows[
    !is.na(rows$track) & nzchar(rows$track),
    ,
    drop = FALSE
  ]
  keys <- .syn_role_panel_key(rows$panel_type, rows$track)
  key_order <- unique(keys)
  collapsed <- lapply(key_order, function(key) {
    candidates <- rows[keys == key, , drop = FALSE]
    out <- candidates[1L, , drop = FALSE]
    for (column in c("individual", "species")) {
      values <- candidates[[column]]
      values <- values[!is.na(values) & nzchar(values)]
      out[[column]] <- if (length(values) == 0L) NA_character_ else values[[1L]]
    }
    out
  })
  dplyr::bind_rows(collapsed)
}

.apply_layer_panel_metadata <- function(layout, data) {
  metadata <- .layer_panel_metadata(data)
  if (nrow(metadata) == 0L) {
    return(layout)
  }

  layout_is_object <- methods::is(layout, "SynLayout")
  panels <- if (layout_is_object) syn_layout_panels(layout) else layout
  if (!is.data.frame(panels) || nrow(panels) == 0L ||
      !all(c("track", "panel_type") %in% names(panels))) {
    return(layout)
  }
  for (column in c("individual", "species")) {
    if (!column %in% names(panels)) {
      panels[[column]] <- NA_character_
    } else {
      panels[[column]] <- as.character(panels[[column]])
    }
  }

  panel_roles <- as.character(panels$panel_type)
  panel_tracks <- as.character(panels$track)
  for (i in seq_len(nrow(metadata))) {
    hit <- panel_roles == metadata$panel_type[[i]] &
      panel_tracks == metadata$track[[i]]
    hit[is.na(hit)] <- FALSE
    for (column in c("individual", "species")) {
      value <- metadata[[column]][[i]]
      if (is.na(value) || !nzchar(value)) {
        next
      }
      current <- panels[[column]]
      replace <- hit & (
        is.na(current) | !nzchar(current) | current == panel_tracks
      )
      replace[is.na(replace)] <- FALSE
      panels[[column]][replace] <- value
    }
  }

  if (!layout_is_object) {
    return(panels)
  }
  SynLayout(
    panels = panels,
    layout_type = layout@layout_type,
    free = layout@free,
    exon_height = layout@exon_height,
    x_translation = layout@x_translation,
    metadata = layout@metadata
  )
}

.compute_role_aware_standard_genomics_layout <- function(data,
                                                         params,
                                                         facet,
                                                         coverage_tracks = character()) {
  coverage_tracks <- unique(as.character(coverage_tracks %||% character()))
  coverage_tracks <- coverage_tracks[
    !is.na(coverage_tracks) & nzchar(coverage_tracks)
  ]
  if (length(coverage_tracks) > 0L) {
    coverage_seed <- data.frame(
      track = coverage_tracks,
      stringsAsFactors = FALSE
    )
    attr(coverage_seed, "ggexon_panel_role") <- "coverage"
    data <- c(data, list(coverage_seed))
  }

  role_map <- list()
  qualified_data <- lapply(data, function(df) {
    if (!is.data.frame(df) || !"track" %in% names(df)) {
      return(df)
    }
    role <- if (.is_link_like_df(df)) "link" else .syn_layer_panel_role(df)
    if (!role %in% c("annotation", "coverage", "link")) {
      return(df)
    }

    public_track <- as.character(df$track)
    if (length(public_track) == 0L) {
      return(df)
    }
    qualified_track <- .syn_role_panel_key(role, public_track)
    role_map[[length(role_map) + 1L]] <<- data.frame(
      qualified_track = qualified_track,
      track = public_track,
      panel_type = role,
      stringsAsFactors = FALSE
    )
    df$track <- qualified_track
    df
  })

  layout <- .compute_standard_genomics_layout(qualified_data, params, facet)
  role_map <- unique(dplyr::bind_rows(role_map))
  matched <- match(as.character(layout$track), role_map$qualified_track)
  layout$track <- role_map$track[matched]
  layout$panel_type <- role_map$panel_type[matched]
  panel_positions <- layout[
    order(as.integer(layout$PANEL)),
    c("PANEL", "ROW", "COL"),
    drop = FALSE
  ]
  role_rank <- match(layout$panel_type, c("coverage", "annotation", "link"))
  layout <- layout[
    order(role_rank, as.integer(layout$PANEL), na.last = TRUE),
    ,
    drop = FALSE
  ]
  layout$PANEL <- panel_positions$PANEL
  layout$ROW <- panel_positions$ROW
  layout$COL <- panel_positions$COL
  rownames(layout) <- NULL
  .apply_layer_panel_metadata(layout, data)
}

.assign_role_aware_y_scales <- function(layout,
                                        free,
                                        panel_scale_specs = list()) {
  roles <- link_panel_type(layout)
  policies <- .resolve_present_panel_y_policies(
    roles,
    specs = panel_scale_specs,
    free = free
  )
  groups <- vapply(seq_len(nrow(layout)), function(i) {
    role <- roles[[i]]
    policy <- policies[[role]] %||% .facet_y_policy(free)
    if (role %in% c("annotation", "coverage")) {
      if (identical(policy, "fixed_y")) {
        return(paste0(role, ":shared"))
      }
      return(paste0(role, ":", layout$PANEL[[i]]))
    }
    if (identical(role, "link")) {
      return("link:shared")
    }
    if (identical(policy, "free_y")) {
      paste0("role:", role, ":", layout$PANEL[[i]])
    } else {
      paste0("role:", role, ":shared")
    }
  }, character(1))
  layout$SCALE_Y <- match(groups, unique(groups))
  layout
}

.compute_standard_genomics_layout <- function(data, params, facet) {
  vars <- params$facets
  ggplot2:::check_facet_vars(names(vars), name = snake_class(facet))

  base <- ggplot2::combine_vars(data, params$plot_env, vars, drop = params$drop)

  id <- ggplot2:::id(base, drop = TRUE)
  n <- attr(id, "n")

  dims <- ggplot2::wrap_dims(n, params$nrow, params$ncol)
  layout <- ggplot2:::wrap_layout(id, dims, params$dir)

  panels <- vec_cbind(layout, base)
  panels <- panels[order(panels$PANEL), , drop = FALSE]
  rownames(panels) <- NULL

  panels$SCALE_X <- if (params$free$x) seq_len(n) else 1L
  panels$SCALE_Y <- if (params$free$y) seq_len(n) else 1L

  panels
}

.detect_link_layers <- function(data) {
  any(vapply(data, function(df) {
    if (!is.data.frame(df)) {
      return(FALSE)
    }
    any(c("tspecies", "qspecies") %in% names(df)) ||
      sum(grepl("start", colnames(df), ignore.case = TRUE)) >= 2L
  }, logical(1)))
}

.syn_context_coverage_tracks <- function(params) {
  tracks <- params$syn_plot_context$coverage_tracks %||% character()
  tracks <- as.character(tracks)
  unique(tracks[!is.na(tracks) & nzchar(tracks)])
}

.syn_context_coverage_windows <- function(params) {
  tracks <- .syn_context_coverage_tracks(params)
  windows <- params$syn_plot_context$windows %||% list()
  stats::setNames(lapply(tracks, function(track) {
    window <- windows[[track]] %||% list()
    if (is.data.frame(window)) as.list(window) else window
  }), tracks)
}

.empty_synspecies_layout_panels <- function() {
  data.frame(
    PANEL = integer(),
    ROW = integer(),
    COL = integer(),
    track = character(),
    panel_type = character(),
    species = character(),
    alignment_name = character(),
    tspecies = character(),
    qspecies = character(),
    stringsAsFactors = FALSE
  )
}

.new_synspecies_coverage_panel <- function(track, window = list()) {
  individual <- as.character(
    window$individual %||% window$species %||% track
  )[[1L]]
  chr <- as.character(window$chr %||% NA_character_)[[1L]]
  start <- suppressWarnings(as.numeric(window$start %||% NA_real_))[[1L]]
  end <- suppressWarnings(as.numeric(window$end %||% NA_real_))[[1L]]
  data.frame(
    PANEL = NA_integer_,
    ROW = NA_integer_,
    COL = 1L,
    track = track,
    panel_type = "coverage",
    individual = individual,
    species = individual,
    alignment_name = NA_character_,
    tspecies = NA_character_,
    qspecies = NA_character_,
    xlim_chr = chr,
    xlim_min = start,
    xlim_max = end,
    .ggexon_coverage_window_explicit = FALSE,
    stringsAsFactors = FALSE
  )
}

.fill_synspecies_coverage_panel <- function(row, track, window = list()) {
  defaults <- .new_synspecies_coverage_panel(track, window)
  for (column in names(defaults)) {
    if (!column %in% names(row)) {
      row[[column]] <- defaults[[column]]
      next
    }
    missing <- is.na(row[[column]])
    if (is.character(row[[column]])) {
      missing <- missing | !nzchar(row[[column]])
    }
    row[[column]][missing] <- defaults[[column]][missing]
  }
  row$track <- track
  row$panel_type <- "coverage"
  row
}

.prepend_synspecies_coverage_rows <- function(layout,
                                              coverage_tracks,
                                              coverage_windows = list()) {
  layout_obj <- as_syn_layout(layout)
  panels <- syn_layout_panels(layout_obj)
  panels <- as.data.frame(panels, stringsAsFactors = FALSE)
  panels$panel_type <- link_panel_type(panels)

  coverage_rows <- panels$panel_type == "coverage"
  coverage_rows[is.na(coverage_rows)] <- FALSE
  if (!".ggexon_coverage_window_explicit" %in% names(panels)) {
    panels$.ggexon_coverage_window_explicit <- rep(NA, nrow(panels))
  }
  stored_complete <- coverage_rows &
    "xlim_min" %in% names(panels) &
    "xlim_max" %in% names(panels) &
    is.finite(suppressWarnings(as.numeric(panels$xlim_min))) &
    is.finite(suppressWarnings(as.numeric(panels$xlim_max)))
  infer_explicit <- coverage_rows &
    is.na(panels$.ggexon_coverage_window_explicit)
  panels$.ggexon_coverage_window_explicit[infer_explicit] <-
    stored_complete[infer_explicit]
  stored_coverage <- panels[coverage_rows, , drop = FALSE]
  annotation_link_chain <- panels[!coverage_rows, , drop = FALSE]

  # Realize any stored factor-based ordering before adding coverage rows, then
  # keep that annotation/link chain in the same relative order below coverage.
  annotation_link_chain <- .normalize_synspecies_layout_order(
    annotation_link_chain
  )
  if ("track" %in% names(annotation_link_chain) &&
      is.factor(annotation_link_chain$track)) {
    annotation_link_chain$track <- as.character(annotation_link_chain$track)
  }

  coverage_tracks <- unique(as.character(coverage_tracks %||% character()))
  coverage_tracks <- coverage_tracks[
    !is.na(coverage_tracks) & nzchar(coverage_tracks)
  ]
  if (length(coverage_tracks) == 0L && nrow(stored_coverage) > 0L) {
    coverage_tracks <- unique(as.character(stored_coverage$track))
  }

  stored_tracks <- as.character(stored_coverage$track %||% character())
  complete_stored_role_grid <- nrow(stored_coverage) > 0L &&
    all(c("ROW", "COL") %in% names(panels)) &&
    !anyNA(panels$ROW) && !anyNA(panels$COL) &&
    length(unique(panels$COL)) > 1L &&
    nrow(unique(panels[, c("ROW", "COL"), drop = FALSE])) == nrow(panels)
  all_coverage_already_stored <- length(coverage_tracks) == length(stored_tracks) &&
    !anyDuplicated(stored_tracks) &&
    setequal(coverage_tracks, stored_tracks)
  if (complete_stored_role_grid && all_coverage_already_stored) {
    preserved_rows <- lapply(seq_len(nrow(panels)), function(i) {
      row <- panels[i, , drop = FALSE]
      if (!coverage_rows[[i]]) {
        return(row)
      }
      track <- as.character(row$track[[1L]])
      window <- coverage_windows[[track]] %||% list()
      .fill_synspecies_coverage_panel(row, track, window)
    })
    panels <- dplyr::bind_rows(preserved_rows)
    rownames(panels) <- NULL
    return(SynLayout(
      panels = panels,
      layout_type = layout_obj@layout_type,
      free = layout_obj@free,
      exon_height = layout_obj@exon_height,
      x_translation = layout_obj@x_translation,
      metadata = layout_obj@metadata
    ))
  }

  ordered_coverage <- lapply(coverage_tracks, function(track) {
    window <- coverage_windows[[track]] %||% list()
    matching <- which(as.character(stored_coverage$track) == track)
    if (length(matching) > 0L) {
      row <- stored_coverage[matching[[1L]], , drop = FALSE]
      if (is.factor(row$track)) row$track <- as.character(row$track)
      return(.fill_synspecies_coverage_panel(row, track, window))
    }
    .new_synspecies_coverage_panel(track, window)
  })

  if (length(ordered_coverage) > 0L) {
    ordered_coverage <- lapply(seq_along(ordered_coverage), function(i) {
      row <- ordered_coverage[[i]]
      row$PANEL <- NA_integer_
      row$ROW <- as.integer(i)
      row$COL <- 1L
      row
    })
    complete_stored_grid <- nrow(annotation_link_chain) > 0L &&
      all(c("ROW", "COL") %in% names(annotation_link_chain)) &&
      !anyNA(annotation_link_chain$ROW) &&
      !anyNA(annotation_link_chain$COL)
    if (complete_stored_grid) {
      annotation_link_chain$ROW <- as.integer(
        annotation_link_chain$ROW + length(ordered_coverage)
      )
    }
  }

  panels <- dplyr::bind_rows(
    Filter(Negate(is.null), ordered_coverage),
    annotation_link_chain
  )
  rownames(panels) <- NULL

  SynLayout(
    panels = panels,
    layout_type = layout_obj@layout_type,
    free = layout_obj@free,
    exon_height = layout_obj@exon_height,
    x_translation = layout_obj@x_translation,
    metadata = layout_obj@metadata
  )
}

synspecies_chain_layout <- function(x,
                                    vars,
                                    free,
                                    annotation_species = NULL,
                                    link_pairs = NULL,
                                    allow_annotation_only = FALSE,
                                    panel_scale_specs = list()) {
  if (!methods::is(x, "SynSpecies")) {
    cli::cli_abort("Expected a {.cls SynSpecies} object.")
  }

  if (length(vars) == 0) {
    cli::cli_abort(
      "{.fn facet_genomics} needs a facet variable such as {.code vars(track)} for {.cls SynSpecies} layout."
    )
  }

  var_names <- names(vars)
  var_labels <- unique(c(
    var_names,
    vapply(vars, rlang::as_name, character(1))
  ))
  if (!"track" %in% var_labels) {
    cli::cli_abort(
      "{.cls SynSpecies} layout currently requires {.code vars(track)} in {.fn facet_genomics}."
    )
  }

  if (!is.null(annotation_species) || !is.null(link_pairs)) {
    return(.synspecies_chain_layout_from_layers(
      x = x,
      annotation_species = annotation_species,
      link_pairs = link_pairs,
      free = free,
      allow_annotation_only = allow_annotation_only,
      panel_scale_specs = panel_scale_specs
    ))
  }

  species_order <- synspecies_chain_species_order(x)
  pair_rows <- synspecies_chain_alignment_rows(x, species_order)
  panel_rows <- vector("list", length(species_order) + nrow(pair_rows))
  panel_index <- 1L

  for (i in seq_along(species_order)) {
    species_name <- species_order[[i]]
    panel_rows[[panel_index]] <- data.frame(
      PANEL = panel_index,
      ROW = panel_index,
      COL = 1L,
      track = species_name,
      panel_type = "annotation",
      species = species_name,
      alignment_name = NA_character_,
      tspecies = NA_character_,
      qspecies = NA_character_,
      stringsAsFactors = FALSE
    )
    panel_index <- panel_index + 1L

    if (i < length(species_order)) {
      pair_row <- pair_rows[
        pair_rows$left_species == species_name &
          pair_rows$right_species == species_order[[i + 1L]],
        ,
        drop = FALSE
      ]
      if (nrow(pair_row) == 1L) {
        panel_rows[[panel_index]] <- data.frame(
          PANEL = panel_index,
          ROW = panel_index,
          COL = 1L,
          track = pair_row$track,
          panel_type = "link",
          species = NA_character_,
          alignment_name = pair_row$alignment_name,
          tspecies = pair_row$tspecies,
          qspecies = pair_row$qspecies,
          stringsAsFactors = FALSE
        )
        panel_index <- panel_index + 1L
      }
    }
  }

  panels <- dplyr::bind_rows(panel_rows)
  .finalize_synspecies_layout_scales(
    panels,
    free = free,
    layout_type = "chain",
    panel_scale_specs = panel_scale_specs
  )
}

.annotation_species_from_layers <- function(data) {
  tracks <- unique(unlist(lapply(data, function(df) {
    if (!is.data.frame(df) || !"track" %in% names(df)) {
      return(character())
    }
    role <- if (.is_link_like_df(df)) "link" else .syn_layer_panel_role(df)
    if (!identical(role, "annotation")) {
      return(character())
    }
    as.character(df$track)
  })))
  tracks <- tracks[!is.na(tracks) & nzchar(tracks)]
  unique(tracks)
}

.has_coverage_layer_data <- function(data) {
  any(vapply(data, function(df) {
    is.data.frame(df) && identical(.syn_layer_panel_role(df), "coverage")
  }, logical(1)))
}

.link_pairs_from_layers <- function(data) {
  pair_rows <- lapply(data, function(df) {
    if (!is.data.frame(df) || !all(c("track", "tspecies", "qspecies") %in% names(df))) {
      return(NULL)
    }
    unique(data.frame(
      track = as.character(df$track),
      tspecies = as.character(df$tspecies),
      qspecies = as.character(df$qspecies),
      stringsAsFactors = FALSE
    ))
  })

  out <- dplyr::bind_rows(Filter(Negate(is.null), pair_rows))
  if (is.null(out) || nrow(out) == 0L) {
    return(data.frame(
      track = character(),
      tspecies = character(),
      qspecies = character(),
      stringsAsFactors = FALSE
    ))
  }
  annotation_metadata <- .layer_panel_metadata(data)
  annotation_metadata <- annotation_metadata[
    annotation_metadata$panel_type == "annotation",
    ,
    drop = FALSE
  ]
  out$t_track <- .resolve_link_annotation_tracks(
    out$tspecies,
    annotation_metadata,
    endpoint = "target"
  )
  out$q_track <- .resolve_link_annotation_tracks(
    out$qspecies,
    annotation_metadata,
    endpoint = "query"
  )
  out
}

.resolve_link_annotation_tracks <- function(values,
                                             annotation_metadata,
                                             endpoint) {
  values <- as.character(values)
  if (!is.data.frame(annotation_metadata) ||
      nrow(annotation_metadata) == 0L) {
    return(values)
  }

  vapply(values, function(value) {
    if (is.na(value) || !nzchar(value)) {
      return(value)
    }
    exact_tracks <- unique(as.character(
      annotation_metadata$track[annotation_metadata$track == value]
    ))
    if (length(exact_tracks) == 1L) {
      return(exact_tracks[[1L]])
    }

    recipient_hit <- rep(FALSE, nrow(annotation_metadata))
    for (column in intersect(
      c("individual", "species"),
      names(annotation_metadata)
    )) {
      recipient_hit <- recipient_hit |
        as.character(annotation_metadata[[column]]) == value
    }
    recipient_hit[is.na(recipient_hit)] <- FALSE
    recipient_tracks <- unique(as.character(
      annotation_metadata$track[recipient_hit]
    ))
    recipient_tracks <- recipient_tracks[
      !is.na(recipient_tracks) & nzchar(recipient_tracks)
    ]
    if (length(recipient_tracks) == 1L) {
      return(recipient_tracks[[1L]])
    }
    if (length(recipient_tracks) > 1L) {
      cli::cli_abort(c(
        "The {endpoint} link endpoint {.val {value}} matches multiple annotation panels.",
        "i" = "Use one annotation track alias as the link endpoint: {paste(recipient_tracks, collapse = ', ')}."
      ))
    }
    value
  }, character(1))
}

.filter_stored_syn_layout <- function(layout,
                                      annotation_species = NULL,
                                      link_pairs = NULL,
                                      coverage_tracks = NULL) {
  layout_obj <- as_syn_layout(layout)
  panels <- syn_layout_panels(layout_obj)
  if (!is.data.frame(panels) || nrow(panels) == 0L) {
    return(layout_obj)
  }

  panels$panel_type <- link_panel_type(panels)

  annotation_species <- unique(as.character(annotation_species %||% character()))
  annotation_species <- annotation_species[!is.na(annotation_species) & nzchar(annotation_species)]
  coverage_tracks <- unique(as.character(coverage_tracks %||% character()))
  coverage_tracks <- coverage_tracks[!is.na(coverage_tracks) & nzchar(coverage_tracks)]

  link_pairs <- link_pairs %||% data.frame()
  selected_species <- unique(c(
    annotation_species,
    as.character(link_pairs$tspecies %||% character()),
    as.character(link_pairs$qspecies %||% character())
  ))
  selected_species <- selected_species[!is.na(selected_species) & nzchar(selected_species)]

  if (length(selected_species) == 0L && length(coverage_tracks) == 0L) {
    return(SynLayout(
      panels = panels,
      layout_type = layout_obj@layout_type,
      free = layout_obj@free,
      exon_height = layout_obj@exon_height,
      x_translation = layout_obj@x_translation,
      metadata = layout_obj@metadata
    ))
  }

  panel_species <- if ("species" %in% names(panels)) {
    as.character(panels$species)
  } else {
    as.character(panels$track)
  }
  panel_type <- as.character(panels$panel_type)

  keep_annotation <- panel_type == "annotation" &
    !is.na(panel_species) &
    panel_species %in% selected_species
  keep_coverage <- panel_type == "coverage" &
    "track" %in% names(panels) &
    as.character(panels$track) %in% coverage_tracks

  link_tracks <- unique(as.character(link_pairs$track %||% character()))
  link_tracks <- link_tracks[!is.na(link_tracks) & nzchar(link_tracks)]
  keep_link <- if (length(link_tracks) > 0L) {
    panel_type == "link" &
      ("track" %in% names(panels)) &
      (as.character(panels$track) %in% link_tracks)
  } else {
    panel_type == "link" &
      ("tspecies" %in% names(panels)) &
      ("qspecies" %in% names(panels)) &
      as.character(panels$tspecies) %in% selected_species &
      as.character(panels$qspecies) %in% selected_species
  }

  known_roles <- c("annotation", "coverage", "link")
  keep_extension <- !panel_type %in% known_roles
  keep_extension[is.na(keep_extension)] <- TRUE

  keep <- keep_annotation | keep_coverage | keep_link | keep_extension
  filtered <- panels[keep, , drop = FALSE]
  if (nrow(filtered) == 0L) {
    return(SynLayout(
      panels = panels,
      layout_type = layout_obj@layout_type,
      free = layout_obj@free,
      exon_height = layout_obj@exon_height,
      x_translation = layout_obj@x_translation,
      metadata = layout_obj@metadata
    ))
  }

  missing_link_tracks <- setdiff(link_tracks, as.character(filtered$track %||% character()))
  if (length(missing_link_tracks) > 0L) {
    missing_pairs <- link_pairs[as.character(link_pairs$track) %in% missing_link_tracks, , drop = FALSE]
    start_panel <- max(as.integer(filtered$PANEL %||% seq_len(nrow(filtered))), na.rm = TRUE)
    start_row <- max(as.integer(filtered$ROW %||% seq_len(nrow(filtered))), na.rm = TRUE)
    if (!is.finite(start_panel)) start_panel <- nrow(filtered)
    if (!is.finite(start_row)) start_row <- nrow(filtered)
    extra_links <- data.frame(
      PANEL = start_panel + seq_len(nrow(missing_pairs)),
      ROW = start_row + seq_len(nrow(missing_pairs)),
      COL = 1L,
      track = as.character(missing_pairs$track),
      panel_type = "link",
      species = NA_character_,
      alignment_name = sub("^link_", "", as.character(missing_pairs$track)),
      tspecies = as.character(missing_pairs$tspecies),
      qspecies = as.character(missing_pairs$qspecies),
      stringsAsFactors = FALSE
    )
    missing_cols <- setdiff(names(filtered), names(extra_links))
    for (col in missing_cols) {
      extra_links[[col]] <- NA
    }
    missing_cols <- setdiff(names(extra_links), names(filtered))
    for (col in missing_cols) {
      filtered[[col]] <- NA
    }
    extra_links <- extra_links[, names(filtered), drop = FALSE]
    filtered <- dplyr::bind_rows(filtered, extra_links)
  }

  rownames(filtered) <- NULL
  SynLayout(
    panels = filtered,
    layout_type = layout_obj@layout_type,
    free = layout_obj@free,
    exon_height = layout_obj@exon_height,
    x_translation = layout_obj@x_translation,
    metadata = layout_obj@metadata
  )
}

.synspecies_chain_layout_from_layers <- function(x,
                                                 annotation_species,
                                                 link_pairs,
                                                 free,
                                                 allow_annotation_only = FALSE,
                                                 panel_scale_specs = list()) {
  annotation_species <- unique(as.character(annotation_species %||% character()))
  if (!is.null(link_pairs) && nrow(link_pairs) > 0L) {
    t_track <- as.character(link_pairs$t_track %||% link_pairs$tspecies)
    q_track <- as.character(link_pairs$q_track %||% link_pairs$qspecies)
    annotation_species <- unique(c(
      annotation_species,
      t_track,
      q_track
    ))
  }
  annotation_species <- annotation_species[!is.na(annotation_species) & nzchar(annotation_species)]
  if (length(annotation_species) == 0L ||
      (length(annotation_species) < 2L && !isTRUE(allow_annotation_only))) {
    return(NULL)
  }

  if (is.null(link_pairs) || nrow(link_pairs) == 0L) {
    if (!isTRUE(allow_annotation_only)) {
      return(NULL)
    }
    link_pairs <- data.frame(
      track = character(),
      tspecies = character(),
      qspecies = character(),
      stringsAsFactors = FALSE
    )
  } else {
    pair_columns <- intersect(
      c("track", "tspecies", "qspecies", "t_track", "q_track"),
      names(link_pairs)
    )
    link_pairs <- unique(link_pairs[, pair_columns, drop = FALSE])
  }
  link_t_track <- as.character(link_pairs$t_track %||% link_pairs$tspecies)
  link_q_track <- as.character(link_pairs$q_track %||% link_pairs$qspecies)
  panel_rows <- vector("list", length(annotation_species) + nrow(link_pairs))
  panel_index <- 1L
  used_links <- rep(FALSE, nrow(link_pairs))

  for (i in seq_along(annotation_species)) {
    species_name <- annotation_species[[i]]
    panel_rows[[panel_index]] <- data.frame(
      PANEL = panel_index,
      ROW = panel_index,
      COL = 1L,
      track = species_name,
      panel_type = "annotation",
      species = species_name,
      alignment_name = NA_character_,
      tspecies = NA_character_,
      qspecies = NA_character_,
      stringsAsFactors = FALSE
    )
    panel_index <- panel_index + 1L

    if (i >= length(annotation_species)) {
      next
    }

    later_species <- annotation_species[seq.int(i + 1L, length(annotation_species))]
    pair_match <- vapply(seq_len(nrow(link_pairs)), function(j) {
      if (isTRUE(used_links[[j]])) return(FALSE)
      pair_species <- c(link_t_track[[j]], link_q_track[[j]])
      species_name %in% pair_species && any(later_species %in% pair_species)
    }, logical(1))
    matched_pairs <- link_pairs[pair_match, , drop = FALSE]
    if (nrow(matched_pairs) == 0L) {
      next
    }

    for (j in which(pair_match)) {
      pair_track <- link_pairs$track[[j]]
      pair_name <- sub("^link_", "", pair_track)
      pair_obj <- pairwise_alignments(x)[[pair_name]]
      panel_rows[[panel_index]] <- data.frame(
        PANEL = panel_index,
        ROW = panel_index,
        COL = 1L,
        track = pair_track,
        panel_type = "link",
        species = NA_character_,
        alignment_name = pair_name,
        tspecies = if (is.null(pair_obj)) link_pairs$tspecies[[j]] else target_individual(pair_obj),
        qspecies = if (is.null(pair_obj)) link_pairs$qspecies[[j]] else query_individual(pair_obj),
        stringsAsFactors = FALSE
      )
      used_links[[j]] <- TRUE
      panel_index <- panel_index + 1L
    }
  }

  remaining_links <- which(!used_links)
  for (j in remaining_links) {
    pair_track <- link_pairs$track[[j]]
    pair_name <- sub("^link_", "", pair_track)
    pair_obj <- pairwise_alignments(x)[[pair_name]]
    panel_rows[[panel_index]] <- data.frame(
      PANEL = panel_index,
      ROW = panel_index,
      COL = 1L,
      track = pair_track,
      panel_type = "link",
      species = NA_character_,
      alignment_name = pair_name,
      tspecies = if (is.null(pair_obj)) link_pairs$tspecies[[j]] else target_individual(pair_obj),
      qspecies = if (is.null(pair_obj)) link_pairs$qspecies[[j]] else query_individual(pair_obj),
      stringsAsFactors = FALSE
    )
    panel_index <- panel_index + 1L
  }

  panels <- dplyr::bind_rows(panel_rows[seq_len(panel_index - 1L)])

  .finalize_synspecies_layout_scales(
    panels,
    free = free,
    layout_type = "chain",
    panel_scale_specs = panel_scale_specs
  )
}

.coverage_x_source_candidates <- function(layout, coverage_row, annotation_rows) {
  track <- as.character(layout$track[[coverage_row]])
  same_track <- annotation_rows[
    as.character(layout$track[annotation_rows]) == track
  ]
  if (length(same_track) > 0L) {
    return(same_track)
  }

  coverage_columns <- intersect(c("individual", "species"), names(layout))
  annotation_columns <- intersect(
    c("individual", "species", "track"),
    names(layout)
  )
  recipients <- unique(unlist(lapply(coverage_columns, function(column) {
    as.character(layout[[column]][[coverage_row]])
  }), use.names = FALSE))
  recipients <- recipients[!is.na(recipients) & nzchar(recipients)]
  if (length(recipients) == 0L) {
    return(integer())
  }

  matched <- rep(FALSE, length(annotation_rows))
  for (column in annotation_columns) {
    matched <- matched |
      as.character(layout[[column]][annotation_rows]) %in% recipients
  }
  annotation_rows[matched]
}

.collapse_equivalent_coverage_x_sources <- function(layout, candidates) {
  if (length(candidates) <= 1L ||
      !all(c("xlim_chr", "xlim_min", "xlim_max") %in% names(layout))) {
    return(candidates)
  }
  windows <- data.frame(
    chr = as.character(layout$xlim_chr[candidates]),
    start = suppressWarnings(as.numeric(layout$xlim_min[candidates])),
    end = suppressWarnings(as.numeric(layout$xlim_max[candidates])),
    stringsAsFactors = FALSE
  )
  finite_coordinates <- all(
    is.finite(windows$start) & is.finite(windows$end)
  )
  has_chr <- !is.na(windows$chr) & nzchar(windows$chr)
  compatible_chr <- all(!has_chr) ||
    (all(has_chr) && length(unique(windows$chr)) == 1L)
  equivalent_coordinates <- length(unique(windows$start)) == 1L &&
    length(unique(windows$end)) == 1L
  if (finite_coordinates && compatible_chr && equivalent_coordinates) {
    return(candidates[[1L]])
  }
  candidates
}

.annotate_coverage_x_source_panels <- function(layout) {
  layout <- as.data.frame(layout, stringsAsFactors = FALSE)
  roles <- link_panel_type(layout)
  coverage_rows <- which(roles == "coverage")
  if (length(coverage_rows) == 0L) {
    return(layout)
  }

  if (!"x_source_panel" %in% names(layout)) {
    layout$x_source_panel <- NA_integer_
  }
  for (column in c("xlim_chr", "xlim_min", "xlim_max")) {
    if (!column %in% names(layout)) {
      layout[[column]] <- if (identical(column, "xlim_chr")) {
        NA_character_
      } else {
        NA_real_
      }
    }
  }
  annotation_rows <- which(roles == "annotation")
  if (length(annotation_rows) == 0L) {
    layout$x_source_panel[coverage_rows] <- NA_integer_
    return(layout)
  }

  for (coverage_row in coverage_rows) {
    candidates <- .coverage_x_source_candidates(
      layout,
      coverage_row,
      annotation_rows
    )
    if (length(candidates) == 0L && length(annotation_rows) == 1L) {
      candidates <- annotation_rows
    }
    candidates <- .collapse_equivalent_coverage_x_sources(
      layout,
      candidates
    )
    track <- as.character(layout$track[[coverage_row]])
    if (length(candidates) != 1L) {
      candidate_panels <- as.integer(layout$PANEL[candidates])
      candidate_text <- if (length(candidate_panels) == 0L) {
        "none"
      } else {
        paste(candidate_panels, collapse = ", ")
      }
      cli::cli_abort(c(
        "Cannot resolve one annotation x source for coverage track {.val {track}}.",
        "i" = "Candidate annotation panels: {candidate_text}."
      ))
    }

    source_row <- candidates[[1L]]
    layout$x_source_panel[[coverage_row]] <- as.integer(
      layout$PANEL[[source_row]]
    )
    coverage_chr <- as.character(layout$xlim_chr[[coverage_row]])
    coverage_start <- suppressWarnings(as.numeric(
      layout$xlim_min[[coverage_row]]
    ))
    coverage_end <- suppressWarnings(as.numeric(
      layout$xlim_max[[coverage_row]]
    ))
    source_chr <- as.character(layout$xlim_chr[[source_row]])
    source_start <- suppressWarnings(as.numeric(layout$xlim_min[[source_row]]))
    source_end <- suppressWarnings(as.numeric(layout$xlim_max[[source_row]]))
    coverage_complete <- is.finite(coverage_start) &&
      is.finite(coverage_end) &&
      (is.na(coverage_chr) || nzchar(coverage_chr))
    source_complete <- is.finite(source_start) && is.finite(source_end) &&
      (is.na(source_chr) || nzchar(source_chr))
    same_complete_window <- coverage_complete && source_complete &&
      identical(coverage_chr, source_chr) &&
      identical(coverage_start, source_start) &&
      identical(coverage_end, source_end)
    inherit_source_window <- !coverage_complete
    coverage_window_explicit <- if (
      ".ggexon_coverage_window_explicit" %in% names(layout) &&
        !is.na(layout$.ggexon_coverage_window_explicit[[coverage_row]])
    ) {
      isTRUE(layout$.ggexon_coverage_window_explicit[[coverage_row]])
    } else {
      coverage_complete
    }
    share_source_scale <- inherit_source_window ||
      (!source_complete && !coverage_window_explicit) ||
      same_complete_window

    if ("SCALE_X" %in% names(layout) &&
        share_source_scale) {
      layout$SCALE_X[[coverage_row]] <- layout$SCALE_X[[source_row]]
    }
    if (inherit_source_window) {
      for (column in c("xlim_chr", "xlim_min", "xlim_max")) {
        source_value <- layout[[column]][[source_row]]
        if (!is.na(source_value)) {
          layout[[column]][[coverage_row]] <- source_value
        }
      }
    }
  }

  if ("SCALE_X" %in% names(layout)) {
    layout$SCALE_X <- match(layout$SCALE_X, unique(layout$SCALE_X))
  }
  layout$x_source_panel <- as.integer(layout$x_source_panel)
  layout
}

.finalize_synspecies_layout_scales <- function(layout,
                                               free,
                                               layout_type = NULL,
                                               panel_scale_specs = list()) {
  layout_obj <- as_syn_layout(layout, layout_type = layout_type, free = free)
  layout <- syn_layout_panels(layout_obj)
  had_panel_type <- "panel_type" %in% names(layout)
  layout <- .normalize_synspecies_layout_order(layout)
  rownames(layout) <- NULL

  layout$SCALE_X <- if (isTRUE(free$x)) seq_len(nrow(layout)) else 1L
  layout <- .annotate_coverage_x_source_panels(layout)

  panel_roles <- link_panel_type(layout)
  policies <- .resolve_present_panel_y_policies(
    panel_roles,
    specs = panel_scale_specs,
    free = free
  )
  has_coverage <- any(panel_roles == "coverage")
  annotation_policy_changed <- !is.null(panel_scale_specs$annotation) &&
    !identical(panel_scale_specs$annotation$policy, "fixed_y")

  if (!has_coverage && !annotation_policy_changed) {
    # Preserve the exact coverage-free annotation/link allocation used by
    # existing pairwise layouts. Role-aware allocation is only required once
    # coverage is present or annotation explicitly opts into per-panel y
    # scales.
    if (isTRUE(free$y)) {
      if (had_panel_type) {
        layout$SCALE_Y <- ifelse(panel_roles == "link", 2L, 1L)
      } else {
        layout$SCALE_Y <- seq_len(nrow(layout))
      }
    } else {
      layout$SCALE_Y <- 1L
    }
  } else {
    groups <- vapply(seq_len(nrow(layout)), function(i) {
      role <- panel_roles[[i]]
      policy <- policies[[role]] %||% .facet_y_policy(free)

      if (role %in% c("annotation", "coverage")) {
        if (identical(policy, "fixed_y")) {
          return(paste0(role, ":shared"))
        }
        return(paste0(role, ":", layout$PANEL[[i]]))
      }
      if (identical(role, "link")) {
        return("link:shared")
      }
      if (identical(policy, "free_y")) {
        paste0("role:", role, ":", layout$PANEL[[i]])
      } else {
        paste0("role:", role, ":shared")
      }
    }, character(1))
    layout$SCALE_Y <- match(groups, unique(groups))
  }

  layout <- .annotate_synspecies_link_source_panels(layout)
  metadata <- layout_obj@metadata
  metadata$panel_role_y_policies <- policies
  resolved_free <- free
  resolved_free$y <- any(vapply(
    policies,
    identical,
    logical(1),
    "free_y"
  ))
  SynLayout(
    panels = layout,
    layout_type = layout_type %||% layout_obj@layout_type,
    free = resolved_free,
    exon_height = layout_obj@exon_height,
    x_translation = layout_obj@x_translation,
    metadata = metadata
  )
}

.syn_layer_panel_role <- function(data) {
  role <- attr(data, "ggexon_panel_role", exact = TRUE)
  if (is.null(role) && ".ggexon_panel_role" %in% names(data)) {
    role <- unique(stats::na.omit(as.character(data$.ggexon_panel_role)))
  }
  if (length(role) == 0L) "annotation" else as.character(role[[1L]])
}

.syn_role_panel_key <- function(panel_type, track) {
  paste(panel_type, track, sep = "\r")
}

.is_link_like_df <- function(df) {
  is.data.frame(df) &&
    all(c("track", "tspecies", "qspecies", "tstart", "qstart") %in% names(df))
}

.is_annotation_like_df <- function(df) {
  is.data.frame(df) &&
    "track" %in% names(df) &&
    !.is_link_like_df(df) &&
    any(c("start", "xmin", "xstart") %in% names(df))
}

.annotate_synspecies_link_source_panels <- function(layout) {
  layout <- as.data.frame(layout, stringsAsFactors = FALSE)

  if (!"t_panel" %in% names(layout)) {
    layout$t_panel <- NA_integer_
  }
  if (!"q_panel" %in% names(layout)) {
    layout$q_panel <- NA_integer_
  }

  if (!"panel_type" %in% names(layout) || !"species" %in% names(layout)) {
    return(layout)
  }

  annotation_rows <- layout[
    layout$panel_type == "annotation" & !is.na(layout$species),
    c("species", "PANEL"),
    drop = FALSE
  ]

  if (nrow(annotation_rows) == 0L) {
    return(layout)
  }

  annotation_rows <- annotation_rows[!duplicated(annotation_rows$species), , drop = FALSE]

  if ("tspecies" %in% names(layout)) {
    matched_t <- match(layout$tspecies, annotation_rows$species)
    layout$t_panel <- ifelse(
      !is.na(matched_t),
      annotation_rows$PANEL[matched_t],
      layout$t_panel
    )
  }

  if ("qspecies" %in% names(layout)) {
    matched_q <- match(layout$qspecies, annotation_rows$species)
    layout$q_panel <- ifelse(
      !is.na(matched_q),
      annotation_rows$PANEL[matched_q],
      layout$q_panel
    )
  }

  layout$t_panel <- as.integer(layout$t_panel)
  layout$q_panel <- as.integer(layout$q_panel)
  layout
}

.annotate_link_source_panels <- function(layout) {
  layout <- as.data.frame(layout, stringsAsFactors = FALSE)

  if (!"t_panel" %in% names(layout)) {
    layout$t_panel <- NA_integer_
  }
  if (!"q_panel" %in% names(layout)) {
    layout$q_panel <- NA_integer_
  }

  required_cols <- c("track", "tspecies", "qspecies")
  if (!all(required_cols %in% names(layout))) {
    layout$t_panel <- as.integer(layout$t_panel)
    layout$q_panel <- as.integer(layout$q_panel)
    return(layout)
  }

  annotation_rows <- layout[
    !is.na(layout$track) &
      !stringr::str_detect(layout$track, "link") &
      !duplicated(layout$track),
    c("track", "PANEL"),
    drop = FALSE
  ]

  if (nrow(annotation_rows) == 0L) {
    layout$t_panel <- as.integer(layout$t_panel)
    layout$q_panel <- as.integer(layout$q_panel)
    return(layout)
  }

  matched_t <- match(layout$tspecies, annotation_rows$track)
  layout$t_panel <- ifelse(
    !is.na(matched_t),
    annotation_rows$PANEL[matched_t],
    layout$t_panel
  )

  matched_q <- match(layout$qspecies, annotation_rows$track)
  layout$q_panel <- ifelse(
    !is.na(matched_q),
    annotation_rows$PANEL[matched_q],
    layout$q_panel
  )

  layout$t_panel <- as.integer(layout$t_panel)
  layout$q_panel <- as.integer(layout$q_panel)
  layout
}

.normalize_synspecies_layout_order <- function(layout) {
  layout <- as.data.frame(layout, stringsAsFactors = FALSE)
  if (nrow(layout) == 0L) {
    return(layout)
  }

  if ("track" %in% names(layout) && is.factor(layout$track)) {
    layout <- layout[order(layout$track), , drop = FALSE]
  }

  rownames(layout) <- NULL
  old_panel <- if (is.factor(layout$PANEL)) {
    suppressWarnings(as.integer(as.character(layout$PANEL)))
  } else {
    suppressWarnings(as.integer(layout$PANEL))
  }
  new_panel <- seq_len(nrow(layout))
  annotation_rows <- which(link_panel_type(layout) == "annotation")
  old_annotation_panel <- old_panel[annotation_rows]
  new_annotation_panel <- new_panel[annotation_rows]
  for (source_column in intersect(c("t_panel", "q_panel"), names(layout))) {
    source_panel <- if (is.factor(layout[[source_column]])) {
      suppressWarnings(as.integer(as.character(layout[[source_column]])))
    } else {
      suppressWarnings(as.integer(layout[[source_column]]))
    }
    source_match <- match(source_panel, old_annotation_panel)
    remapped <- rep(NA_integer_, length(source_panel))
    resolved <- !is.na(source_match)
    remapped[resolved] <- new_annotation_panel[source_match[resolved]]
    layout[[source_column]] <- remapped
  }

  complete_multi_column_grid <- all(c("ROW", "COL") %in% names(layout)) &&
    !anyNA(layout$ROW) && !anyNA(layout$COL) &&
    length(unique(layout$COL)) > 1L
  if (!complete_multi_column_grid) {
    layout$ROW <- seq_len(nrow(layout))
  }
  layout$PANEL <- new_panel
  layout
}

synspecies_chain_species_order <- function(x) {
  species_names <- names(individuals(x))
  pair_list <- pairwise_alignments(x)

  if (length(pair_list) == 0L) {
    return(species_names)
  }

  pair_df <- data.frame(
    left = vapply(pair_list, target_individual, character(1)),
    right = vapply(pair_list, query_individual, character(1)),
    stringsAsFactors = FALSE
  )

  pair_nodes <- unique(c(pair_df$left, pair_df$right))
  adjacency <- stats::setNames(vector("list", length(pair_nodes)), pair_nodes)
  for (i in seq_len(nrow(pair_df))) {
    left <- pair_df$left[[i]]
    right <- pair_df$right[[i]]
    adjacency[[left]] <- unique(c(adjacency[[left]], right))
    adjacency[[right]] <- unique(c(adjacency[[right]], left))
  }

  degree <- vapply(adjacency, length, integer(1))
  if (any(degree > 2L)) {
    cli::cli_abort(
      "Current chain layout supports only path-like pairwise alignments; species {.val {names(degree)[degree > 2L]}} participate in more than two alignments."
    )
  }

  endpoints <- names(degree)[degree <= 1L]
  preferred_start <- species_names[species_names %in% endpoints]
  if (length(preferred_start) > 0L) {
    start <- preferred_start[[1L]]
  } else {
    start <- species_names[species_names %in% pair_nodes][[1L]]
  }

  ordered <- character()
  previous <- NA_character_
  current <- start
  while (!is.na(current) && nzchar(current) && !current %in% ordered) {
    ordered <- c(ordered, current)
    next_nodes <- setdiff(adjacency[[current]], previous)
    next_nodes <- next_nodes[!next_nodes %in% ordered]
    previous <- current
    current <- if (length(next_nodes) > 0L) next_nodes[[1L]] else NA_character_
  }

  if (!setequal(ordered, pair_nodes)) {
    missing_nodes <- setdiff(pair_nodes, ordered)
    cli::cli_abort(
      "Current chain layout needs pairwise alignments to form one connected chain. Missing species from the walk: {.val {missing_nodes}}."
    )
  }

  c(ordered, setdiff(species_names, ordered))
}

synspecies_chain_alignment_rows <- function(x, species_order) {
  pair_list <- pairwise_alignments(x)
  if (length(pair_list) == 0L) {
    return(data.frame())
  }

  pair_df <- data.frame(
    alignment_name = names(pair_list),
    tspecies = vapply(pair_list, target_individual, character(1)),
    qspecies = vapply(pair_list, query_individual, character(1)),
    stringsAsFactors = FALSE
  )

  species_pos <- stats::setNames(seq_along(species_order), species_order)
  pair_df$left_species <- ifelse(
    species_pos[pair_df$tspecies] < species_pos[pair_df$qspecies],
    pair_df$tspecies,
    pair_df$qspecies
  )
  pair_df$right_species <- ifelse(
    species_pos[pair_df$tspecies] < species_pos[pair_df$qspecies],
    pair_df$qspecies,
    pair_df$tspecies
  )
  pair_df$track <- paste0("link_", pair_df$alignment_name)

  gap <- species_pos[pair_df$right_species] - species_pos[pair_df$left_species]
  if (any(is.na(gap))) {
    cli::cli_abort("Every pairwise alignment must reference individuals present in the {.cls SynSpecies} object.")
  }
  if (any(gap != 1L)) {
    bad <- pair_df$alignment_name[gap != 1L]
    cli::cli_abort(
      "Current chain layout requires each pairwise alignment to connect adjacent species in the chain. Offending alignments: {.val {bad}}."
    )
  }

  pair_df
}
