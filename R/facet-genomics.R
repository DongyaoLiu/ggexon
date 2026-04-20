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
#'
#' @return A `FacetGenomics` ggproto object.
#'
#' @section SynSpecies behavior:
#' In Syn-aware builds, `facet_genomics()` is responsible for deciding the panel
#' structure used by `Layout2`. The returned panel table may include:
#'
#' - annotation panels for each species track
#' - link panels inserted between paired species tracks
#' - `panel_type`, `tspecies`, `qspecies`, `t_panel`, and `q_panel` metadata
#'   used later by `geom_nuclink()`
#'
#' If no Syn-specific layout is available, the facet falls back to ordinary
#' wrap-style panel generation.
#'
#' @seealso [SynLayout]
#' @export
facet_genomics <- function(facets, nrow = NULL, ncol = NULL, scales = "fixed",
                       shrink = TRUE, labeller = "label_value", as.table = TRUE,
                       switch = deprecated(), drop = TRUE, dir = "h",
                       strip.position = 'top', axes = "margins",
                       axis.labels = "all") {
  scales <- arg_match0(scales %||% "fixed", c("fixed", "free_x", "free_y", "free"))
  dir <- arg_match0(dir, c("h", "v", "lt", "tl", "lb", "bl", "rt", "tr", "rb", "br"))

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
  draw_axes <- arg_match0(axes, c("margins", "all_x", "all_y", "all"))
  draw_axes <- list(
    x = free$x || any(draw_axes %in% c("all_x", "all")),
    y = free$y || any(draw_axes %in% c("all_y", "all"))
  )

  # Omitting labels is special-cased internally, so only omit labels if
  # scales are not free and the axis is to be drawn
  axis_labels <- arg_match0(axis.labels, c("margins", "all_x", "all_y", "all"))
  axis_labels <- list(
    x = free$x || !draw_axes$x || any(axis_labels %in% c("all_x", "all")),
    y = free$y || !draw_axes$y || any(axis_labels %in% c("all_y", "all"))
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
      axis_labels = axis_labels
    )
  )
}

#' ggproto backend for `facet_genomics()`
#'
#' `FacetGenomics` extends ggplot2's `FacetWrap` with Syn-aware layout logic.
#' The main custom responsibilities are:
#'
#' - deciding whether to use a stored `SynLayout`, derive a new comparative
#'   chain layout, or fall back to standard wrap-style faceting
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
#'   when link layers are present, then a derived chain layout, and finally a
#'   standard wrap layout.}
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

    # Count occurrences of "start" in column names
    start_count <- sum(grepl("start", colnames(df), ignore.case = TRUE))

    # Determine output based on count of start
    # one start is obvious annotation file
    # two starts (e.g. qstart, tstart) should be the link file.
    if (start_count == 1) {
    return("annotation")
    } else if (start_count == 2) {
    return("link")
    } else {
    cli::cli_abort(c("can not detect start(case ignore) in the dataframe colnames"))
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
      if (!is.null(params$layout_override)) {
        # Highest priority: use an explicit layout override attached during
        # build, e.g. from species_layout(sp) or a layer-provided override.
        return(
          syn_layout_panels(
            .finalize_synspecies_layout_scales(
              params$layout_override,
              free = params$free
            )
          )
        )
      }

      if (isTRUE(params$has_link_layers)) {
        # If link layers are present, prefer the stored Syn layout so
        # annotation panels and link panels stay in the intended chain order.
        stored_layout <- species_layout(params$plot_data)
        if (!is.null(stored_layout)) {
          return(
            syn_layout_panels(
              .finalize_synspecies_layout_scales(
                stored_layout,
                free = params$free
              )
            )
          )
        }
      }

      # Otherwise derive a SynSpecies layout from the layers participating in
      # this plot (annotation species and requested link pairs).
      plot_layout <- synspecies_chain_layout(
        x = params$plot_data,
        vars = vars,
        free = params$free,
        annotation_species = .annotation_species_from_layers(data),
        link_pairs = .link_pairs_from_layers(data)
      )
      if (!is.null(plot_layout)) {
        return(syn_layout_panels(plot_layout))
      }

      return(.compute_standard_genomics_layout(data, params, self))
    }

    if (length(vars) == 0) {
      return(layout_null())
    }
    .compute_standard_genomics_layout(data, params, self)
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

    link_layout = layout[stringr::str_detect(layout$track, "link"), ]


    link_y_list = list()
    for (i in 1:nrow(link_layout)) {
      link_index = match(link_layout$PANEL[i], layout$PANEL)


      tspecies = link_layout[i,"tspecies"]
      qspecies = link_layout[i, "qspecies"]
      uppper_panel_species = if (!is.na(link_index) && link_index > 1L) {
        layout[link_index - 1L, "track"]
      } else {
        NA_character_
      }



      if (!is.na(uppper_panel_species) && uppper_panel_species == tspecies){
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

      #print(colnames(df))
      #detect the link datatable
      if (sum(grepl("start", colnames(df), ignore.case = TRUE)) == 2){
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

.compute_standard_genomics_layout <- function(data, params, facet) {
  vars <- params$facets
  ggplot2:::check_facet_vars(names(vars), name = snake_class(facet))

  base <- ggplot2:::combine_vars(data, params$plot_env, vars, drop = params$drop)

  id <- ggplot2:::id(base, drop = TRUE)
  n <- attr(id, "n")

  dims <- ggplot2:::wrap_dims(n, params$nrow, params$ncol)
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

synspecies_chain_layout <- function(x,
                                    vars,
                                    free,
                                    annotation_species = NULL,
                                    link_pairs = NULL) {
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
      free = free
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
    layout_type = "chain"
  )
}

.annotation_species_from_layers <- function(data) {
  tracks <- unique(unlist(lapply(data, function(df) {
    if (!is.data.frame(df) || !"track" %in% names(df)) {
      return(character())
    }
    if (any(c("tspecies", "qspecies") %in% names(df))) {
      return(character())
    }
    as.character(df$track)
  })))
  tracks <- tracks[!is.na(tracks) & nzchar(tracks)]
  unique(tracks)
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

  dplyr::bind_rows(Filter(Negate(is.null), pair_rows))
}

.synspecies_chain_layout_from_layers <- function(x,
                                                 annotation_species,
                                                 link_pairs,
                                                 free) {
  annotation_species <- unique(as.character(annotation_species %||% character()))
  annotation_species <- annotation_species[annotation_species %in% names(individuals(x))]
  if (length(annotation_species) != 2L) {
    return(NULL)
  }

  if (is.null(link_pairs) || nrow(link_pairs) == 0L) {
    return(NULL)
  }

  link_pairs <- unique(link_pairs[, c("track", "tspecies", "qspecies"), drop = FALSE])
  pair_match <- vapply(seq_len(nrow(link_pairs)), function(i) {
    setequal(annotation_species, c(link_pairs$tspecies[[i]], link_pairs$qspecies[[i]]))
  }, logical(1))
  link_pairs <- link_pairs[pair_match, , drop = FALSE]
  if (nrow(link_pairs) != 1L) {
    return(NULL)
  }

  pair_track <- link_pairs$track[[1L]]
  pair_name <- sub("^link_", "", pair_track)
  pair_obj <- pairwise_alignments(x)[[pair_name]]
  if (is.null(pair_obj)) {
    top_species <- link_pairs$qspecies[[1L]]
    bottom_species <- link_pairs$tspecies[[1L]]
  } else {
    top_species <- query_individual(pair_obj)
    bottom_species <- target_individual(pair_obj)
  }
  if (!setequal(annotation_species, c(top_species, bottom_species))) {
    return(NULL)
  }

  panels <- data.frame(
    PANEL = c(1L, 2L, 3L),
    ROW = c(1L, 2L, 3L),
    COL = c(1L, 1L, 1L),
    track = c(top_species, pair_track, bottom_species),
    panel_type = c("annotation", "link", "annotation"),
    species = c(top_species, NA_character_, bottom_species),
    alignment_name = c(NA_character_, pair_name, NA_character_),
    tspecies = c(
      NA_character_,
      if (is.null(pair_obj)) link_pairs$tspecies[[1L]] else target_individual(pair_obj),
      NA_character_
    ),
    qspecies = c(
      NA_character_,
      if (is.null(pair_obj)) link_pairs$qspecies[[1L]] else query_individual(pair_obj),
      NA_character_
    ),
    stringsAsFactors = FALSE
  )

  .finalize_synspecies_layout_scales(
    panels,
    free = free,
    layout_type = "chain"
  )
}

.finalize_synspecies_layout_scales <- function(layout, free, layout_type = NULL) {
  layout_obj <- as_syn_layout(layout, layout_type = layout_type, free = free)
  layout <- syn_layout_panels(layout_obj)
  layout <- .normalize_synspecies_layout_order(layout)
  rownames(layout) <- NULL

  layout$SCALE_X <- if (isTRUE(free$x)) seq_len(nrow(layout)) else 1L

  if (isTRUE(free$y)) {
    if ("panel_type" %in% colnames(layout)) {
      layout$SCALE_Y <- ifelse(layout$panel_type == "link", 2L, 1L)
    } else {
      layout$SCALE_Y <- seq_len(nrow(layout))
    }
  } else {
    layout$SCALE_Y <- 1L
  }

  layout <- .annotate_synspecies_link_source_panels(layout)
  SynLayout(
    panels = layout,
    layout_type = layout_type %||% layout_obj@layout_type,
    free = free,
    exon_height = layout_obj@exon_height,
    y_scale = layout_obj@y_scale,
    x_translation = layout_obj@x_translation,
    metadata = layout_obj@metadata
  )
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
  layout$ROW <- seq_len(nrow(layout))
  layout$PANEL <- seq_len(nrow(layout))
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
