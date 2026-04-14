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

#' @export
FacetGenomics <- ggproto("FacetGenomics", FacetWrap,


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



  compute_layout = function(self, data, params) {
    vars <- params$facets

    if (methods::is(params$plot_data, "SynSpecies")) {
      if (!is.null(params$layout_override)) {
        return(
          .finalize_synspecies_layout_scales(
            params$layout_override,
            free = params$free
          )
        )
      }
      stored_layout <- species_layout(params$plot_data)
      if (!is.null(stored_layout)) {
        return(
          .finalize_synspecies_layout_scales(
            stored_layout,
            free = params$free
          )
        )
      }
      return(
        synspecies_chain_layout(
          x = params$plot_data,
          vars = vars,
          free = params$free
        )
      )
    }

    if (length(vars) == 0) {
      return(layout_null())
    }

    ggplot2:::check_facet_vars(names(vars), name = snake_class(self))

    base <- ggplot2:::combine_vars(data, params$plot_env, vars, drop = params$drop)

    id <- ggplot2:::id(base, drop = TRUE)
    n <- attr(id, "n")


    dims <- ggplot2:::wrap_dims(n, params$nrow, params$ncol)
    layout <- ggplot2:::wrap_layout(id, dims, params$dir)

    panels <- vec_cbind(layout, base)
    panels <- panels[order(panels$PANEL), , drop = FALSE]
    rownames(panels) <- NULL

    # Add scale identification
    panels$SCALE_X <- if (params$free$x) seq_len(n) else 1L
    panels$SCALE_Y <- if (params$free$y) seq_len(n) else 1L

    panels
  },

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

    # sort the panel order.
    split_by_col = split(layout2, layout2$COL)

    split_by_col = lapply(split_by_col, function(df){

    link_layout = df[str_detect(df$track, "link"), c("tspecies", "qspecies")]

    for (i in 1:nrow(link_layout)){

      target_row_index = rownames(df[df$track == link_layout[i,"tspecies"], ])
      query_row_index = rownames(df[df$track == link_layout[i,"qspecies"], ])
      annotation_index = sort(c(target_row_index, query_row_index))
      link_index_row = rownames(link_layout[i , ])

      if (link_index_row < annotation_index[1] || link_index_row > annotation_index[2]) {
        new_row_index = c(annotation_index[1], link_index_row, annotation_index[2])
      }
    }

    # siuation 1: all species have a link panel
    if (unique(sort(new_row_index) == rownames(df))){
      df = df[new_row_index, ]
    }else{
    # siuation 2: there are some species no link table
      df = df[c(new_row_index, setdiff(rownames(df), new_row_index)), ]
    }
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

    #print(link_layout)
    link_layout

  },

  map_link_direction = function(self, data, layout){

    link_layout = layout[str_detect(layout$track, "link"), ]


    link_y_list = list()
    for (i in 1:nrow(link_layout)) {
      link_index = as.numeric(rownames(link_layout[i,]))


      tspecies = link_layout[i,"tspecies"]
      qspecies = link_layout[i, "qspecies"]
      uppper_panel_species = layout[link_index - 1, "track"]



      if (uppper_panel_species == tspecies){
        ty = 1
        qy = 0
      }else{
        ty = 0
        qy = 1
      }
      link_y_list = append(link_y_list, c("link" = link_layout[i,"track"], "ty" = ty, "qy" = qy))

    }
    link_y_table = bind_rows(link_y_list)

    data = lapply(data, function(df){
      if (is_waiver(df)){ df }

      #print(colnames(df))
      #detect the link datatable
      if (sum(grepl("start", colnames(df), ignore.case = TRUE)) == 2){
        #print(df)
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

synspecies_chain_layout <- function(x, vars, free) {
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
  .finalize_synspecies_layout_scales(panels, free = free)
}

.finalize_synspecies_layout_scales <- function(layout, free) {
  layout <- as.data.frame(layout, stringsAsFactors = FALSE)
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
  layout
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
