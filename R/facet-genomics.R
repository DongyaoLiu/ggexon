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
