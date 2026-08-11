LayerSyn <- ggproto(
  "LayerSyn",
  get("Layer", envir = asNamespace("ggplot2")),
  layer_data = function(self, plot_data) {
    data_source <- self$data
    if (inherits(data_source, "waiver")) {
      data_source <- plot_data
    } else if (is.function(data_source)) {
      data_source <- data_source(plot_data)
      if (!is.data.frame(data_source)) {
        cli::cli_abort("{.fn layer_data} must return a {.cls data.frame}.")
      }
      rownames(data_source) <- NULL
      return(data_source)
    }

    if (methods::is(data_source, "SynSpecies") || methods::is(data_source, "SynIndividual")) {
      data_source <- resolve_syn_layer_data(data_source, self)
    }

    if (is.null(data_source) || inherits(data_source, "waiver")) {
      return(data_source)
    }

    rownames(data_source) <- NULL
    data_source
  },
  setup_layer = function(self, data, plot) {
    syn_layout_override <- attr(data, "syn_layout_override", exact = TRUE)

    if (is_syn_layer_input(self, plot@data)) {
      defaults_fn <- get("defaults", envir = asNamespace("ggplot2"))
      self$mapping <- ggplot2::class_mapping(
        defaults_fn(self$mapping, syn_default_mapping(data, self))
      )
    }

    defaults_fn <- get("defaults", envir = asNamespace("ggplot2"))

    if (isTRUE(self$inherit.aes)) {
      self$computed_mapping <- ggplot2::class_mapping(defaults_fn(self$mapping, plot@mapping))
      if (self$geom$rename_size && "size" %in% names(plot@mapping) &&
          !"linewidth" %in% names(self$computed_mapping) &&
          "linewidth" %in% self$geom$aesthetics()) {
        self$computed_mapping$size <- plot@mapping$size
      }
    } else {
      self$computed_mapping <- self$mapping
    }

    attr(data, "layout") <- self$layout
    if (!is.null(syn_layout_override)) {
      attr(data, "syn_layout_override") <- syn_layout_override
    }
    data
  }
)

is_syn_layer_input <- function(layer, plot_data) {
  (inherits(layer$data, "waiver") &&
     (methods::is(plot_data, "SynSpecies") || methods::is(plot_data, "SynIndividual"))) ||
    methods::is(layer$data, "SynSpecies") ||
    methods::is(layer$data, "SynIndividual")
}

syn_default_mapping <- function(data, layer) {
  syn_identity_mapping(default_syn_aesthetics(data, layer))
}
default_syn_aesthetics <- function(data, layer) {
  cols <- layer$geom$syn_default_aes %||% character()
  intersect(cols, names(data))
}

syn_identity_mapping <- function(cols) {
  if (length(cols) == 0L) {
    return(ggplot2::aes())
  }

  exprs <- stats::setNames(lapply(cols, rlang::sym), cols)
  rlang::inject(ggplot2::aes(!!!exprs))
}

collect_syn_plot_context <- function(layers, plot_data, facet = NULL) {
  syn_data <- find_syn_plot_data(layers, plot_data)
  if (is.null(syn_data)) {
    return(NULL)
  }
  has_syn_dispatched_layer <- any(vapply(
    layers,
    is_syn_layer_input,
    logical(1),
    plot_data = plot_data
  ))
  if (!has_syn_dispatched_layer) {
    return(NULL)
  }

  annotation_requests <- unlist(lapply(layers, function(layer) {
    collect_syn_annotation_requests(layer, syn_data, plot_data)
  }), recursive = FALSE)
  link_requests <- unlist(lapply(layers, function(layer) {
    collect_syn_link_requests(layer, syn_data, plot_data)
  }), recursive = FALSE)
  annotation_species_order <- resolve_syn_plot_species_order(
    syn_data,
    annotation_requests = annotation_requests,
    link_requests = link_requests
  )

  layout_windows <- collect_layout_panel_windows(syn_data)
  layout_annotation_windows <- collect_layout_panel_windows(
    syn_data,
    panel_roles = "annotation"
  )
  facet_windows <- collect_facet_panel_windows(
    syn_data,
    facet = facet,
    annotation_species_order = annotation_species_order
  )
  explicit_annotation_windows <- collect_explicit_annotation_windows(
    annotation_requests,
    syn_data
  )
  windows <- utils::modifyList(layout_windows, facet_windows)
  windows <- utils::modifyList(windows, explicit_annotation_windows)
  annotation_windows <- utils::modifyList(
    layout_annotation_windows,
    facet_windows
  )
  annotation_windows <- utils::modifyList(
    annotation_windows,
    explicit_annotation_windows
  )
  windows <- derive_syn_plot_windows(
    syn_data,
    windows,
    link_requests,
    annotation_species_order = annotation_species_order
  )
  annotation_windows <- derive_syn_plot_windows(
    syn_data,
    annotation_windows,
    link_requests,
    annotation_species_order = annotation_species_order
  )
  window_names <- names(annotation_windows) %||%
    rep("", length(annotation_windows))
  coverage_annotation_windows <- annotation_windows[vapply(
    seq_along(annotation_windows),
    function(i) {
    window <- annotation_windows[[i]]
    recipient <- as.character(
      window$individual %||% window$species %||% window_names[[i]]
    )
    length(recipient) == 1L && recipient %in% annotation_species_order
  }, logical(1))]
  coverage_requests <- collect_syn_coverage_requests(
    layers = layers,
    syn_data = syn_data,
    plot_data = plot_data,
    windows = windows,
    annotation_windows = coverage_annotation_windows
  )
  coverage_tracks <- unique(unlist(lapply(coverage_requests, function(request) {
    request$tracks
  }), use.names = FALSE))
  coverage_tracks <- as.character(
    coverage_tracks[!is.na(coverage_tracks) & nzchar(coverage_tracks)]
  )
  for (request in coverage_requests) {
    for (track in names(request$windows)) {
      windows[[track]] <- request$windows[[track]]
    }
  }

  list(
    syn_data = syn_data,
    annotation_requests = annotation_requests,
    link_requests = link_requests,
    windows = windows,
    annotation_windows = annotation_windows,
    annotation_species_order = annotation_species_order,
    coverage_requests = coverage_requests,
    coverage_tracks = coverage_tracks,
    query_cache = new.env(parent = emptyenv())
  )
}

.layout_panel_recipients <- function(panels, syn_data) {
  recipients <- rep(NA_character_, nrow(panels))
  individual_ids <- names(individuals(syn_data))
  recipient_columns <- intersect(
    c("individual", "species", "track"),
    names(panels)
  )

  for (recipient_column in recipient_columns) {
    values <- as.character(panels[[recipient_column]])
    fill <- is.na(recipients) &
      !is.na(values) & nzchar(values) &
      values %in% individual_ids
    recipients[fill] <- values[fill]
  }

  recipients
}

.coverage_track_recipients <- function(syn_data) {
  if (methods::is(syn_data, "SynIndividual")) {
    id <- syn_id(syn_data)
    return(stats::setNames(id, id))
  }
  if (!methods::is(syn_data, "SynSpecies")) {
    return(stats::setNames(character(), character()))
  }

  individual_ids <- names(individuals(syn_data))
  layout <- species_layout(syn_data)
  if (!is.null(layout)) {
    panels <- syn_layout_panels(layout)
    if (is.data.frame(panels) && "track" %in% names(panels)) {
      panel_type <- if ("panel_type" %in% names(panels)) {
        as.character(panels$panel_type)
      } else {
        rep("annotation", nrow(panels))
      }
      panel_type[is.na(panel_type)] <- "annotation"
      recipient_rows <- panel_type %in% c("annotation", "coverage")
      tracks <- as.character(panels$track)
      recipients <- .layout_panel_recipients(panels, syn_data)
      keep <- recipient_rows &
        !is.na(tracks) & nzchar(tracks) &
        !is.na(recipients) & nzchar(recipients)
      tracks <- tracks[keep]
      recipients <- recipients[keep]
      panel_type <- panel_type[keep]
      preferred_recipient_rows <- unlist(lapply(
        unique(recipients),
        function(recipient) {
          hits <- which(recipients == recipient)
          coverage_hits <- hits[panel_type[hits] == "coverage"]
          if (length(coverage_hits) > 0L) coverage_hits else hits
        }
      ), use.names = FALSE)
      tracks <- tracks[preferred_recipient_rows]
      recipients <- recipients[preferred_recipient_rows]
      panel_type <- panel_type[preferred_recipient_rows]
      track_order <- unique(tracks)
      if (length(track_order) > 0L) {
        preferred_rows <- vapply(track_order, function(track) {
          hits <- which(tracks == track)
          coverage_hits <- hits[panel_type[hits] == "coverage"]
          if (length(coverage_hits) > 0L) {
            return(coverage_hits[[1L]])
          }
          hits[[1L]]
        }, integer(1))
        out <- stats::setNames(
          recipients[preferred_rows],
          track_order
        )
        missing_individuals <- individual_ids[
          !individual_ids %in% unname(out)
        ]
        return(c(
          out,
          stats::setNames(missing_individuals, missing_individuals)
        ))
      }
    }
  }

  stats::setNames(individual_ids, individual_ids)
}

.coverage_individual_is_eligible <- function(individual, annotation = NULL) {
  if (!is.null(annotation)) {
    return(!is.null(resolve_syn_bigwig_annotation(
      individual,
      annotation = annotation,
      allow_missing = TRUE
    )))
  }

  matches <- vapply(annotation_names(individual), function(annotation_name) {
    methods::is(
      get_annotation(individual, annotation_name),
      "SynBigWigAnnotation"
    )
  }, logical(1))
  match_count <- sum(matches)
  if (match_count > 1L) {
    resolve_syn_bigwig_annotation(individual)
  }

  identical(match_count, 1L)
}

.normalize_coverage_species_selector <- function(syn_data, species = NULL) {
  if (is.null(species)) {
    return(NULL)
  }
  if (methods::is(syn_data, "SynIndividual")) {
    return(syn_id(syn_data))
  }
  unique(as.character(species))
}

.coverage_window_is_complete <- function(window) {
  if (is.null(window)) {
    return(FALSE)
  }
  chr <- as.character(window$chr %||% character())
  start <- suppressWarnings(as.numeric(window$start %||% numeric()))
  end <- suppressWarnings(as.numeric(window$end %||% numeric()))
  length(chr) == 1L && !is.na(chr) && nzchar(chr) &&
    length(start) == 1L && is.finite(start) &&
    length(end) == 1L && is.finite(end)
}

.coverage_window_coordinates <- function(window) {
  list(
    chr = as.character(window$chr[[1L]]),
    start = as.numeric(window$start[[1L]]),
    end = as.numeric(window$end[[1L]])
  )
}

.coverage_window_key <- function(window) {
  coordinates <- .coverage_window_coordinates(window)
  paste0(
    nchar(coordinates$chr), ":", coordinates$chr, ":",
    sprintf("%.17g", coordinates$start), ":",
    sprintf("%.17g", coordinates$end)
  )
}

.complete_common_coverage_windows <- function(coverage_tracks,
                                               coverage_windows,
                                               annotation_windows) {
  coverage_tracks <- unique(as.character(coverage_tracks %||% character()))
  coverage_tracks <- coverage_tracks[
    !is.na(coverage_tracks) & nzchar(coverage_tracks)
  ]
  if (length(coverage_tracks) == 0L) {
    return(list())
  }

  coverage_windows <- coverage_windows %||% list()
  coverage_windows <- stats::setNames(lapply(coverage_tracks, function(track) {
    window <- coverage_windows[[track]] %||% list()
    if (is.data.frame(window)) {
      window <- as.list(window)
    }
    window
  }), coverage_tracks)
  resolved <- vapply(
    coverage_windows,
    .coverage_window_is_complete,
    logical(1)
  )
  if (all(resolved)) {
    return(coverage_windows)
  }

  candidates <- Filter(
    .coverage_window_is_complete,
    annotation_windows %||% list()
  )
  if (length(candidates) == 0L) {
    return(coverage_windows[resolved])
  }
  candidate_keys <- vapply(candidates, .coverage_window_key, character(1))
  candidates <- candidates[!duplicated(candidate_keys)]
  if (length(candidates) > 1L) {
    cli::cli_abort(c(
      "Cannot choose a common coverage window from multiple incompatible annotation windows.",
      "i" = "Add one explicit annotation window for coverage, use a stored layout window, or supply facet {.arg xlim}/{.arg xlim_chr}."
    ))
  }

  common <- candidates[[1L]]
  if (is.data.frame(common)) {
    common <- as.list(common)
  }
  common <- common[c("chr", "start", "end")]
  for (track in coverage_tracks[!resolved]) {
    recipient <- coverage_windows[[track]]
    individual <- as.character(
      recipient$individual %||% recipient$species %||% track
    )[[1L]]
    coverage_windows[[track]] <- c(
      common,
      list(track = track, individual = individual, species = individual)
    )
  }
  coverage_windows
}

collect_syn_coverage_requests <- function(layers,
                                          syn_data,
                                          plot_data,
                                          windows = list(),
                                          annotation_windows = windows) {
  layer_indices <- which(vapply(layers, function(layer) {
    identical(layer$geom, GeomCoverage) && is_syn_layer_input(layer, plot_data)
  }, logical(1)))
  if (length(layer_indices) == 0L) {
    return(list())
  }

  recipients <- .coverage_track_recipients(syn_data)
  lapply(seq_along(layer_indices), function(request_index) {
    layer_index <- layer_indices[[request_index]]
    params <- syn_layer_params(layers[[layer_index]])
    selector <- .normalize_coverage_species_selector(syn_data, params$species)
    omitted_species <- is.null(params$species)
    annotation <- params$annotation %||% NULL

    selected <- recipients
    if (!omitted_species) {
      selected_tracks <- unique(unlist(lapply(selector, function(value) {
        names(recipients)[
          names(recipients) == value | unname(recipients) == value
        ]
      }), use.names = FALSE))
      unmatched <- selector[
        !selector %in% names(recipients) &
          !selector %in% unname(recipients)
      ]
      selected <- c(
        recipients[selected_tracks],
        stats::setNames(unmatched, unmatched)
      )
    } else {
      eligible <- vapply(unname(recipients), function(individual_name) {
        individual <- if (methods::is(syn_data, "SynIndividual")) {
          syn_data
        } else {
          individuals(syn_data)[[individual_name]]
        }
        methods::is(individual, "SynIndividual") &&
          .coverage_individual_is_eligible(
            individual,
            annotation = annotation
          )
      }, logical(1))
      selected <- recipients[eligible]
    }

    tracks <- unname(names(selected))
    coverage_windows <- stats::setNames(lapply(seq_along(tracks), function(i) {
      track <- tracks[[i]]
      individual <- unname(selected[[i]])
      source_window <- windows[[track]] %||% NULL
      if (is.data.frame(source_window)) {
        source_window <- as.list(source_window)
      }
      utils::modifyList(
        source_window %||% list(),
        list(track = track, individual = individual, species = individual)
      )
    }), tracks)
    coverage_windows <- .complete_common_coverage_windows(
      tracks,
      coverage_windows,
      annotation_windows
    )

    list(
      request_id = paste0("coverage:", request_index),
      annotation = annotation,
      species = selector,
      omitted_species = omitted_species,
      layer_index = as.integer(layer_index),
      tracks = tracks,
      windows = coverage_windows
    )
  })
}

compact_syn_plot_context <- function(context) {
  if (is.null(context)) {
    return(NULL)
  }
  keep_window_fields <- c(
    "chr", "start", "end", "individual", "species", "track"
  )
  windows <- lapply(context$windows %||% list(), function(window) {
    window[intersect(keep_window_fields, names(window))]
  })
  annotation_windows <- lapply(
    context$annotation_windows %||% list(),
    function(window) {
      window[intersect(keep_window_fields, names(window))]
    }
  )
  coverage_requests <- lapply(context$coverage_requests %||% list(), function(request) {
    request_windows <- lapply(request$windows %||% list(), function(window) {
      window[intersect(keep_window_fields, names(window))]
    })
    list(
      request_id = as.character(request$request_id),
      annotation = request$annotation %||% NULL,
      species = request$species %||% NULL,
      omitted_species = isTRUE(request$omitted_species),
      layer_index = as.integer(request$layer_index),
      tracks = as.character(request$tracks %||% character()),
      windows = request_windows
    )
  })
  list(
    windows = windows,
    annotation_windows = annotation_windows,
    annotation_species_order = as.character(
      context$annotation_species_order %||% character()
    ),
    coverage_requests = coverage_requests,
    coverage_tracks = as.character(context$coverage_tracks %||% character())
  )
}

collect_layout_panel_windows <- function(
    syn_data,
    panel_roles = c("annotation", "coverage")) {
  if (!methods::is(syn_data, "SynSpecies")) {
    return(list())
  }

  layout <- species_layout(syn_data)
  if (is.null(layout)) {
    return(list())
  }

  panels <- syn_layout_panels(layout)
  required_cols <- c("xlim_chr", "xlim_min", "xlim_max")
  if (!is.data.frame(panels) || !all(required_cols %in% names(panels))) {
    return(list())
  }

  panel_type <- if ("panel_type" %in% names(panels)) {
    as.character(panels$panel_type)
  } else {
    rep("annotation", nrow(panels))
  }
  panel_type[is.na(panel_type)] <- "annotation"
  recipients <- .layout_panel_recipients(panels, syn_data)
  track_col <- as.character(panels$track)
  xlim_chr <- as.character(panels$xlim_chr)
  xlim_min <- suppressWarnings(as.numeric(panels$xlim_min))
  xlim_max <- suppressWarnings(as.numeric(panels$xlim_max))
  complete_rows <- panel_type %in% panel_roles &
    !is.na(recipients) & nzchar(recipients) &
    !is.na(track_col) & nzchar(track_col) &
    !is.na(xlim_chr) & nzchar(xlim_chr) &
    is.finite(xlim_min) & is.finite(xlim_max)

  if (!any(complete_rows)) {
    return(list())
  }

  panels <- panels[complete_rows, , drop = FALSE]
  panel_type <- panel_type[complete_rows]
  recipients <- recipients[complete_rows]
  track_col <- track_col[complete_rows]
  xlim_chr <- xlim_chr[complete_rows]
  xlim_min <- xlim_min[complete_rows]
  xlim_max <- xlim_max[complete_rows]
  out <- list()
  for (track_name in unique(track_col)) {
    hits <- which(track_col == track_name)
    coverage_hits <- hits[panel_type[hits] == "coverage"]
    if (length(coverage_hits) > 0L) {
      hits <- coverage_hits
    }
    i <- utils::tail(hits, 1L)
    species_name <- recipients[[i]]
    individual <- individuals(syn_data)[[species_name]]
    window <- list(
      chr = if (methods::is(individual, "SynIndividual")) {
        resolve_syn_seqname(individual, xlim_chr[[i]])
      } else {
        xlim_chr[[i]]
      },
      start = xlim_min[[i]],
      end = xlim_max[[i]],
      individual = species_name,
      species = species_name,
      track = track_name
    )
    out[[track_name]] <- window
  }

  out
}

collect_facet_panel_windows <- function(syn_data,
                                        facet = NULL,
                                        annotation_species_order = NULL) {
  if (is.null(facet)) {
    return(list())
  }

  params <- facet$params %||% list()
  if (!.has_facet_panel_xlim(params)) {
    return(list())
  }

  if (methods::is(syn_data, "SynIndividual")) {
    individual_id <- syn_id(syn_data)
    requested <- .facet_panel_xlim_individuals(
      params,
      available = individual_id
    )
    xlim_map <- .facet_panel_xlim_map(requested, params$panel_xlim)
    chr_map <- .facet_panel_xlim_chr_map(requested, params$panel_xlim_chr)
    return(stats::setNames(lapply(requested, function(name) {
      chr <- chr_map[[name]] %||% NA_character_
      if (!is.na(chr) && nzchar(chr)) {
        chr <- resolve_syn_seqname_or_raw(syn_data, chr)
      }
      limits <- xlim_map[[name]]
      list(
        chr = chr,
        start = min(limits),
        end = max(limits),
        individual = individual_id,
        species = individual_id,
        track = individual_id
      )
    }), requested))
  }

  if (!methods::is(syn_data, "SynSpecies")) {
    return(list())
  }

  layout <- species_layout(syn_data)
  if (is.null(layout)) {
    individual <- .facet_panel_xlim_individuals(
      params,
      available = annotation_species_order %||% names(individuals(syn_data))
    )
    layout <- SynLayout(
      panels = data.frame(
        PANEL = seq_along(individual),
        ROW = seq_along(individual),
        COL = 1L,
        track = individual,
        panel_type = "annotation",
        species = individual,
        stringsAsFactors = FALSE
      ),
      free = list(x = length(individual) > 1L, y = FALSE)
    )
  }

  layout <- .apply_facet_panel_xlim_to_layout(
    layout,
    plot_data = syn_data,
    params = params
  )

  species_layout(syn_data) <- layout
  collect_layout_panel_windows(syn_data)
}

find_syn_plot_data <- function(layers, plot_data) {
  if (methods::is(plot_data, "SynSpecies") || methods::is(plot_data, "SynIndividual")) {
    return(plot_data)
  }

  for (layer in layers) {
    if (methods::is(layer$data, "SynSpecies") || methods::is(layer$data, "SynIndividual")) {
      return(layer$data)
    }
  }

  NULL
}

collect_syn_annotation_requests <- function(layer, syn_data, plot_data) {
  if (!(identical(layer$geom, GeomExon) ||
        identical(layer$geom, GeomExon2) ||
        identical(layer$geom, GeomGene) ||
        identical(layer$geom, GeomGeneLabel) ||
        identical(layer$geom, GeomGeneBox) ||
        identical(layer$geom, GeomGeneTag) ||
        identical(layer$geom, GeomMotif))) {
    return(list())
  }
  if (!is_syn_layer_input(layer, plot_data)) {
    return(list())
  }

  params <- syn_layer_params(layer)
  species <- resolve_plot_species_params(syn_data, params$species)
  if (length(species) == 0L) {
    return(list())
  }

  lapply(species, function(species_name) {
    list(
      geom = class(layer$geom)[1L],
      species = species_name,
      explicit_species = !is.null(params$species),
      chr = params$chr,
      subset = params$subset
    )
  })
}

collect_syn_link_requests <- function(layer, syn_data, plot_data) {
  if (!identical(layer$geom, GeomNucLink)) {
    return(list())
  }
  if (!is_syn_layer_input(layer, plot_data)) {
    return(list())
  }

  params <- syn_layer_params(layer)
  alignment <- resolve_plot_alignment_name(syn_data, params$alignment)
  alignments <- if (length(alignment) == 0L || is.null(alignment)) NULL else as.character(alignment)

  if (is.null(alignments) || length(alignments) <= 1L) {
    alignment_obj <- tryCatch(
      resolve_plot_alignment_object(syn_data, alignment),
      error = function(...) NULL
    )

    return(list(list(
      alignment = alignment,
      alignment_obj = alignment_obj,
      reference = params$reference,
      chr = params$chr,
      subset = params$subset,
      filter_by_len = params$filter_by_len
    )))
  }

  lapply(alignments, function(one_alignment) {
    list(
      alignment = one_alignment,
      alignment_obj = tryCatch(
        resolve_plot_alignment_object(syn_data, one_alignment),
        error = function(...) NULL
      ),
      reference = params$reference,
      chr = params$chr,
      subset = params$subset,
      filter_by_len = params$filter_by_len
    )
  })
}

syn_layer_params <- function(layer) {
  geom_defaults <- if (is.function(layer$geom$default_params)) {
    layer$geom$default_params()
  } else {
    list()
  }
  layout_params <- syn_layout_layer_params(layer)
  params <- utils::modifyList(geom_defaults, layout_params)
  utils::modifyList(params, layer$geom_params)
}

syn_layout_layer_params <- function(layer) {
  syn_data <- layer$syn_plot_context$syn_data %||% NULL
  if (is.null(syn_data) && methods::is(layer$data, "SynSpecies")) {
    syn_data <- layer$data
  }

  if (!methods::is(syn_data, "SynSpecies")) {
    return(list())
  }

  layout <- species_layout(syn_data)
  if (is.null(layout)) {
    return(list())
  }

  out <- list(
    exon_height = layout@exon_height,
    x_translation = layout@x_translation
  )
  out[vapply(out, function(x) is.numeric(x) && length(x) == 1L && !is.na(x), logical(1))]
}

resolve_plot_species_params <- function(x, species = NULL) {
  if (methods::is(x, "SynIndividual")) {
    return(syn_id(x))
  }

  if (!methods::is(x, "SynSpecies")) {
    return(species %||% character())
  }

  if (is.null(species)) {
    return(names(individuals(x)))
  }

  unique(as.character(species))
}

resolve_syn_plot_species_order <- function(x,
                                           annotation_requests = NULL,
                                           link_requests = NULL) {
  if (!methods::is(x, "SynSpecies")) {
    return(unique(vapply(annotation_requests %||% list(), function(request) {
      request$species
    }, character(1))))
  }

  annotation_requests <- annotation_requests %||% list()
  explicit_requests <- Filter(function(request) isTRUE(request$explicit_species), annotation_requests)
  explicit_species <- unique(vapply(explicit_requests, function(request) {
    request$species
  }, character(1)))

  if (length(explicit_species) > 0L) {
    selected_species <- explicit_species
  } else {
    selected_species <- names(individuals(x))

    pairwise_species <- unique(unlist(lapply(link_requests %||% list(), function(request) {
      alignment_obj <- request$alignment_obj %||% NULL
      if (methods::is(alignment_obj, "SynPairAlignment")) {
        return(alignment_individuals(alignment_obj))
      }
      character()
    }), use.names = FALSE))
    if (length(pairwise_species) >= 2L) {
      selected_species <- pairwise_species
    }
  }

  if (length(link_requests) == 0L) {
    return(selected_species)
  }

  for (request in link_requests) {
    alignment_obj <- request$alignment_obj %||% NULL
    reference <- request$reference %||% NULL
    if (is.null(reference) ||
        is.null(alignment_obj) ||
        !methods::is(alignment_obj, "SynMultiAlignment") ||
        !identical(alignment_format(alignment_obj), "odgi")) {
      next
    }

    aligned_species <- selected_species[selected_species %in% alignment_individuals(alignment_obj)]
    if (!reference %in% alignment_individuals(alignment_obj) || length(aligned_species) < 2L) {
      next
    }

    ordered_species <- odgi_species_order(
      alignment_obj,
      reference_species = reference,
      selected_species = aligned_species,
      filter_by_len = request$filter_by_len
    )
    return(c(ordered_species, setdiff(selected_species, ordered_species)))
  }

  selected_species
}

resolve_context_species_params <- function(x, species = NULL, context = NULL) {
  if (!is.null(species)) {
    explicit_species <- resolve_plot_species_params(x, species)
    return(explicit_species)
  }

  if (methods::is(x, "SynIndividual")) {
    return(syn_id(x))
  }

  if (!methods::is(x, "SynSpecies")) {
    return(character())
  }

  context_windows <- context$windows %||% list()
  context_species <- names(context_windows)
  context_species <- context_species[!is.na(context_species) & nzchar(context_species)]
  context_species <- unique(context_species)
  if (length(context_species) > 0L) {
    context_individual <- vapply(context_species, function(name) {
      window <- context_windows[[name]]
      as.character((window$individual %||% window$species %||% name)[[1L]])
    }, character(1))
    is_panel_track <- !(context_species %in% names(individuals(x))) |
      (!is.na(context_individual) & nzchar(context_individual) & context_individual != context_species)
    if (any(is_panel_track)) {
      return(context_species)
    }
  }

  annotation_species <- context$annotation_species_order %||% character()
  annotation_species <- annotation_species[annotation_species %in% names(individuals(x))]
  annotation_species <- unique(annotation_species)
  if (length(annotation_species) > 0L) {
    return(annotation_species)
  }

  if (length(context_species) > 0L) {
    return(context_species)
  }

  names(individuals(x))
}

.context_window_for_track <- function(context, species) {
  if (is.null(context)) {
    return(NULL)
  }
  windows <- context$windows %||% list()
  windows[[species]] %||% NULL
}

.context_individual_for_track <- function(x, species, context = NULL) {
  if (!methods::is(x, "SynSpecies")) {
    return(species)
  }
  if (species %in% names(individuals(x))) {
    return(species)
  }

  window <- .context_window_for_track(context, species)
  if (!is.null(window)) {
    individual <- window$individual %||% window$species %||% NULL
    if (!is.null(individual) && length(individual) == 1L && !is.na(individual) && nzchar(individual)) {
      return(as.character(individual))
    }
  }

  layout <- species_layout(x)
  if (!is.null(layout)) {
    panels <- syn_layout_panels(layout)
    if (is.data.frame(panels) && all(c("track", "species") %in% names(panels))) {
      hit <- which(as.character(panels$track) == species)
      if (length(hit) == 1L) {
        individual <- as.character(panels$species[[hit]])
        if (!is.na(individual) && nzchar(individual)) {
          return(individual)
        }
      }
    }
  }

  species
}

resolve_plot_alignment_name <- function(x, alignment = NULL) {
  if (!methods::is(x, "SynSpecies")) {
    return(alignment)
  }

  if (!is.null(alignment)) {
    return(alignment)
  }

  pair_list <- pairwise_alignments(x)
  if (length(pair_list) == 1L) {
    return(names(pair_list)[[1L]])
  }

  multi_list <- multiple_alignments(x)
  if (length(pair_list) == 0L && length(multi_list) == 1L) {
    return(names(multi_list)[[1L]])
  }

  alignment
}

infer_nuclink_species_order <- function(x,
                                        context = NULL,
                                        reference_species = NULL,
                                        alignment = NULL,
                                        filter_by_len = NULL) {
  if (!methods::is(x, "SynSpecies")) {
    return(NULL)
  }

  annotation_species <- context$annotation_species_order %||% character()
  annotation_species <- unique(annotation_species)
  if (length(annotation_species) > 0L) {
    return(annotation_species)
  }

  context_windows <- context$windows %||% list()
  context_species <- names(context_windows)
  context_species <- unique(context_species)
  if (length(context_species) > 0L) {
    return(context_species)
  }

  if (!is.null(reference_species)) {
    alignment_obj <- tryCatch(
      resolve_plot_alignment_object(x, resolve_plot_alignment_name(x, alignment)),
      error = function(...) NULL
    )
    if (!is.null(alignment_obj) &&
        methods::is(alignment_obj, "SynMultiAlignment") &&
        identical(alignment_format(alignment_obj), "odgi")) {
      return(odgi_species_order(
        alignment_obj,
        reference_species = reference_species,
        selected_species = names(individuals(x)),
        filter_by_len = filter_by_len
      ))
    }
  }

  names(individuals(x))
}

resolve_plot_alignment_object <- function(x, alignment = NULL) {
  if (!methods::is(x, "SynSpecies")) {
    cli::cli_abort("Plot-derived alignments require a {.cls SynSpecies} object.")
  }

  pair_list <- pairwise_alignments(x)
  if (!is.null(alignment) && alignment %in% names(pair_list)) {
    return(pair_list[[alignment]])
  }
  if (is.null(alignment) && length(pair_list) == 1L) {
    return(pair_list[[1L]])
  }

  multi_list <- multiple_alignments(x)
  if (!is.null(alignment) && alignment %in% names(multi_list)) {
    return(multi_list[[alignment]])
  }
  if (is.null(alignment) && length(pair_list) == 0L && length(multi_list) == 1L) {
    return(multi_list[[1L]])
  }

  NULL
}

resolve_plot_link_alignments <- function(x,
                                         alignment = NULL,
                                         species_order = NULL,
                                         reference_species = NULL,
                                         filter_by_len = NULL) {
  if (!methods::is(x, "SynSpecies")) {
    cli::cli_abort("Plot-derived link windows require a {.cls SynSpecies} object.")
  }

  pair_list <- pairwise_alignments(x)
  species_order <- unique(as.character(species_order %||% character()))

  if (!is.null(alignment) && length(alignment) > 1L) {
    alignment <- as.character(alignment)
    missing_alignments <- setdiff(alignment, names(pair_list))
    if (length(missing_alignments) > 0L) {
      cli::cli_abort(
        "Unknown alignment {.val {missing_alignments[[1L]]}}. Available pairwise alignments: {.val {names(pair_list)}}."
      )
    }
    return(unname(pair_list[alignment]))
  }

  if (!is.null(alignment) && alignment %in% names(pair_list)) {
    return(list(pair_list[[alignment]]))
  }

  if (is.null(alignment) && length(pair_list) > 0L && length(species_order) > 1L) {
    resolved_pairs <- lapply(seq_len(length(species_order) - 1L), function(i) {
      species_pair <- c(species_order[[i]], species_order[[i + 1L]])
      hits <- pair_list[vapply(pair_list, function(pair) {
        setequal(alignment_individuals(pair), species_pair)
      }, logical(1))]
      if (length(hits) > 1L) {
        cli::cli_abort(
          "Multiple pairwise alignments connect {.val {species_pair[[1L]]}} and {.val {species_pair[[2L]]}}; supply {.arg alignment}."
        )
      }
      if (length(hits) == 0L) {
        return(NULL)
      }
      hits[[1L]]
    })
    resolved_pairs <- Filter(Negate(is.null), resolved_pairs)
    if (length(resolved_pairs) > 0L) {
      return(resolved_pairs)
    }
  }

  if (is.null(alignment) && length(pair_list) == 1L) {
    return(list(pair_list[[1L]]))
  }

  multi <- resolve_plot_alignment_object(x, alignment)
  if (!is.null(multi)) {
    if (methods::is(multi, "SynPairAlignment")) {
      return(list(multi))
    }
    if (!identical(alignment_format(multi), "odgi")) {
      cli::cli_abort(
        "Only ODGI multiple alignments can be dispatched as pairwise link panels right now."
      )
    }
    species_order <- species_order[species_order %in% alignment_individuals(multi)]
    if (length(species_order) == 0L) {
      species_order <- alignment_individuals(multi)
    }
    if (length(species_order) < 2L) {
      cli::cli_abort(
        c(
          "Need at least two plotted species to dispatch ODGI alignment {.val {alignment_name(multi)}} to middle link panels.",
          "i" = "Use annotation layers that resolve two or more species, or add explicit pairwise alignments."
        )
      )
    }

    out <- .odgi_pairwise_alignments_from_multi(
      msa = multi,
      species_order = species_order,
      reference_species = reference_species,
      filter_by_len = filter_by_len
    )
    if (length(out) == 0L) {
      cli::cli_abort("No adjacent ODGI pairwise links could be derived for the plotted species order.")
    }
    return(unname(out))
  }

  if (length(pair_list) == 0L) {
    cli::cli_abort("The {.cls SynSpecies} object does not contain any pairwise or ODGI multiple alignments.")
  }

  if (is.null(alignment)) {
    cli::cli_abort("Supply {.arg alignment} when multiple pairwise alignments are available.")
  }

  cli::cli_abort(
    "Unknown alignment {.val {alignment}}. Available pairwise alignments: {.val {names(pair_list)}}."
  )
}

resolve_plot_pairwise_alignment <- function(x,
                                            alignment = NULL,
                                            pair_species = NULL,
                                            reference_species = NULL) {
  out <- resolve_plot_link_alignments(
    x,
    alignment = alignment,
    species_order = pair_species,
    reference_species = reference_species
  )
  if (length(out) != 1L) {
    cli::cli_abort("Need exactly one pairwise alignment in this plotting context.")
  }
  out[[1L]]
}

collect_explicit_annotation_windows <- function(annotation_requests, syn_data) {
  windows <- list()

  for (request in annotation_requests) {
    if (is.null(request$subset)) {
      next
    }

    windows[[request$species]] <- normalize_syn_window_request(
      x = syn_data,
      species = request$species,
      chr = request$chr,
      subset = request$subset,
      allow_missing_subset = FALSE,
      context = NULL
    )
  }

  windows
}

derive_syn_plot_windows <- function(x, windows, link_requests, annotation_species_order = NULL) {
  if (!methods::is(x, "SynSpecies") || length(link_requests) == 0L) {
    return(windows)
  }

  for (request in link_requests) {
    alignment_obj <- request$alignment_obj

    selected_species <- annotation_species_order %||% names(windows)
    selected_species <- unique(as.character(selected_species))

    pair_species <- if (methods::is(alignment_obj, "SynPairAlignment")) {
      alignment_individuals(alignment_obj)
    } else {
      selected_species
    }
    if (length(pair_species) > 0L && all(pair_species %in% names(windows))) {
      next
    }

    if (!is.null(request$chr) || !is.null(request$subset)) {
      if (is.null(request$reference) || is.null(request$chr) || is.null(request$subset)) {
        cli::cli_abort(
          "Provide {.arg reference}, {.arg chr}, and {.arg subset} together when subsetting {.fn geom_nuclink}."
        )
      }
      if (!is.numeric(request$subset) || length(request$subset) != 2L) {
        cli::cli_abort("{.arg subset} must be a numeric vector of length 2 for {.fn geom_nuclink}.")
      }

      out <- subset_synspecies_window(
        x = x,
        reference_species = request$reference,
        chr = request$chr,
        start = min(request$subset),
        end = max(request$subset),
        alignment = request$alignment,
        selected_species = selected_species,
        filter_by_len = request$filter_by_len
      )
      windows <- utils::modifyList(windows, out$windows)
      next
    }

    available <- intersect(selected_species, names(windows))
    if (length(available) != 1L) {
      if (methods::is(alignment_obj, "SynPairAlignment")) {
        windows <- utils::modifyList(windows, infer_pairwise_alignment_windows(x, alignment_obj))
      }
      next
    }

    reference_window <- windows[[available[[1L]]]]
    out <- subset_synspecies_window(
      x = x,
      reference_species = available[[1L]],
      chr = reference_window$chr,
      start = reference_window$start,
      end = reference_window$end,
      alignment = request$alignment,
      selected_species = selected_species,
      filter_by_len = request$filter_by_len
    )
    windows <- utils::modifyList(windows, out$windows)
  }

  windows
}

infer_pairwise_alignment_windows <- function(x, pair) {
  if (!methods::is(pair, "SynPairAlignment")) {
    return(list())
  }

  paf <- pairwise_alignment_data(pair)
  if (!is.data.frame(paf) || nrow(paf) == 0L) {
    return(list())
  }

  out <- list()
  query_species <- query_individual(pair)
  target_species <- target_individual(pair)

  query_chr <- unique(as.character(paf$qchr))
  query_chr <- query_chr[!is.na(query_chr) & nzchar(query_chr)]
  if (length(query_chr) == 1L) {
    query_individual_obj <- if (methods::is(x, "SynSpecies")) individuals(x)[[query_species]] else NULL
    out[[query_species]] <- data.frame(
      chr = if (methods::is(query_individual_obj, "SynIndividual")) {
        resolve_syn_seqname_or_raw(query_individual_obj, query_chr[[1L]])
      } else {
        query_chr[[1L]]
      },
      start = min(paf$qstart, na.rm = TRUE),
      end = max(paf$qend, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }

  target_chr <- unique(as.character(paf$tchr))
  target_chr <- target_chr[!is.na(target_chr) & nzchar(target_chr)]
  if (length(target_chr) == 1L) {
    target_individual_obj <- if (methods::is(x, "SynSpecies")) individuals(x)[[target_species]] else NULL
    out[[target_species]] <- data.frame(
      chr = if (methods::is(target_individual_obj, "SynIndividual")) {
        resolve_syn_seqname_or_raw(target_individual_obj, target_chr[[1L]])
      } else {
        target_chr[[1L]]
      },
      start = min(paf$tstart, na.rm = TRUE),
      end = max(paf$tend, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }

  out
}

normalize_syn_window_request <- function(x,
                                         species,
                                         chr = NULL,
                                         subset = NULL,
                                         allow_missing_subset = TRUE,
                                         context = NULL,
                                         geom = "annotation") {
  individual_name <- .context_individual_for_track(x, species, context = context)
  individual <- resolve_syn_individual(x, species = individual_name)

  if (!is.null(subset)) {
    if (is.null(chr)) {
      cli::cli_abort(
        "{.arg chr} must be supplied when {.arg subset} is used for {.val {geom}}."
      )
    }
    if (!is.numeric(subset) || length(subset) != 2L) {
      cli::cli_abort("{.arg subset} must be a numeric vector of length 2.")
    }

    return(list(
      chr = resolve_syn_seqname_or_raw(individual, chr),
      start = min(subset),
      end = max(subset)
    ))
  }

  derived_window <- .context_window_for_track(context, species) %||%
    .context_window_for_track(context, individual_name)
  if (!is.null(derived_window)) {
    derived_window$chr <- resolve_syn_seqname_or_raw(
      individual,
      derived_window$chr
    )
    if (!is.null(chr)) {
      requested_chr <- resolve_syn_seqname_or_raw(individual, chr)
      if (!identical(requested_chr, derived_window$chr)) {
        cli::cli_abort(
          "Derived window for {.val {species}} is on {.val {derived_window$chr}}, not {.val {requested_chr}}."
        )
      }
    }
    return(derived_window)
  }

  if (allow_missing_subset) {
    return(list(chr = resolve_syn_seqname_or_raw(individual, chr), start = NULL, end = NULL))
  }

  if (identical(geom, "geom_coverage")) {
    cli::cli_abort(c(
      "No genomic window is available for coverage track {.val {species}}.",
      "i" = "Add an explicit annotation window, use a stored layout window, or supply facet {.arg xlim}/{.arg xlim_chr}."
    ))
  }

  cli::cli_abort(
    c(
      "{.arg subset} is required for Syn annotation layers.",
      "i" = "You can omit it only when the species window can be derived from {.fn geom_nuclink} and another annotation layer with defined coordinates."
    )
  )
}

window_to_region_string <- function(window) {
  paste0(window$chr, ":", window$start, "-", window$end)
}

is_unrestricted_syn_window <- function(window) {
  is.null(window$chr) && is.null(window$start) && is.null(window$end)
}

resolve_syn_layer_data <- function(x, layer) {
  handler <- layer$geom$syn_data
  if (is.null(handler)) {
    geom_name <- class(layer$geom)[1] %||% ""
    cli::cli_abort(
      "Syn object input is not yet implemented for geom {.val {geom_name}}."
    )
  }
  handler(x, layer)
}

resolve_syn_domain_annotation <- function(x, annotation = NULL, allow_missing = FALSE) {
  if (!methods::is(x, "SynIndividual")) {
    cli::cli_abort("Protein motif plotting requires a {.cls SynIndividual} object.")
  }

  if (!is.null(annotation)) {
    ann_names <- annotation_names(x)
    if (!annotation %in% ann_names) {
      if (allow_missing) {
        return(NULL)
      }
      cli::cli_abort("Unknown annotation layer {.val {annotation}}.")
    }
    ann <- get_annotation(x, annotation)
    if (!methods::is(ann, "SynProteinDomainAnnotation")) {
      if (allow_missing) {
        return(NULL)
      }
      cli::cli_abort(
        "Annotation layer {.val {annotation}} is not a {.cls SynProteinDomainAnnotation}."
      )
    }
    return(ann)
  }

  ann_names <- annotation_names(x)
  for (ann_name in ann_names) {
    ann <- get_annotation(x, ann_name)
    if (methods::is(ann, "SynProteinDomainAnnotation")) {
      return(ann)
    }
  }

  if (allow_missing) {
    return(NULL)
  }

  cli::cli_abort(
    "No {.cls SynProteinDomainAnnotation} layer is attached to this {.cls SynIndividual}."
  )
}

resolve_syn_bigwig_annotation <- function(x,
                                          annotation = NULL,
                                          allow_missing = FALSE) {
  if (!methods::is(x, "SynIndividual")) {
    cli::cli_abort("Coverage plotting requires a {.cls SynIndividual} object.")
  }

  individual_id <- syn_id(x)
  if (!is.null(annotation)) {
    if (!is.character(annotation) || length(annotation) != 1L ||
        is.na(annotation) || !nzchar(annotation)) {
      cli::cli_abort(
        "Coverage annotation for individual {.val {individual_id}} must be one non-empty layer name."
      )
    }
    if (!annotation %in% annotation_names(x)) {
      if (allow_missing) {
        return(NULL)
      }
      cli::cli_abort(
        "Individual {.val {individual_id}} has no annotation layer {.val {annotation}}."
      )
    }
    ann <- get_annotation(x, annotation)
    if (!methods::is(ann, "SynBigWigAnnotation")) {
      if (allow_missing) {
        return(NULL)
      }
      cli::cli_abort(
        "Annotation layer {.val {annotation}} on individual {.val {individual_id}} is not a {.cls SynBigWigAnnotation}."
      )
    }
    return(ann)
  }

  matches <- lapply(annotation_names(x), function(ann_name) {
    ann <- get_annotation(x, ann_name)
    if (methods::is(ann, "SynBigWigAnnotation")) ann else NULL
  })
  matches <- Filter(Negate(is.null), matches)

  if (length(matches) == 1L) {
    return(matches[[1L]])
  }
  if (allow_missing) {
    return(NULL)
  }
  if (length(matches) == 0L) {
    cli::cli_abort(
      "Individual {.val {individual_id}} has no attached {.cls SynBigWigAnnotation} layer."
    )
  }
  cli::cli_abort(
    "Individual {.val {individual_id}} has multiple attached {.cls SynBigWigAnnotation} layers; supply {.arg annotation}."
  )
}

query_syn_bigwig_window <- function(annotation, window, context = NULL) {
  path <- normalizePath(source_file(annotation), mustWork = FALSE)
  key <- paste(path, window$chr, window$start, window$end, sep = "::")
  cache <- context$query_cache %||% NULL
  if (is.environment(cache) && exists(key, envir = cache, inherits = FALSE)) {
    return(get(key, envir = cache, inherits = FALSE))
  }

  region <- GenomicRanges::GRanges(
    window$chr,
    IRanges::IRanges(window$start, window$end)
  )
  result <- query_annotation(annotation, region)
  if (is.environment(cache)) {
    assign(key, result, envir = cache)
  }
  result
}

.empty_syn_coverage_df <- function() {
  out <- data.frame(
    track = character(),
    individual = character(),
    species = character(),
    chr = character(),
    interval_start = integer(),
    interval_end = integer(),
    genomic_xmin = integer(),
    genomic_xmax = integer(),
    xmin = numeric(),
    xmax = numeric(),
    coverage = numeric(),
    group = integer(),
    .ggexon_panel_role = character(),
    stringsAsFactors = FALSE
  )
  attr(out, "ggexon_panel_role") <- "coverage"
  out
}

.coverage_annotation_selector <- function(annotation = NULL) {
  if (is.null(annotation)) {
    return(NULL)
  }
  as.character(annotation)[[1L]]
}

.match_context_coverage_request <- function(x,
                                             context,
                                             annotation = NULL,
                                             species = NULL) {
  requests <- context$coverage_requests %||% list()
  if (length(requests) == 0L) {
    return(NULL)
  }

  annotation <- .coverage_annotation_selector(annotation)
  omitted_species <- is.null(species)
  selector <- .normalize_coverage_species_selector(x, species)
  matches <- which(vapply(requests, function(request) {
    request_annotation <- .coverage_annotation_selector(
      request$annotation %||% NULL
    )
    annotation_matches <- identical(request_annotation, annotation)
    omitted_matches <- identical(
      isTRUE(request$omitted_species),
      omitted_species
    )
    species_matches <- omitted_species || identical(
      request$species %||% NULL,
      selector
    )
    annotation_matches && omitted_matches && species_matches
  }, logical(1)))

  if (length(matches) == 0L) {
    return(NULL)
  }
  requests[[matches[[1L]]]]
}

syn_to_coverage_df <- function(x,
                               species = NULL,
                               annotation = NULL,
                               context = NULL) {
  request <- .match_context_coverage_request(
    x,
    context,
    annotation = annotation,
    species = species
  )
  if (!is.null(request)) {
    context$windows <- request$windows %||% list()
    context$coverage_tracks <- request$tracks %||% character()
    context$coverage_requests <- list()
    species <- request$tracks %||% character()
  } else if (length(context$coverage_requests %||% list()) > 1L) {
    cli::cli_abort(
      "No build-local coverage request matches this layer's annotation and species selector."
    )
  } else if (is.null(species) &&
             length(context$coverage_tracks %||% character()) > 0L) {
    species <- context$coverage_tracks
  }

  species <- resolve_context_species_params(x, species, context)

  if (length(species %||% character()) == 0L) {
    return(.empty_syn_coverage_df())
  }

  if (methods::is(x, "SynSpecies") && length(species %||% character()) > 1L) {
    species <- unique(as.character(species))
    out <- dplyr::bind_rows(lapply(species, function(species_name) {
      syn_to_coverage_df(
        x = x,
        species = species_name,
        annotation = annotation,
        context = context
      )
    }))
    if (nrow(out) == 0L) {
      attr(out, "ggexon_panel_role") <- "coverage"
    }
    return(out)
  }

  individual_name <- if (length(species %||% character()) == 0L) {
    NULL
  } else {
    as.character(species[[1L]])
  }
  individual <- resolve_syn_individual(
    x,
    species = .context_individual_for_track(x, individual_name, context = context)
  )
  ann <- resolve_syn_bigwig_annotation(
    individual,
    annotation = annotation
  )
  window <- normalize_syn_window_request(
    x = x,
    species = individual_name %||% syn_id(individual),
    allow_missing_subset = FALSE,
    context = context,
    geom = "geom_coverage"
  )
  signal <- query_syn_bigwig_window(ann, window, context = context)

  if (length(signal) == 0L) {
    return(.empty_syn_coverage_df())
  }

  interval_start <- BiocGenerics::start(signal)
  interval_end <- BiocGenerics::end(signal)
  track_name <- as.character(
    window$track %||% individual_name %||% syn_id(individual)
  )

  data.frame(
    track = track_name,
    individual = syn_id(individual),
    species = syn_id(individual),
    chr = as.character(GenomeInfoDb::seqnames(signal)),
    interval_start = interval_start,
    interval_end = interval_end,
    genomic_xmin = interval_start,
    genomic_xmax = interval_end,
    xmin = interval_start - 0.5,
    xmax = interval_end + 0.5,
    coverage = as.numeric(S4Vectors::mcols(signal)$score),
    group = seq_along(signal),
    .ggexon_panel_role = "coverage",
    stringsAsFactors = FALSE
  )
}

syn_to_motif_df <- function(x,
                            species = NULL,
                            chr = NULL,
                            subset = NULL,
                            annotation = NULL,
                            ids = NULL,
                            domains = NULL,
                            model = "all",
                            motif = NULL,
                            y_offset = 0,
                            context = NULL) {
  species <- resolve_context_species_params(x, species, context)

  if (methods::is(x, "SynSpecies") && length(species %||% character()) > 1L) {
    species <- unique(as.character(species))
    return(dplyr::bind_rows(lapply(species, function(species_name) {
      syn_to_motif_df(
        x = x,
        species = species_name,
        chr = chr,
        subset = subset,
        annotation = annotation,
        ids = ids,
        domains = domains,
        model = model,
        motif = motif,
        y_offset = y_offset,
        context = context
      )
    })))
  }

  individual <- resolve_syn_individual(x, species = species)
  ann <- resolve_syn_domain_annotation(
    individual,
    annotation = annotation,
    allow_missing = TRUE
  )
  if (is.null(ann)) {
    return(data.frame())
  }

  window <- normalize_syn_window_request(
    x = x,
    species = syn_id(individual),
    chr = chr,
    subset = subset,
    allow_missing_subset = is.null(ids),
    context = context,
    geom = "geom_motif"
  )

  projected_df <- project_domains_to_genome(
    x = individual,
    annotation = annotation_name(ann),
    ids = ids,
    domains = domains,
    model = model,
    motif = motif,
    chr = window$chr,
    start = window$start,
    end = window$end
  )

  if (nrow(projected_df) == 0L) {
    return(data.frame())
  }

  model_levels <- .resolve_projected_model_levels(projected_df$model, model = model)

  order_df <- data.frame(
    transcripts = projected_df$transcripts,
    model = projected_df$model,
    xmin = projected_df$xmin,
    stringsAsFactors = FALSE
  )
  order_df <- stats::aggregate(xmin ~ transcripts + model, data = order_df, FUN = min)
  transcript_order <- stats::aggregate(xmin ~ transcripts, data = order_df, FUN = min)
  transcript_order <- transcript_order[
    order(transcript_order$xmin, transcript_order$transcripts),
    ,
    drop = FALSE
  ]
  transcript_order$transcript_rank <- seq_len(nrow(transcript_order))
  order_df <- merge(
    order_df,
    transcript_order[, c("transcripts", "transcript_rank")],
    by = "transcripts",
    all.x = TRUE,
    sort = FALSE
  )
  order_df$model_rank <- match(order_df$model, model_levels)
  order_df$stack_id <- paste(order_df$transcripts, order_df$model, sep = "::")
  order_df <- order_df[
    order(order_df$transcript_rank, order_df$model_rank, order_df$model, order_df$xmin),
    ,
    drop = FALSE
  ]
  order_df$ymin <- rev(seq_len(nrow(order_df))) * 2 + y_offset
  order_df$group <- seq_len(nrow(order_df))

  out <- data.frame(
    xmin = projected_df$xmin,
    xmax = projected_df$xmax,
    strand = projected_df$strand,
    transcripts = projected_df$transcripts,
    track = syn_id(individual),
    model = projected_df$model,
    motif = projected_df$motif,
    domain_id = projected_df$domain_id,
    text = projected_df$text,
    stack_id = paste(projected_df$transcripts, projected_df$model, sep = "::"),
    stringsAsFactors = FALSE
  )

  out <- merge(
    out,
    order_df[, c("stack_id", "ymin", "group")],
    by = "stack_id",
    all.x = TRUE,
    sort = FALSE
  )
  out <- out[order(match(out$stack_id, order_df$stack_id), out$xmin, out$xmax), , drop = FALSE]
  out$transcripts <- as.character(out$transcripts)
  out$PANEL <- 1L
  out$stack_id <- NULL
  rownames(out) <- NULL
  out
}

#' Project protein-domain coordinates onto genomic coordinates
#'
#' Uses transcript CDS structure from a `SynIndividual` to convert
#' protein-domain intervals into one or more genomic intervals.
#'
#' @param x A `SynIndividual` object.
#' @param annotation Optional name of an attached `SynProteinDomainAnnotation`
#'   layer. Defaults to the first available protein-domain annotation.
#' @param ids Optional explicit identifier vector matched against the domain
#'   annotation key column.
#' @param domains Optional domain names/accessions to filter.
#' @param model InterProScan analysis model(s) to keep. Accepts a single
#'   string, a character vector, or `"all"`.
#' @param motif Optional motif name(s) used to filter the InterProScan table.
#' @param genes Optional gene identifiers used to limit the projected proteins.
#' @param transcripts Optional transcript identifiers used to limit the
#'   projected proteins.
#' @param chr Optional chromosome name used to define the genomic window.
#' @param start,end Optional genomic window bounds.
#'
#' @return A data frame with projected genomic motif segments.
#' @export
project_domains_to_genome <- function(x,
                                      annotation = NULL,
                                      ids = NULL,
                                      domains = NULL,
                                      model = "all",
                                      motif = NULL,
                                      genes = NULL,
                                      transcripts = NULL,
                                      chr = NULL,
                                      start = NULL,
                                      end = NULL) {
  if (!methods::is(x, "SynIndividual")) {
    stop("`project_domains_to_genome()` expects a SynIndividual object.", call. = FALSE)
  }

  if (!is.null(chr)) {
    chr <- resolve_syn_seqname(x, chr)
  }

  ann <- resolve_syn_domain_annotation(x, annotation = annotation)
  

  domain_df <- query_domains(ann, ids = ids, domains = domains)

  domain_df <- .filter_projectable_domains(domain_df, model = model, motif = motif)
  if (nrow(domain_df) == 0L) {
    return(data.frame())
  }

  key_col <- ann@keytype
  if (!key_col %in% colnames(domain_df)) {
    stop(
      "The protein-domain table does not contain the key column: ",
      key_col,
      call. = FALSE
    )
  }
  if (!all(c("start", "end") %in% colnames(domain_df))) {
    stop(
      "Protein-domain projection requires `start` and `end` columns.",
      call. = FALSE
    )
  }

  # if without specify which [id], guess it from the table 
  if (is.null(ids)) {
    seed_gr <- query_features(
      x,
      genes = genes,
      transcripts = transcripts,
      chr = chr,
      start = start,
      end = end,
      feature_type = NULL,
      all = FALSE
    )
    if (length(seed_gr) == 0L) {
      return(data.frame())
    }
    candidate_df <- .domain_projection_candidates(seed_gr)
    key_values <- unique(candidate_df$match_key)
    key_values <- key_values[!is.na(key_values) & nzchar(key_values)]
    domain_df <- domain_df[
      as.character(domain_df[[key_col]]) %in% key_values,
      ,
      drop = FALSE
    ]
    if (nrow(domain_df) == 0L) {
      return(data.frame())
    }
  }

  if (!is.null(ids)) {
    target_cds <- query_features(
      x,
      genes = genes,
      transcripts = transcripts %||% ids,
      chr = chr,
      start = start,
      end = end,
      feature_type = "CDS",
      all = is.null(genes) && is.null(transcripts) && is.null(chr)
    )
  } else {
    seed_gr <- query_features(
      x,
      genes = genes,
      transcripts = transcripts,
      chr = chr,
      start = start,
      end = end,
      feature_type = NULL,
      all = FALSE
    )
    transcript_ids <- unique(.annotation_transcript_ids(seed_gr))
    transcript_ids <- transcript_ids[!is.na(transcript_ids) & nzchar(transcript_ids)]
    if (length(transcript_ids) == 0L) {
      return(data.frame())
    }
    target_cds <- query_features(
      x,
      transcripts = transcript_ids,
      feature_type = "CDS"
    )
  }

  if (length(target_cds) == 0L) {
    return(data.frame())
  }

  text_col <- if ("domain_name" %in% colnames(domain_df)) {
    "domain_name"
  } else {
    .pick_domain_column(domain_df)
  }

  transcript_meta <- .domain_projection_transcript_meta(target_cds)
  out <- lapply(seq_len(nrow(domain_df)), function(i) {
    .project_one_domain_row(
      domain_row = domain_df[i, , drop = FALSE],
      key_col = key_col,
      text_col = text_col,
      transcript_meta = transcript_meta,
      cds_gr = target_cds
    )
  })
  out <- dplyr::bind_rows(out)

  if (!is.null(chr)) {
    chr <- resolve_syn_seqname(x, chr)
    out <- out[out$seqnames == chr, , drop = FALSE]
  }
  if (!is.null(start)) {
    out <- out[out$xmax >= start, , drop = FALSE]
  }
  if (!is.null(end)) {
    out <- out[out$xmin <= end, , drop = FALSE]
  }

  rownames(out) <- NULL
  out
}

#' Project amino-acid variants onto genomic coordinates
#'
#' Converts protein-coordinate variants (for example `C316H` at residue 316)
#' into genomic coordinates using a transcript's CDS structure, so amino-acid
#' variants can be annotated directly on the exon/intron model drawn by
#' [geom_exon()]. Each variant is treated as a single codon: residue `p` maps to
#' CDS nucleotides `(p - 1) * 3 + 1 .. p * 3`, walked across CDS segments so a
#' codon that spans a splice junction yields one genomic row per segment. The
#' phase of the 5'-most CDS is honoured for 5'-truncated gene models.
#'
#' This is the mutation counterpart of [project_domains_to_genome()] and shares
#' the same coordinate-projection core.
#'
#' @param x A `SynIndividual` object.
#' @param annotation Optional `SynProteinMutationAnnotation` layer name.
#'   Defaults to the first attached protein-mutation annotation.
#' @param genes,transcripts Optional identifiers limiting the transcripts that
#'   variants are projected onto.
#' @param strains,mutation,event_type,min_sample_count,protein_ranges,ref
#'   Optional variant filters forwarded to [query_protein_mutations()].
#' @param chr,start,end Optional genomic window used to clip the projection.
#'
#' @return A data frame with one row per (variant, overlapped CDS segment),
#'   containing the projected `seqnames`, `xmin`, `xmax`, `strand`,
#'   `transcripts`, and the variant metadata columns (`position`, `ref`, `alt`,
#'   `mutation`, ...). Returns an empty data frame when nothing projects.
#' @seealso [project_domains_to_genome()], [geom_aa_variant()]
#' @export
project_mutations_to_genome <- function(x,
                                        annotation = NULL,
                                        genes = NULL,
                                        transcripts = NULL,
                                        strains = NULL,
                                        mutation = NULL,
                                        event_type = NULL,
                                        min_sample_count = NULL,
                                        protein_ranges = NULL,
                                        ref = NULL,
                                        chr = NULL,
                                        start = NULL,
                                        end = NULL) {
  if (!methods::is(x, "SynIndividual")) {
    stop("`project_mutations_to_genome()` expects a SynIndividual object.", call. = FALSE)
  }

  if (!is.null(chr)) {
    chr <- resolve_syn_seqname(x, chr)
  }

  ann <- resolve_syn_protein_mutation_annotation(x, annotation = annotation)
  key_col <- ann@keytype

  mut <- query_protein_mutations(
    x,
    annotation = annotation,
    genes = genes,
    event_type = event_type,
    min_sample_count = min_sample_count,
    strains = strains,
    mutation = mutation,
    protein_ranges = protein_ranges,
    ref = ref
  )
  if (nrow(mut) == 0L) {
    return(data.frame())
  }
  if (!key_col %in% names(mut)) {
    stop("The mutation table does not contain the key column: ", key_col, call. = FALSE)
  }
  if (!"position" %in% names(mut)) {
    stop("Variant projection requires a `position` column in the mutation table.", call. = FALSE)
  }

  key_values <- unique(as.character(mut[[key_col]]))
  key_values <- key_values[!is.na(key_values) & nzchar(key_values)]
  if (length(key_values) == 0L) {
    return(data.frame())
  }

  # Resolve the transcripts whose CDS to project onto, mirroring the seed-and-CDS
  # lookup used by project_domains_to_genome().
  seed_gr <- query_features(
    x,
    genes = if (identical(key_col, "gene_id")) key_values else genes,
    transcripts = if (identical(key_col, "gene_id")) {
      transcripts
    } else {
      unique(c(transcripts, key_values))
    },
    chr = chr,
    start = start,
    end = end,
    feature_type = NULL,
    all = FALSE
  )
  if (length(seed_gr) == 0L) {
    return(data.frame())
  }
  transcript_ids <- unique(.annotation_transcript_ids(seed_gr))
  transcript_ids <- transcript_ids[!is.na(transcript_ids) & nzchar(transcript_ids)]
  if (length(transcript_ids) == 0L) {
    return(data.frame())
  }
  target_cds <- query_features(x, transcripts = transcript_ids, feature_type = "CDS")
  if (length(target_cds) == 0L) {
    return(data.frame())
  }

  transcript_meta <- .domain_projection_transcript_meta(target_cds)
  out <- lapply(seq_len(nrow(mut)), function(i) {
    .project_one_mutation_row(
      mut_row = mut[i, , drop = FALSE],
      key_col = key_col,
      transcript_meta = transcript_meta,
      cds_gr = target_cds
    )
  })
  out <- dplyr::bind_rows(out)
  if (nrow(out) == 0L) {
    return(out)
  }

  if (!is.null(chr)) {
    out <- out[out$seqnames == chr, , drop = FALSE]
  }
  if (!is.null(start)) {
    out <- out[out$xmax >= start, , drop = FALSE]
  }
  if (!is.null(end)) {
    out <- out[out$xmin <= end, , drop = FALSE]
  }

  rownames(out) <- NULL
  out
}

.project_one_mutation_row <- function(mut_row, key_col, transcript_meta, cds_gr) {
  key_value <- as.character(mut_row[[key_col]][[1L]])
  position <- suppressWarnings(as.integer(mut_row$position[[1L]]))
  if (is.na(key_value) || !nzchar(key_value) || is.na(position)) {
    return(data.frame())
  }

  tx_hits <- transcript_meta$transcript_id[
    transcript_meta$transcript_id == key_value |
      transcript_meta$gene_id == key_value |
      transcript_meta$gene_name == key_value
  ]
  tx_hits <- unique(tx_hits[!is.na(tx_hits) & nzchar(tx_hits)])
  if (length(tx_hits) == 0L) {
    return(data.frame())
  }

  # Carry every variant column except those that would clash with the
  # projection output, so aesthetics like fill = ref or label = mutation work.
  keep_meta <- setdiff(
    names(mut_row),
    c("seqnames", "xmin", "xmax", "strand", "transcripts")
  )

  dplyr::bind_rows(lapply(tx_hits, function(tx_id) {
    tx_gr <- cds_gr[.coalesce_character_cols(
      S4Vectors::mcols(cds_gr),
      c("transcript_id", "Parent", "ID")
    ) == tx_id]
    segs <- .aa_interval_to_genome(tx_gr, aa_start = position, aa_end = position)
    if (nrow(segs) == 0L) {
      return(data.frame())
    }
    seg_meta <- mut_row[rep(1L, nrow(segs)), keep_meta, drop = FALSE]
    rownames(seg_meta) <- NULL
    data.frame(
      seqnames = segs$seqnames,
      xmin = segs$xmin,
      xmax = segs$xmax,
      strand = segs$strand,
      transcripts = tx_id,
      seg_meta,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }))
}

.filter_projectable_domains <- function(domain_df,
                                        model = "all",
                                        motif = NULL) {
  if (!(is.data.frame(domain_df) || methods::is(domain_df, "DataFrame")) ||
      nrow(domain_df) == 0L) {
    return(domain_df)
  }

  model_filter <- .normalize_character_filter(model, arg = "model", allow_all = TRUE)
  if (!is.null(model_filter)) {
    if (!"analysis" %in% colnames(domain_df)) {
      stop("The domain table does not contain an `analysis` column.", call. = FALSE)
    }
    available_models <- unique(as.character(domain_df$analysis))
    missing_models <- setdiff(model_filter, available_models)
    if (length(missing_models) > 0L) {
      stop(
        "Unknown InterProScan model(s): ",
        paste(missing_models, collapse = ", "),
        call. = FALSE
      )
    }
    domain_df <- domain_df[
      as.character(domain_df$analysis) %in% model_filter,
      ,
      drop = FALSE
    ]
  }

  motif_filter <- .normalize_character_filter(motif, arg = "motif", allow_all = FALSE)
  if (is.null(motif_filter)) {
    return(domain_df)
  }

  candidate_cols <- intersect(
    c(
      "domain_name",
      "domain",
      "interpro_description",
      "signature_description",
      "interpro_accession",
      "signature_accession"
    ),
    colnames(domain_df)
  )
  if (length(candidate_cols) == 0L) {
    stop("The domain table does not contain motif-identifying columns.", call. = FALSE)
  }

  motif_filter_lower <- to_lower_ascii(motif_filter)
  keep <- Reduce(`|`, lapply(candidate_cols, function(col) {
    values <- to_lower_ascii(as.character(domain_df[[col]]))
    values %in% motif_filter_lower
  }))
  domain_df[keep, , drop = FALSE]
}

.normalize_character_filter <- function(x, arg, allow_all = FALSE) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.list(x)) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
  }
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0L) {
    return(NULL)
  }
  if (allow_all && length(x) == 1L && identical(to_lower_ascii(x), "all")) {
    return(NULL)
  }
  if (allow_all && any(to_lower_ascii(x) == "all")) {
    stop(
      sprintf("`%s` cannot mix \"all\" with explicit values.", arg),
      call. = FALSE
    )
  }
  unique(x)
}

.resolve_projected_model_levels <- function(model_values, model = "all") {
  explicit_models <- .normalize_character_filter(model, arg = "model", allow_all = TRUE)
  if (!is.null(explicit_models)) {
    return(explicit_models)
  }
  unique(as.character(model_values))
}

.domain_projection_candidates <- function(gr) {
  meta <- S4Vectors::mcols(gr)
  data.frame(
    transcript_id = .coalesce_character_cols(meta, c("transcript_id", "ID", "Parent")),
    gene_id = .coalesce_character_cols(meta, c("gene_id", "ID", "Name")),
    gene_name = .coalesce_character_cols(meta, c("plot_label", "gene_name", "Name", "gene_id")),
    stringsAsFactors = FALSE
  ) |>
    tidyr::pivot_longer(
      cols = c("transcript_id", "gene_id", "gene_name"),
      names_to = "match_type",
      values_to = "match_key"
    ) |>
    dplyr::filter(!is.na(.data$match_key) & nzchar(.data$match_key)) |>
    dplyr::distinct()
}

.domain_projection_transcript_meta <- function(cds_gr) {
  meta <- S4Vectors::mcols(cds_gr)
  data.frame(
    transcript_id = .coalesce_character_cols(meta, c("transcript_id", "Parent", "ID")),
    gene_id = .coalesce_character_cols(meta, c("gene_id", "ID")),
    gene_name = .coalesce_character_cols(meta, c("plot_label", "gene_name", "Name", "gene_id")),
    stringsAsFactors = FALSE
  ) |>
    dplyr::filter(!is.na(.data$transcript_id) & nzchar(.data$transcript_id)) |>
    dplyr::distinct()
}

.project_one_domain_row <- function(domain_row,
                                    key_col,
                                    text_col,
                                    transcript_meta,
                                    cds_gr) {
  key_value <- as.character(domain_row[[key_col]][[1L]])
  if (is.na(key_value) || !nzchar(key_value)) {
    return(data.frame())
  }

  tx_hits <- transcript_meta$transcript_id[
    transcript_meta$transcript_id == key_value |
      transcript_meta$gene_id == key_value |
      transcript_meta$gene_name == key_value
  ]
  tx_hits <- unique(tx_hits[!is.na(tx_hits) & nzchar(tx_hits)])
  if (length(tx_hits) == 0L) {
    return(data.frame())
  }

  aa_start <- as.integer(domain_row$start[[1L]])
  aa_end <- as.integer(domain_row$end[[1L]])
  if (is.na(aa_start) || is.na(aa_end) || aa_start > aa_end) {
    return(data.frame())
  }

  domain_label <- as.character(domain_row[[text_col]][[1L]])
  domain_id <- if ("domain" %in% colnames(domain_row)) {
    as.character(domain_row$domain[[1L]])
  } else {
    domain_label
  }
  model_name <- if ("analysis" %in% colnames(domain_row)) {
    as.character(domain_row$analysis[[1L]])
  } else {
    "unknown"
  }

  dplyr::bind_rows(lapply(tx_hits, function(tx_id) {
    tx_gr <- cds_gr[.coalesce_character_cols(
      S4Vectors::mcols(cds_gr),
      c("transcript_id", "Parent", "ID")
    ) == tx_id]
    .project_domain_to_transcript(
      tx_gr = tx_gr,
      aa_start = aa_start,
      aa_end = aa_end,
      transcript_id = tx_id,
      text = domain_label %||% domain_id,
      motif = domain_label %||% domain_id,
      domain_id = domain_id,
      model = model_name
    )
  }))
}

# Map an amino-acid interval onto genomic coordinates using a transcript's CDS
# structure. Returns one row per CDS segment the interval overlaps, so a codon
# (or domain) spanning a splice junction yields multiple genomic segments.
#
# `cds_phase` accounts for a 5'-truncated coding sequence: it is the GFF phase
# of the 5'-most CDS segment (the number of bases trimmed before the first
# complete codon, 0-2). When `NULL` it is read from a `phase`/`frame` metadata
# column on `tx_gr`, defaulting to 0 -- which leaves complete, phase-0 gene
# models unchanged. Amino-acid positions are interpreted relative to the protein
# obtained by translating from the first complete codon.
.aa_interval_to_genome <- function(tx_gr, aa_start, aa_end, cds_phase = NULL) {
  if (length(tx_gr) == 0L) {
    return(data.frame())
  }
  aa_start <- as.integer(aa_start)
  aa_end <- as.integer(aa_end)
  if (is.na(aa_start) || is.na(aa_end) || aa_start > aa_end) {
    return(data.frame())
  }

  strand_value <- unique(as.character(BiocGenerics::strand(tx_gr)))
  strand_value <- strand_value[!is.na(strand_value)]
  if (length(strand_value) != 1L) {
    return(data.frame())
  }
  strand_value <- strand_value[[1L]]

  if (strand_value == "-") {
    order_idx <- order(IRanges::start(tx_gr), decreasing = TRUE)
  } else {
    order_idx <- order(IRanges::start(tx_gr))
  }
  tx_gr <- tx_gr[order_idx]

  start_phase <- .resolve_cds_start_phase(tx_gr, cds_phase)

  seg_width <- IRanges::width(tx_gr)
  tx_nt_start <- cumsum(c(1L, head(seg_width, -1L)))
  tx_nt_end <- cumsum(seg_width)
  nt_start <- (aa_start - 1L) * 3L + 1L + start_phase
  nt_end <- aa_end * 3L + start_phase

  overlaps <- pmax(tx_nt_start, nt_start) <= pmin(tx_nt_end, nt_end)
  if (!any(overlaps)) {
    return(data.frame())
  }

  out <- lapply(which(overlaps), function(i) {
    overlap_start <- max(tx_nt_start[[i]], nt_start)
    overlap_end <- min(tx_nt_end[[i]], nt_end)

    if (strand_value == "-") {
      genome_start <- IRanges::end(tx_gr)[[i]] - (overlap_end - tx_nt_start[[i]])
      genome_end <- IRanges::end(tx_gr)[[i]] - (overlap_start - tx_nt_start[[i]])
    } else {
      genome_start <- IRanges::start(tx_gr)[[i]] + (overlap_start - tx_nt_start[[i]])
      genome_end <- IRanges::start(tx_gr)[[i]] + (overlap_end - tx_nt_start[[i]])
    }

    data.frame(
      seqnames = as.character(GenomeInfoDb::seqnames(tx_gr))[i],
      xmin = min(genome_start, genome_end),
      xmax = max(genome_start, genome_end),
      strand = strand_value,
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(out)
}

# Phase of the 5'-most CDS segment (after `tx_gr` is ordered 5'->3'). An explicit
# `cds_phase` wins; otherwise read a `phase`/`frame` column, treating "." / NA
# as 0 so complete models behave as before.
.resolve_cds_start_phase <- function(tx_gr, cds_phase = NULL) {
  if (!is.null(cds_phase)) {
    ph <- suppressWarnings(as.integer(cds_phase)[[1L]])
    return(if (is.na(ph)) 0L else ph %% 3L)
  }
  meta <- S4Vectors::mcols(tx_gr)
  col <- intersect(c("phase", "frame"), colnames(meta))
  if (length(col) == 0L) {
    return(0L)
  }
  ph <- suppressWarnings(as.integer(meta[[col[[1L]]]][[1L]]))
  if (is.na(ph)) 0L else ph %% 3L
}

.project_domain_to_transcript <- function(tx_gr,
                                          aa_start,
                                          aa_end,
                                          transcript_id,
                                          text,
                                          motif,
                                          domain_id,
                                          model) {
  segs <- .aa_interval_to_genome(tx_gr, aa_start = aa_start, aa_end = aa_end)
  if (nrow(segs) == 0L) {
    return(data.frame())
  }

  data.frame(
    seqnames = segs$seqnames,
    xmin = segs$xmin,
    xmax = segs$xmax,
    strand = segs$strand,
    transcripts = transcript_id,
    model = model,
    motif = motif,
    domain_id = domain_id,
    text = text,
    stringsAsFactors = FALSE
  )
}

is_comparative_syn_request <- function(species = NULL, reference = NULL) {
  length(species %||% character()) > 1L || !is.null(reference)
}

syn_to_comparative_annotation_df <- function(x,
                                             species,
                                             reference,
                                             chr,
                                             subset,
                                             alignment = NULL,
                                             geom = c("exon", "gene"),
                                             annotation_type = "exon") {
  geom <- match.arg(geom)
  context <- resolve_syn_comparative_context(
    x = x,
    species = species,
    reference = reference,
    chr = chr,
    subset = subset,
    alignment = alignment
  )

  if (geom == "exon") {
    out <- dplyr::bind_rows(lapply(names(context$annotations), function(species_name) {
      syn_gr_to_exon_df(
        feature_gr = context$annotations[[species_name]],
        track = species_name,
        annotation_type = annotation_type
      )
    }))
  } else {
    out <- dplyr::bind_rows(lapply(names(context$annotations), function(species_name) {
      syn_gr_to_gene_df(
        feature_gr = context$annotations[[species_name]],
        track = species_name
      )
    }))
  }

  out <- syn_flatten_annotation_rows(out)
  if (methods::is(x, "SynSpecies")) {
    out <- .inject_homology_columns(out, homology_annotations(x))
  }

  attr(out, "syn_layout_override") <- context$layout
  out
}

syn_flatten_annotation_rows <- function(data, baseline = 1) {
  if (!is.data.frame(data) || nrow(data) == 0L || !"track" %in% names(data) || !"ymin" %in% names(data)) {
    return(data)
  }

  data$ymin <- baseline
  data
}

syn_to_comparative_link_df <- function(x,
                                       species,
                                       reference,
                                       chr,
                                       subset,
                                       alignment = NULL) {
  context <- resolve_syn_comparative_context(
    x = x,
    species = species,
    reference = reference,
    chr = chr,
    subset = subset,
    alignment = alignment
  )
  context$links
}

comparative_nuclink_layer <- function(species,
                                      reference,
                                      chr,
                                      subset,
                                      alignment = NULL,
                                      na.rm = FALSE) {
  layer(
    data = function(plot_data) {
      syn_to_comparative_link_df(
        x = plot_data,
        species = species,
        reference = reference,
        chr = chr,
        subset = subset,
        alignment = alignment
      )
    },
    mapping = ggplot2::aes(
      tspecies = tspecies,
      tchr = tchr,
      tstart = tstart,
      tend = tend,
      strand = strand,
      qspecies = qspecies,
      qchr = qchr,
      qstart = qstart,
      qend = qend,
      target_anchor_y = target_anchor_y,
      query_anchor_y = query_anchor_y
    ),
    geom = GeomNucLink,
    stat = "identity",
    position = "identity",
    show.legend = NA,
    inherit.aes = FALSE,
    layer_class = LayerSyn,
    params = list(na.rm = na.rm)
  )
}

resolve_syn_comparative_context <- function(x,
                                            species,
                                            reference,
                                            chr,
                                            subset,
                                            alignment = NULL) {
  if (!methods::is(x, "SynSpecies")) {
    cli::cli_abort("Comparative plotting requires a {.cls SynSpecies} object.")
  }

  if (length(species %||% character()) != 2L) {
    cli::cli_abort("Comparative plotting expects {.arg species} to contain exactly two individuals.")
  }
  species <- unique(as.character(species))
  if (length(species) != 2L) {
    cli::cli_abort("Comparative plotting expects two unique entries in {.arg species}.")
  }
  if (is.null(reference) || !reference %in% species) {
    cli::cli_abort("{.arg reference} must be one of the requested {.arg species}.")
  }
  if (is.null(chr)) {
    cli::cli_abort("Comparative plotting requires {.arg chr} on the reference species.")
  }
  if (!is.numeric(subset) || length(subset) != 2L) {
    cli::cli_abort("Comparative plotting requires {.arg subset} as a numeric vector of length 2.")
  }

  out <- subset_synspecies_window(
    x = x,
    reference_species = reference,
    chr = chr,
    start = min(subset),
    end = max(subset),
    alignment = alignment
  )

  if (!setequal(names(out$annotations), species)) {
    cli::cli_abort(
      "Selected alignment returns individuals {.val {names(out$annotations)}}, which do not match requested {.arg species} {.val {species}}."
    )
  }

  partner <- setdiff(species, reference)[1L]
  annotations <- out$annotations[c(reference, partner)]
  link_track <- unique(as.character(out$links$track))
  if (length(link_track) != 1L) {
    cli::cli_abort("Expected exactly one link track in the comparative subset.")
  }

  layout <- data.frame(
    PANEL = c(1L, 2L, 3L),
    ROW = c(1L, 2L, 3L),
    COL = c(1L, 1L, 1L),
    track = c(reference, link_track, partner),
    panel_type = c("annotation", "link", "annotation"),
    species = c(reference, NA_character_, partner),
    alignment_name = c(NA_character_, sub("^link_", "", link_track), NA_character_),
    tspecies = c(NA_character_, unique(as.character(out$links$tspecies))[1L], NA_character_),
    qspecies = c(NA_character_, unique(as.character(out$links$qspecies))[1L], NA_character_),
    stringsAsFactors = FALSE
  )
  layout <- .finalize_synspecies_layout_scales(layout, free = list(x = FALSE, y = TRUE))

  top_species <- reference
  bottom_species <- partner
  links <- out$links
  links$target_anchor_y <- if (unique(as.character(links$tspecies))[1L] == top_species) 1 else 0
  links$query_anchor_y <- if (unique(as.character(links$qspecies))[1L] == top_species) 1 else 0
  links$track <- link_track

  list(
    annotations = annotations,
    links = links,
    layout = layout,
    top_species = top_species,
    bottom_species = bottom_species
  )
}

syn_to_exon_df <- function(x,
                           species = NULL,
                           chr = NULL,
                           subset = NULL,
                           annotation_type = "exon",
                           context = NULL) {
  requested_species <- species
  species <- resolve_context_species_params(x, species, context)
  if (is.null(requested_species) && methods::is(x, "SynSpecies")) {
    context_species <- names(context$windows %||% list())
    context_species <- context_species[!is.na(context_species) & nzchar(context_species)]
    if (length(context_species) > 0L) {
      species <- unique(c(species, context_species))
    }
  }

  if (methods::is(x, "SynSpecies") && length(species %||% character()) > 1L) {
    species <- unique(as.character(species))
    return(dplyr::bind_rows(lapply(species, function(species_name) {
      syn_to_exon_df(
        x = x,
        species = species_name,
        chr = chr,
        subset = subset,
        annotation_type = annotation_type,
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

  if (is.null(individual)) {
    blank_window <- .context_window_for_track(context, species)
    return(blank_syn_exon_df(track = species, window = blank_window, annotation_type = annotation_type))
  }
  if (!has_syn_annotation_source(individual)) {
    blank_window <- .context_window_for_track(context, species) %||%
      .context_window_for_track(context, syn_id(individual))
    return(blank_syn_exon_df(
      track = species,
      window = blank_window,
      annotation_type = annotation_type
    ))
  }

  window <- normalize_syn_window_request(
    x = x,
    species = species,
    chr = chr,
    subset = subset,
    allow_missing_subset = TRUE,
    context = context,
    geom = "geom_exon"
  )

  feature_type <- if (is.null(annotation_type) ||
                      identical(annotation_type, "exon") ||
                      identical(annotation_type, "all")) {
    NULL
  } else {
    annotation_type
  }

  feature_gr <- query_features(
    individual,
    chr = window$chr,
    start = window$start,
    end = window$end,
    feature_type = feature_type,
    all = is_unrestricted_syn_window(window)
  )

  if (length(feature_gr) == 0L) {
    return(data.frame())
  }

  result <- syn_gr_to_exon_df(
    feature_gr = feature_gr,
    track = species,
    annotation_type = annotation_type
  )
  if (methods::is(x, "SynSpecies")) {
    result <- .inject_homology_columns(result, homology_annotations(x))
  }
  result
}

blank_syn_exon_df <- function(track, window = NULL, annotation_type = "exon") {
  if (is.null(window) || is.null(window$start) || is.null(window$end)) {
    return(data.frame())
  }

  placeholder_id <- paste0("__blank__", track)
  data.frame(
    seqnames = window$chr[[1L]] %||% NA_character_,
    xmin = as.integer(window$start[[1L]]),
    xmax = as.integer(window$end[[1L]]),
    strand = "+",
    type = annotation_type,
    transcript_id = placeholder_id,
    transcripts = placeholder_id,
    gene_id = NA_character_,
    gene_name = NA_character_,
    track = track,
    fill = NA_character_,
    linetype = 0,
    linewidth = 0,
    alpha = 0,
    ymin = 2,
    group = 1L,
    PANEL = 1L,
    blank_panel = TRUE,
    stringsAsFactors = FALSE
  )
}

#' Convert genomic features to a `geom_exon()` data frame
#'
#' Turns a `GRanges` annotation subset into the rectangular feature table used by
#' [`geom_exon()`]. The returned data always includes a canonical identifier set
#' for aesthetic mappings:
#'
#' - `transcript_id`: normalized transcript-level identifier
#' - `gene_id`: normalized gene-level identifier
#' - `gene_name`: display-friendly gene label
#'
#' The existing `transcripts` column is retained because ggexon uses it
#' internally for grouping and track layout.
#'
#' @param feature_gr A `GRanges` object containing exon-like annotation features.
#' @param track Track label written into the output table.
#' @param annotation_type Feature type to keep. Defaults to `"exon"`. When
#'   `"exon"`, CDS rows are used only as a fallback for transcripts that do not
#'   already have explicit exon records.
#'
#' @return A `data.frame` ready for [`geom_exon()`] with positional columns plus
#'   canonical identifier columns such as `transcript_id`, `gene_id`, and
#'   `gene_name`.
#' @keywords internal
syn_gr_to_exon_df <- function(feature_gr,
                              track,
                              annotation_type = "exon") {
  meta <- S4Vectors::mcols(feature_gr)
  types <- as.character(meta$type)
  transcript_ids <- .coalesce_character_cols(
    meta,
    c("transcript_id", "Parent", "ID", "gene_id", "gene_name")
  )

  if (is.null(annotation_type) || identical(annotation_type, "all")) {
    keep_rows <- types %in% c(
      "exon", "CDS", "five_prime_UTR", "three_prime_UTR",
      "5UTR", "3UTR", "five_prime_utr", "three_prime_utr",
      "UTR", "utr"
    )
    feature_gr <- feature_gr[keep_rows]
    meta <- S4Vectors::mcols(feature_gr)
    types <- as.character(meta$type)
    transcript_ids <- .coalesce_character_cols(
      meta,
      c("transcript_id", "Parent", "ID", "gene_id", "gene_name")
    )
  } else if (identical(annotation_type, "exon")) {
    transcripts_with_exons <- unique(transcript_ids[types == "exon" & !is.na(transcript_ids)])
    keep_rows <- types == "exon" |
      (types == "CDS" &
         !is.na(transcript_ids) &
         nzchar(transcript_ids) &
         !(transcript_ids %in% transcripts_with_exons))
    feature_gr <- feature_gr[keep_rows]
    meta <- S4Vectors::mcols(feature_gr)
    types <- as.character(meta$type)
    transcript_ids <- .coalesce_character_cols(
      meta,
      c("transcript_id", "Parent", "ID", "gene_id", "gene_name")
    )
    types[types == "CDS"] <- "exon"
  } else {
    feature_gr <- feature_gr[types == annotation_type]
    meta <- S4Vectors::mcols(feature_gr)
    types <- as.character(meta$type)
    transcript_ids <- .coalesce_character_cols(
      meta,
      c("transcript_id", "Parent", "ID", "gene_id", "gene_name")
    )
  }

  if (length(feature_gr) == 0L) {
    return(data.frame())
  }

  transcript_ids[is.na(transcript_ids) | !nzchar(transcript_ids)] <- paste0(
    "feature_", seq_along(transcript_ids)
  )

  gene_labels <- .coalesce_character_cols(
    meta,
    c("plot_label", "gene_name", "gene_id", "Name", "ID")
  )
  gene_ids <- .coalesce_character_cols(
    meta,
    c("gene_id", "gene_name", "Name", "ID", "Parent")
  )

  order_df <- data.frame(
    transcripts = transcript_ids,
    xmin = IRanges::start(feature_gr),
    stringsAsFactors = FALSE
  )
  order_df <- stats::aggregate(xmin ~ transcripts, data = order_df, FUN = min)
  order_df <- order_df[order(order_df$xmin, order_df$transcripts), , drop = FALSE]
  order_df$ymin <- rev(seq_len(nrow(order_df))) * 2
  order_df$group <- seq_len(nrow(order_df))

  out <- data.frame(
    seqnames = as.character(GenomeInfoDb::seqnames(feature_gr)),
    xmin = IRanges::start(feature_gr),
    xmax = IRanges::end(feature_gr),
    strand = as.character(BiocGenerics::strand(feature_gr)),
    type = types,
    transcript_id = transcript_ids,
    transcripts = transcript_ids,
    gene_id = gene_ids,
    gene_name = gene_labels,
    track = track,
    fill = "black",
    linetype = 1,
    linewidth = 0,
    alpha = NA_real_,
    stringsAsFactors = FALSE
  )

  out <- merge(
    out,
    order_df[, c("transcripts", "ymin", "group")],
    by = "transcripts",
    all.x = TRUE,
    sort = FALSE
  )
  out <- out[order(match(out$transcripts, transcript_ids), out$xmin, out$xmax), , drop = FALSE]
  out$PANEL <- 1L
  rownames(out) <- NULL
  out
}

syn_to_gene_df <- function(x,
                           species = NULL,
                           chr = NULL,
                           subset = NULL,
                           context = NULL) {
  species <- resolve_context_species_params(x, species, context)

  if (methods::is(x, "SynSpecies") && length(species %||% character()) > 1L) {
    species <- unique(as.character(species))
    return(dplyr::bind_rows(lapply(species, function(species_name) {
      syn_to_gene_df(
        x = x,
        species = species_name,
        chr = chr,
        subset = subset,
        context = context
      )
    })))
  }

  individual_name <- .context_individual_for_track(x, species, context = context)
  individual <- resolve_syn_individual(x, species = individual_name)
  if (!has_syn_annotation_source(individual)) {
    return(data.frame())
  }
  window <- normalize_syn_window_request(
    x = x,
    species = species,
    chr = chr,
    subset = subset,
    allow_missing_subset = TRUE,
    context = context,
    geom = "geom_gene"
  )

  feature_gr <- query_features(
    individual,
    chr = window$chr,
    start = window$start,
    end = window$end,
    feature_type = "gene",
    all = is_unrestricted_syn_window(window)
  )

  if (length(feature_gr) == 0L) {
    return(data.frame())
  }

  result <- syn_gr_to_gene_df(
    feature_gr = feature_gr,
    track = species
  )
  if (methods::is(x, "SynSpecies")) {
    result <- .inject_homology_columns(result, homology_annotations(x))
  }
  result
}

syn_gr_to_gene_df <- function(feature_gr, track) {

  meta <- S4Vectors::mcols(feature_gr)
  gene_ids <- .coalesce_character_cols(
    meta,
    c("gene_id", "gene_name", "ID", "Name")
  )
  gene_ids[is.na(gene_ids) | !nzchar(gene_ids)] <- paste0("gene_", seq_len(length(gene_ids)))[
    is.na(gene_ids) | !nzchar(gene_ids)
  ]

  gene_labels <- .coalesce_character_cols(
    meta,
    c("plot_label", "gene_name", "gene_id", "Name", "ID")
  )

  if (length(feature_gr) == 0L) {
    return(data.frame())
  }

  gene_df <- data.frame(
    gene_id = gene_ids,
    seqnames = as.character(GenomeInfoDb::seqnames(feature_gr)),
    xmin = IRanges::start(feature_gr),
    xmax = IRanges::end(feature_gr),
    strand = as.character(BiocGenerics::strand(feature_gr)),
    gene_name = gene_labels,
    stringsAsFactors = FALSE
  )
  gene_df$gene_name[is.na(gene_df$gene_name) | !nzchar(gene_df$gene_name)] <- gene_df$gene_id[
    is.na(gene_df$gene_name) | !nzchar(gene_df$gene_name)
  ]
  gene_df$label <- gene_df$gene_name
  gene_df <- gene_df[order(gene_df$xmin, gene_df$gene_id), , drop = FALSE]
  gene_df$ymin <- 0
  gene_df$group <- seq_len(nrow(gene_df))
  gene_df$transcripts <- gene_df$gene_id
  gene_df$track <- track
  gene_df$fill <- "black"
  gene_df$linetype <- 1
  gene_df$linewidth <- 0
  gene_df$alpha <- NA_real_
  gene_df$PANEL <- 1L
  rownames(gene_df) <- NULL
  gene_df
}

syn_to_nuclink_df <- function(x,
                              alignment = NULL,
                              reference = NULL,
                              chr = NULL,
                              subset = NULL,
                              filter_by_len = NULL,
                              context = NULL) {
  if (!methods::is(x, "SynSpecies")) {
    cli::cli_abort("{.fn geom_nuclink} with implicit Syn data requires a {.cls SynSpecies} object.")
  }

  species_order <- infer_nuclink_species_order(
    x,
    context,
    reference_species = reference,
    alignment = alignment,
    filter_by_len = filter_by_len
  )

  if (!is.null(chr) || !is.null(subset)) {
    if (is.null(reference) || is.null(chr) || is.null(subset)) {
      cli::cli_abort("Provide {.arg reference}, {.arg chr}, and {.arg subset} together when subsetting {.fn geom_nuclink}.")
    }
    if (!is.numeric(subset) || length(subset) != 2L) {
      cli::cli_abort("{.arg subset} must be a numeric vector of length 2 for {.fn geom_nuclink}.")
    }

    out <- subset_synspecies_window(
      x = x,
      reference_species = reference,
      chr = chr,
      start = min(subset),
      end = max(subset),
      alignment = alignment,
      selected_species = species_order,
      filter_by_len = filter_by_len
    )

    return(out$links)
  }

  pairs <- resolve_plot_link_alignments(
    x,
    alignment,
    species_order = species_order,
    reference_species = reference,
    filter_by_len = filter_by_len
  )
  use_context_windows <- length(pairs) == 1L
  link_data <- lapply(pairs, function(pair) {
    pair_species <- alignment_individuals(pair)
    pair_windows <- if (!is.null(context)) context$windows[pair_species] else list()
    if (isTRUE(use_context_windows) &&
        length(pair_windows) == length(pair_species) &&
        all(!vapply(pair_windows, is.null, logical(1)))) {
      subset_regions <- vapply(pair_windows, window_to_region_string, character(1))
      return(pairwise_alignment_data(pair, subset = subset_regions))
    }
    pairwise_alignment_data(pair)
  })
  dplyr::bind_rows(link_data)
}

resolve_syn_seqname <- function(individual, chr = NULL) {
  if (is.null(chr)) {
    return(NULL)
  }

  tryCatch(
    .resolve_annotation_seqname(individual, chr = chr),
    error = function(cnd) {
      cli::cli_abort(conditionMessage(cnd))
    }
  )
}

resolve_syn_seqname_or_raw <- function(individual, chr = NULL) {
  if (is.null(chr)) {
    return(NULL)
  }
  if (has_syn_annotation_source(individual)) {
    return(resolve_syn_seqname(individual, chr))
  }
  chr
}

has_syn_annotation_source <- function(individual) {
  if (!methods::is(individual, "SynIndividual")) {
    return(FALSE)
  }
  if (!is.null(annotation_data(individual))) {
    return(TRUE)
  }

  annotation_paths <- annotation_file(individual)
  length(annotation_paths) > 0L &&
    !(length(annotation_paths) == 1L && is.na(annotation_paths[[1L]])) &&
    all(!is.na(annotation_paths)) &&
    all(nzchar(annotation_paths))
}

#' Resolve one individual from Syn-backed plot input
#'
#' Normalizes Syn plotting inputs so downstream layer helpers can work with a
#' single [`SynIndividual`] object. When `x` is already a `SynIndividual`, the
#' function returns it unchanged after optionally checking that `species`
#' matches its identifier. When `x` is a [`SynSpecies`] collection, the helper
#' selects one stored individual by name.
#'
#' This function is mainly used inside Syn-aware geoms and query helpers that
#' allow users to supply either a whole `SynSpecies` object or an already
#' selected `SynIndividual`.
#'
#' @param x A [`SynSpecies`] or [`SynIndividual`] object.
#' @param species Optional individual identifier. When `x` is a `SynSpecies`
#'   with more than one stored individual, this argument is required.
#'
#' @return A single [`SynIndividual`] object.
#'
#' @details
#' The helper throws an error when:
#'
#' - `x` is neither a `SynSpecies` nor a `SynIndividual`
#' - the supplied `SynSpecies` has no individuals
#' - `species` is omitted for a `SynSpecies` that stores multiple individuals
#' - `species` does not match any stored individual
#' - `species` is supplied for a `SynIndividual` but does not match
#'
#' @keywords internal
resolve_syn_individual <- function(x, species = NULL) {
  if (methods::is(x, "SynIndividual")) {
    if (!is.null(species) && !identical(syn_id(x), species)) {
      cli::cli_abort(
        "Requested {.val {species}} but the supplied {.cls SynIndividual} is {.val {syn_id(x)}}."
      )
    }
    return(x)
  }

  if (!methods::is(x, "SynSpecies")) {
    cli::cli_abort("Expected a {.cls SynSpecies} or {.cls SynIndividual} object.")
  }

  all_individuals <- individuals(x)
  if (length(all_individuals) == 0L) {
    cli::cli_abort("The {.cls SynSpecies} object does not contain any individuals.")
  }

  if (is.null(species)) {
    if (length(all_individuals) != 1L) {
      cli::cli_abort(
        "Use {.arg species} to select one individual from the {.cls SynSpecies} object."
      )
    }
    return(all_individuals[[1L]])
  }

  if (!species %in% names(all_individuals)) {
    cli::cli_abort(
      "Unknown species {.val {species}}. Available individuals: {.val {names(all_individuals)}}."
    )
  }

  all_individuals[[species]]
}
