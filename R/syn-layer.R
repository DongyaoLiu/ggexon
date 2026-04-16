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

    if (is_syn_layer_input(self, plot@data) &&
        length(self$mapping) == 0L &&
        length(plot@mapping) == 0L) {
    self$mapping <- syn_default_mapping(data, self)
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
  if (identical(layer$geom, GeomExon)) {
    cols <- c("xmin", "xmax", "ymin", "transcripts", "strand", "track", "type", "group")
    return(intersect(cols, names(data)))
  }
  if (identical(layer$geom, GeomGene)) {
    cols <- c("xmin", "xmax", "ymin", "transcripts", "strand", "track", "group")
    return(intersect(cols, names(data)))
  }
  if (identical(layer$geom, GeomGeneLabel)) {
    cols <- c("xmin", "xmax", "ymin", "transcripts", "strand", "track", "label", "group")
    return(intersect(cols, names(data)))
  }
  if (identical(layer$geom, GeomNucLink)) {
    cols <- c(
      "tspecies", "tchr", "tstart", "tend", "strand",
      "qspecies", "qchr", "qstart", "qend", "group", "track", "target_anchor_y", "query_anchor_y"
    )
    return(intersect(cols, names(data)))
  }

  character()
}

syn_identity_mapping <- function(cols) {
  if (length(cols) == 0L) {
    return(ggplot2::aes())
  }

  exprs <- stats::setNames(lapply(cols, rlang::sym), cols)
  rlang::inject(ggplot2::aes(!!!exprs))
}

collect_syn_plot_context <- function(layers, plot_data) {
  syn_data <- find_syn_plot_data(layers, plot_data)
  if (is.null(syn_data)) {
    return(NULL)
  }

  annotation_requests <- unlist(lapply(layers, function(layer) {
    collect_syn_annotation_requests(layer, syn_data, plot_data)
  }), recursive = FALSE)
  link_requests <- unlist(lapply(layers, function(layer) {
    collect_syn_link_requests(layer, syn_data, plot_data)
  }), recursive = FALSE)

  windows <- collect_explicit_annotation_windows(annotation_requests, syn_data)
  windows <- derive_syn_plot_windows(syn_data, windows, link_requests)

  list(
    syn_data = syn_data,
    annotation_requests = annotation_requests,
    link_requests = link_requests,
    windows = windows
  )
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
        identical(layer$geom, GeomGene) ||
        identical(layer$geom, GeomGeneLabel))) {
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
  pair <- tryCatch(
    resolve_plot_pairwise_alignment(syn_data, alignment),
    error = function(...) NULL
  )

  list(list(
    alignment = alignment,
    pair = pair,
    reference = params$reference,
    chr = params$chr,
    subset = params$subset
  ))
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
    y_scale = layout@y_scale,
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
    if (length(individuals(x)) == 1L) {
      return(names(individuals(x)))
    }
    return(character())
  }

  unique(as.character(species))
}

resolve_context_species_params <- function(x, species = NULL, context = NULL) {
  explicit_species <- resolve_plot_species_params(x, species)
  if (length(explicit_species) > 0L) {
    return(explicit_species)
  }

  if (!methods::is(x, "SynSpecies")) {
    return(explicit_species)
  }

  context_windows <- context$windows %||% list()
  context_species <- names(context_windows)
  context_species <- context_species[!is.na(context_species) & nzchar(context_species)]
  context_species <- unique(context_species)
  if (length(context_species) == 0L) {
    return(NULL)
  }
  context_species
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

  alignment
}

resolve_plot_pairwise_alignment <- function(x, alignment = NULL) {
  if (!methods::is(x, "SynSpecies")) {
    cli::cli_abort("Plot-derived link windows require a {.cls SynSpecies} object.")
  }

  pair_list <- pairwise_alignments(x)
  if (length(pair_list) == 0L) {
    cli::cli_abort("The {.cls SynSpecies} object does not contain any pairwise alignments.")
  }

  if (is.null(alignment)) {
    if (length(pair_list) == 1L) {
      return(pair_list[[1L]])
    }
    cli::cli_abort("Supply {.arg alignment} when multiple pairwise alignments are available.")
  }

  if (!alignment %in% names(pair_list)) {
    cli::cli_abort(
      "Unknown alignment {.val {alignment}}. Available pairwise alignments: {.val {names(pair_list)}}."
    )
  }

  pair_list[[alignment]]
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

derive_syn_plot_windows <- function(x, windows, link_requests) {
  if (!methods::is(x, "SynSpecies") || length(link_requests) == 0L) {
    return(windows)
  }

  for (request in link_requests) {
    pair <- request$pair
    if (is.null(pair)) {
      next
    }

    pair_species <- alignment_individuals(pair)
    if (all(pair_species %in% names(windows))) {
      next
    }

    if (!is.null(request$reference) || !is.null(request$chr) || !is.null(request$subset)) {
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
        alignment = request$alignment
      )
      windows <- utils::modifyList(windows, out$windows)
      next
    }

    available <- intersect(pair_species, names(windows))
    if (length(available) != 1L) {
      next
    }

    reference_window <- windows[[available[[1L]]]]
    out <- subset_synspecies_window(
      x = x,
      reference_species = available[[1L]],
      chr = reference_window$chr,
      start = reference_window$start,
      end = reference_window$end,
      alignment = alignment_name(pair)
    )
    windows <- utils::modifyList(windows, out$windows)
  }

  windows
}

normalize_syn_window_request <- function(x,
                                         species,
                                         chr = NULL,
                                         subset = NULL,
                                         allow_missing_subset = TRUE,
                                         context = NULL,
                                         geom = "annotation") {
  individual <- resolve_syn_individual(x, species = species)

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
      chr = resolve_syn_seqname(individual, chr),
      start = min(subset),
      end = max(subset)
    ))
  }

  derived_window <- context$windows[[species]] %||% NULL
  if (!is.null(derived_window)) {
    if (!is.null(chr)) {
      requested_chr <- resolve_syn_seqname(individual, chr)
      if (!identical(requested_chr, derived_window$chr)) {
        cli::cli_abort(
          "Derived window for {.val {species}} is on {.val {derived_window$chr}}, not {.val {requested_chr}}."
        )
      }
    }
    return(derived_window)
  }

  if (allow_missing_subset) {
    return(list(chr = resolve_syn_seqname(individual, chr), start = NULL, end = NULL))
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

resolve_syn_layer_data <- function(x, layer) {
  params <- syn_layer_params(layer)
  context <- layer$syn_plot_context %||% NULL

  if (identical(layer$geom, GeomExon)) {
    return(
      syn_to_exon_df(
        x = x,
        species = params$species,
        chr = params$chr,
        subset = params$subset,
        annotation_type = params$annotation_type,
        context = context
      )
    )
  }
  if (identical(layer$geom, GeomGene)) {
    return(
      syn_to_gene_df(
        x = x,
        species = params$species,
        chr = params$chr,
        subset = params$subset,
        context = context
      )
    )
  }
  if (identical(layer$geom, GeomGeneLabel)) {
    return(
      syn_to_gene_df(
        x = x,
        species = params$species,
        chr = params$chr,
        subset = params$subset,
        context = context
      )
    )
  }
  if (identical(layer$geom, GeomNucLink)) {
    return(
      syn_to_nuclink_df(
        x = x,
        alignment = params$alignment,
        reference = params$reference,
        chr = params$chr,
        subset = params$subset,
        context = context
      )
    )
  }

  geom_name <- class(layer$geom)[1] %||% ""
  cli::cli_abort(
    "Syn object input is not yet implemented for geom {.val {geom_name}}."
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

  individual <- resolve_syn_individual(x, species = species)
  window <- normalize_syn_window_request(
    x = x,
    species = syn_id(individual),
    chr = chr,
    subset = subset,
    allow_missing_subset = FALSE,
    context = context,
    geom = "geom_exon"
  )

  feature_gr <- query_features(
    individual,
    chr = window$chr,
    start = window$start,
    end = window$end,
    feature_type = if (identical(annotation_type, "exon")) NULL else annotation_type
  )

  if (length(feature_gr) == 0L) {
    return(data.frame())
  }

  syn_gr_to_exon_df(
    feature_gr = feature_gr,
    track = syn_id(individual),
    annotation_type = annotation_type
  )
}

syn_gr_to_exon_df <- function(feature_gr,
                              track,
                              annotation_type = "exon") {
  meta <- S4Vectors::mcols(feature_gr)
  types <- as.character(meta$type)
  transcript_ids <- .coalesce_character_cols(
    meta,
    c("transcript_id", "Parent", "ID", "gene_id", "gene_name")
  )

  if (identical(annotation_type, "exon")) {
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
    transcripts = transcript_ids,
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

  individual <- resolve_syn_individual(x, species = species)
  window <- normalize_syn_window_request(
    x = x,
    species = syn_id(individual),
    chr = chr,
    subset = subset,
    allow_missing_subset = FALSE,
    context = context,
    geom = "geom_gene"
  )

  feature_gr <- query_features(
    individual,
    chr = window$chr,
    start = window$start,
    end = window$end,
    feature_type = "gene"
  )

  if (length(feature_gr) == 0L) {
    return(data.frame())
  }

  syn_gr_to_gene_df(
    feature_gr = feature_gr,
    track = syn_id(individual)
  )
}

syn_gr_to_gene_df <- function(feature_gr, track) {

  meta <- S4Vectors::mcols(feature_gr)
  gene_ids <- .coalesce_character_cols(
    meta,
    c("gene_id", "gene_name", "ID", "Name")
  )
  valid_gene <- !is.na(gene_ids) & nzchar(gene_ids)
  feature_gr <- feature_gr[valid_gene]
  meta <- S4Vectors::mcols(feature_gr)
  gene_ids <- gene_ids[valid_gene]

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
  gene_df$ymin <- rev(seq_len(nrow(gene_df))) * 2
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
                              context = NULL) {
  if (!methods::is(x, "SynSpecies")) {
    cli::cli_abort("{.fn geom_nuclink} with implicit Syn data requires a {.cls SynSpecies} object.")
  }

  if (!is.null(reference) || !is.null(chr) || !is.null(subset)) {
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
      alignment = alignment
    )

    return(out$links)
  }

  pair <- resolve_plot_pairwise_alignment(x, alignment)
  pair_species <- alignment_individuals(pair)
  pair_windows <- if (!is.null(context)) context$windows[pair_species] else list()
  if (length(pair_windows) == length(pair_species) &&
      all(!vapply(pair_windows, is.null, logical(1)))) {
    subset_regions <- vapply(pair_windows, window_to_region_string, character(1))
    return(
      pairwise_alignment_data(
        x,
        alignment = alignment_name(pair),
        subset = subset_regions
      )
    )
  }

  pairwise_alignment_data(x, alignment = alignment_name(pair))
}

resolve_syn_seqname <- function(individual, chr = NULL) {
  if (is.null(chr)) {
    return(NULL)
  }

  individual <- load_annotation(individual)
  available <- unique(as.character(GenomeInfoDb::seqnames(annotation_data(individual))))

  if (chr %in% available) {
    return(chr)
  }

  lower_available <- base::tolower(available)
  lower_chr <- base::tolower(chr)
  if (lower_chr %in% lower_available) {
    return(available[match(lower_chr, lower_available)])
  }

  chr_parts <- strsplit(chr, "_", fixed = TRUE)[[1L]]
  if (length(chr_parts) > 1L) {
    swapped <- paste(rev(chr_parts), collapse = "_")
    if (swapped %in% available) {
      return(swapped)
    }
    swapped_lower <- base::tolower(swapped)
    if (swapped_lower %in% lower_available) {
      return(available[match(swapped_lower, lower_available)])
    }
  }

  cli::cli_abort(
    "Unknown chromosome {.val {chr}} for {.val {syn_id(individual)}}. Available seqnames include {.val {utils::head(available, 10)}}."
  )
}

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
