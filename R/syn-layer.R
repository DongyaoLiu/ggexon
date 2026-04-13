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

  character()
}

syn_identity_mapping <- function(cols) {
  if (length(cols) == 0L) {
    return(ggplot2::aes())
  }

  exprs <- stats::setNames(lapply(cols, rlang::sym), cols)
  rlang::inject(ggplot2::aes(!!!exprs))
}

resolve_syn_layer_data <- function(x, layer) {
  if (identical(layer$geom, GeomExon)) {
    params <- utils::modifyList(layer$geom$default_params(), layer$geom_params)
    return(
      syn_to_exon_df(
        x = x,
        species = params$species,
        chr = params$chr,
        subset = params$subset,
        annotation_type = params$annotation_type
      )
    )
  }

  geom_name <- class(layer$geom)[1] %||% ""
  cli::cli_abort(
    "Syn object input is not yet implemented for geom {.val {geom_name}}."
  )
}

syn_to_exon_df <- function(x,
                           species = NULL,
                           chr = NULL,
                           subset = NULL,
                           annotation_type = "exon") {
  individual <- resolve_syn_individual(x, species = species)
  chr <- resolve_syn_seqname(individual, chr)

  start <- end <- NULL
  if (!is.null(subset)) {
    if (!is.numeric(subset) || length(subset) != 2L) {
      cli::cli_abort("{.arg subset} must be a numeric vector of length 2.")
    }
    start <- min(subset)
    end <- max(subset)
  }

  feature_gr <- query_features(
    individual,
    chr = chr,
    start = start,
    end = end,
    feature_type = annotation_type
  )

  if (length(feature_gr) == 0L) {
    return(data.frame())
  }

  meta <- S4Vectors::mcols(feature_gr)
  transcript_ids <- .coalesce_character_cols(
    meta,
    c("transcript_id", "Parent", "ID", "gene_id", "gene_name")
  )
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
    type = as.character(meta$type),
    transcripts = transcript_ids,
    gene_name = gene_labels,
    track = syn_id(individual),
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
