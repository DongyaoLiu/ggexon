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
  }
)

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
    stringsAsFactors = FALSE
  )

  out <- merge(out, order_df[, c("transcripts", "ymin", "group")],
               by = "transcripts", all.x = TRUE, sort = FALSE)
  out <- out[order(match(out$transcripts, transcript_ids), out$xmin, out$xmax), , drop = FALSE]
  rownames(out) <- NULL
  out
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
