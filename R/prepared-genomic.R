#' Stored genomic projection of a protein annotation
#'
#' Created by [project_mutations_to_genome()] or [project_domains_to_genome()]
#' with `store = TRUE`. Original protein annotations remain attached. Projected
#' fragments and mutation anchors retain their source row and transcript identity.
#' @slot projection_data Genomic fragments and source metadata.
#' @slot anchors One exonic anchor per source mutation and transcript.
#' @slot source_annotation Name of the original protein annotation.
#' @slot feature_kind Either `"mutation"` or `"domain"`.
#' @slot recipe Projection selectors and reference annotation identity.
#' @slot signature Fingerprint used to reject stale projections.
#' @exportClass SynGenomicProjectionAnnotation
setClass("SynGenomicProjectionAnnotation", contains = "SynGenomeAnnotation",
  slots = c(projection_data = "data.frame", anchors = "data.frame",
    source_annotation = "character", feature_kind = "character", recipe = "list",
    signature = "character"),
  prototype = list(annotation_scope = "nucleotide", lazy = FALSE, loaded = TRUE,
    projection_data = data.frame(), anchors = data.frame(), source_annotation = "",
    feature_kind = "mutation", recipe = list(), signature = ""),
  validity = function(object) {
    if (length(object@feature_kind) != 1L || !object@feature_kind %in% c("mutation", "domain"))
      return("`feature_kind` must be mutation or domain.")
    if (length(object@source_annotation) != 1L || !nzchar(object@source_annotation))
      return("`source_annotation` must identify the original annotation.")
    TRUE
  })

.genomic_name <- function(value, arg) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value))
    stop("`", arg, "` must be one non-empty name.", call. = FALSE)
  value
}

.projection_signature <- function(x, source, feature_annotation, recipe = NULL) {
  ann <- get_annotation(x, source)
  reference <- x
  reference <- set_active_feature_annotation(reference, feature_annotation)
  features <- if (!is.null(recipe$transcripts)) {
    as.data.frame(query_features(reference, transcripts = recipe$transcripts))
  } else if (!is.null(recipe$genes)) {
    as.data.frame(query_features(reference, genes = recipe$genes))
  } else as.data.frame(query_features(reference, all = TRUE))
  payload <- if (methods::is(ann, "SynProteinMutationAnnotation")) {
    list(data = query_protein_mutations(ann), index = protein_mutation_individual_index(ann),
      keytype = ann@keytype)
  } else if (methods::is(ann, "SynProteinDomainAnnotation")) {
    list(data = as.data.frame(query_domains(ann)), keytype = ann@keytype)
  } else stop("The source protein annotation changed type; prepare its projection again.", call. = FALSE)
  rlang::hash(list(features = features, payload = payload, recipe = recipe))
}

.find_genomic_projection <- function(x, annotation, kind = NULL) {
  if (is.null(annotation) || !annotation %in% annotation_names(x)) return(NULL)
  ann <- get_annotation(x, annotation)
  if (!methods::is(ann, "SynGenomicProjectionAnnotation")) return(NULL)
  if (!is.null(kind) && !identical(ann@feature_kind, kind))
    stop("Stored projection `", annotation, "` contains ", ann@feature_kind, " features.", call. = FALSE)
  .validate_genomic_projection(x, ann)
  ann
}

.validate_genomic_projection <- function(x, ann) {
  current <- tryCatch(.projection_signature(x, ann@source_annotation,
    ann@recipe$feature_annotation, ann@recipe), error = function(e) NULL)
  if (is.null(current) || !identical(current, ann@signature))
    stop("Stored genomic projection `", annotation_name(ann),
      "` is stale: its protein annotation or reference features changed. Run the projection verb again.",
      call. = FALSE)
  invisible(ann)
}

.finish_genomic_projection <- function(x, out, source, kind, store, name, recipe) {
  if (!is.null(recipe$transcripts) && nrow(out) && "transcripts" %in% names(out))
    out <- out[out$transcripts %in% recipe$transcripts, , drop = FALSE]
  out <- as.data.frame(out)
  rownames(out) <- NULL
  if (!isTRUE(store)) return(out)
  .genomic_name(name, "name")
  if (name %in% annotation_names(x) &&
      !methods::is(get_annotation(x, name), "SynGenomicProjectionAnnotation"))
    stop("Projection name `", name, "` would replace a source annotation.", call. = FALSE)
  if (!nrow(out)) {
    source_ann <- get_annotation(x, source)
    out <- if (kind == "mutation") query_protein_mutations(source_ann)[0, , drop = FALSE] else data.frame()
    out$seqnames <- character()
    out$xmin <- out$xmax <- numeric()
    out$strand <- out$transcripts <- character()
    if (kind == "mutation") out$.ggexon_projection_row <- integer()
    else {
      out$model <- out$motif <- out$text <- out$domain_id <- character()
      out$.ggexon_projection_row <- integer()
    }
  }
  anchors <- out[0, , drop = FALSE]
  anchors$anchor <- numeric()
  if (kind == "mutation" && nrow(out)) {
    identity <- interaction(out$.ggexon_projection_row, out$transcripts, drop = TRUE)
    picks <- vapply(split(seq_len(nrow(out)), identity), function(i)
      i[which.max(out$xmax[i] - out$xmin[i] + 1)], integer(1))
    picks <- sort(picks)
    anchors <- out[picks, , drop = FALSE]
    anchors$anchor <- (anchors$xmin + anchors$xmax) / 2
    rownames(anchors) <- NULL
  }
  recipe$feature_annotation <- active_feature_annotation(x)
  ann <- methods::new("SynGenomicProjectionAnnotation", name = name,
    source_file = paste0("<projection:", source, ">"), source_annotation = source,
    feature_kind = kind, projection_data = out, anchors = anchors, recipe = recipe,
    signature = .projection_signature(x, source, recipe$feature_annotation, recipe))
  add_annotation(x, ann)
}

#' Inspect a stored genomic protein projection
#'
#' Coordinates are original genomic base centres. Mutation anchors use the
#' longest exonic fragment of each codon. Source changes require projection again.
#' @param x A SynIndividual containing a stored projection.
#' @param annotation Name of a stored genomic projection.
#' @param component `"fragments"` or `"anchors"`.
#' @param transcripts,chr,start,end Optional genomic/transcript filters.
#' @return A data frame; the source object is unchanged.
#' @export
query_genomic_projection <- function(x, annotation, component = c("fragments", "anchors"),
                                     transcripts = NULL, chr = NULL, start = NULL, end = NULL) {
  if (!methods::is(x, "SynIndividual")) stop("Supply a SynIndividual.", call. = FALSE)
  .genomic_name(annotation, "annotation")
  ann <- .find_genomic_projection(x, annotation)
  if (is.null(ann)) stop("Unknown stored genomic projection `", annotation, "`.", call. = FALSE)
  component <- match.arg(component)
  out <- if (component == "fragments") ann@projection_data else ann@anchors
  if (nrow(out)) {
    if (!is.null(transcripts)) out <- out[out$transcripts %in% transcripts, , drop = FALSE]
    if (!is.null(chr)) out <- out[out$seqnames %in% chr, , drop = FALSE]
    if (!is.null(start)) out <- out[out$xmax >= start, , drop = FALSE]
    if (!is.null(end)) out <- out[out$xmin <= end, , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

.query_stored_projection <- function(x, ann, args) {
  out <- query_genomic_projection(x, annotation_name(ann), transcripts = args$transcripts,
    chr = args$chr, start = args$start, end = args$end)
  if (!nrow(out)) return(out)
  if (ann@feature_kind == "mutation") {
    selectors <- args[intersect(names(args), c("genes", "strains", "mutation", "event_type",
      "min_sample_count", "protein_ranges", "ref"))]
    selected <- do.call(query_protein_mutations,
      c(list(x = get_annotation(x, ann@source_annotation)), selectors))
    keys <- intersect(names(selected), names(out))
    keys <- setdiff(keys, c("seqnames", "xmin", "xmax", "strand", "transcripts"))
    out <- as.data.frame(dplyr::semi_join(out, selected, by = keys))
  } else {
    source <- get_annotation(x, ann@source_annotation)
    source@domain_data <- query_domains(source)
    source@domain_data$.ggexon_projection_row <- seq_len(nrow(source@domain_data))
    selected <- query_domains(source, ids = args$ids, domains = args$domains)
    selected <- .filter_projectable_domains(selected, model = args$model, motif = args$motif)
    out <- out[out$.ggexon_projection_row %in% selected$.ggexon_projection_row, , drop = FALSE]
    if (!is.null(args$ids) || !is.null(args$genes)) {
      reference <- set_active_feature_annotation(x, ann@recipe$feature_annotation)
      meta <- .domain_projection_transcript_meta(query_features(reference, feature_type = "CDS", all = TRUE))
      if (!is.null(args$ids)) {
        selected <- query_domains(source, ids = args$ids)
        keys <- as.character(selected[[source@keytype]])
        meta <- meta[meta$transcript_id %in% keys | meta$gene_id %in% keys | meta$gene_name %in% keys, , drop = FALSE]
      }
      if (!is.null(args$genes)) meta <- meta[meta$gene_id %in% args$genes | meta$gene_name %in% args$genes, , drop = FALSE]
      out <- out[out$transcripts %in% meta$transcript_id, , drop = FALSE]
    }
  }
  rownames(out) <- NULL
  out
}

#' Prepare shared genomic geometry on a Syn object
#'
#' Computes a single exon-union intron map and bounded local head positions.
#' Stems, mutation heads, labels and exon layers can then use the same layout.
#' Geometry remains in original genomic units and the stored map is applied
#' once by `scale_x_ggexon_genomic(genomic_layout = name)`.
#' @param x A SynIndividual with a stored mutation projection.
#' @param annotation Stored mutation-projection name.
#' @param name Name for the prepared layout in the object.
#' @param transcripts One reference transcript; inferred only when unambiguous.
#' @param chr Optional reference chromosome.
#' @param intron_factor Compression factor for the shared intron map.
#' @param head_spread Maximum sideways offset, in genomic units. Zero disables spreading.
#' @param mutation_y_by,mutation_y_trans,mutation_y_range Height controls passed to protein_lollipop_data().
#' @param track_ymin,track_ymax Bottom and top of the exon model.
#' @param stem_points,curve_k Curve controls shared by all stem consumers.
#' @param base_edges Use half-base exon edges to preserve inclusive base counts.
#' @return An updated SynIndividual. Original annotations are retained.
#' @export
prepare_genomic_layout <- function(x, annotation, name = "genomic", transcripts = NULL,
                                   chr = NULL, intron_factor = 10, head_spread = 0,
                                   mutation_y_by = NULL, mutation_y_trans = "identity",
                                   mutation_y_range = c(0.85, 1.45), track_ymin = 2,
                                   track_ymax = 2.8, stem_points = 30, curve_k = 1,
                                   base_edges = TRUE) {
  .genomic_name(name, "name")
  ann <- .find_genomic_projection(x, annotation, "mutation")
  if (is.null(ann)) stop("Prepare a stored mutation projection first with `store = TRUE`.", call. = FALSE)
  if (is.null(transcripts)) transcripts <- ann@recipe$transcripts %||% unique(ann@anchors$transcripts)
  if (length(transcripts) != 1L || is.na(transcripts) || !nzchar(transcripts))
    stop("Select one reference transcript for a genomic layout.", call. = FALSE)
  intron_factor <- .lollipop_scalar_number(intron_factor, "intron_factor")
  head_spread <- .lollipop_scalar_number(head_spread, "head_spread")
  track_ymin <- .lollipop_scalar_number(track_ymin, "track_ymin")
  track_ymax <- .lollipop_scalar_number(track_ymax, "track_ymax")
  if (any(!is.finite(c(intron_factor, head_spread, track_ymin, track_ymax))) ||
      intron_factor <= 0 || head_spread < 0 || track_ymax <= track_ymin)
    stop("Use positive intron_factor, non-negative head_spread, and track_ymax > track_ymin.", call. = FALSE)
  if (!is.logical(base_edges) || length(base_edges) != 1L || is.na(base_edges))
    stop("`base_edges` must be TRUE or FALSE.", call. = FALSE)
  if (!is.null(ann@recipe$transcripts) && !transcripts %in% ann@recipe$transcripts)
    stop("Select a reference transcript included in the stored projection.", call. = FALSE)
  reference <- set_active_feature_annotation(x, ann@recipe$feature_annotation)
  features <- query_features(reference, transcripts = transcripts, chr = chr, feature_type = "exon")
  if (!length(features)) stop("The reference transcript has no exon features.", call. = FALSE)
  exons <- syn_gr_to_exon_df(features, track = syn_id(x), annotation_type = "exon")
  if (length(unique(exons$seqnames)) != 1L) stop("Select one chromosome per genomic layout.", call. = FALSE)
  chr <- unique(exons$seqnames)
  exons <- exons[order(exons$xmin), , drop = FALSE]
  exons$xmin <- as.numeric(exons$xmin) - if (base_edges) 0.5 else 0
  exons$xmax <- as.numeric(exons$xmax) + if (base_edges) 0.5 else 0
  exons$ymin <- track_ymin
  exons$PANEL <- NULL
  source <- data.frame(genomic_start = exons$xmin, genomic_end = exons$xmax,
    transcripts = exons$transcripts, track = exons$track, seqnames = exons$seqnames)
  map_source <- source
  map_source$PANEL <- 1L
  map <- build_ggexon_genomic_x_transform(map_source, intron_factor)
  map$PANEL <- NULL
  anchors <- query_genomic_projection(x, annotation, component = "anchors", transcripts = transcripts, chr = chr)
  domains <- data.frame(start = exons$xmin, end = exons$xmax, domain = "CDS")
  prepared <- protein_lollipop_data(anchors, position = "anchor", domains = domains, domain = "domain",
    protein_start = min(exons$xmin), protein_length = max(exons$xmax), spread_threshold = 0,
    track_ymin = track_ymin, track_ymax = track_ymax,
    domain_default_ymin = track_ymin, domain_default_ymax = track_ymax,
    mutation_y_by = mutation_y_by, mutation_y_trans = mutation_y_trans,
    mutation_y_range = mutation_y_range, stem_points = stem_points, curve_k = curve_k)
  heads <- prepared$mutations
  for (indices in split(seq_len(nrow(heads)), heads$lollipop_position)) {
    if (length(indices) < 2L || head_spread == 0) next
    span <- min(head_spread, heads$lollipop_position[indices[1L]] - min(exons$xmin),
      max(exons$xmax) - heads$lollipop_position[indices[1L]])
    heads$lollipop_x[indices] <- heads$lollipop_position[indices] + seq(-span, span, length.out = length(indices))
  }
  stems <- .generate_lollipop_stems(heads$lollipop_position, heads$lollipop_stem_y,
    heads$lollipop_x, heads$lollipop_y, n = stem_points, k = curve_k)
  stems$.layout_mutation_index <- stems$lollipop_curve_id
  heads$.layout_mutation_index <- seq_len(nrow(heads))
  components <- list(exons = exons, mutations = heads, stems = stems,
    backbone = data.frame(xmin = numeric(), xmax = numeric(), ymin = numeric(), ymax = numeric()),
    domains = data.frame(lollipop_start = numeric(), lollipop_end = numeric(),
      lollipop_ymin = numeric(), lollipop_ymax = numeric(), lollipop_domain = character()))
  parameters <- as.list(environment())[c("intron_factor", "head_spread", "mutation_y_by", "mutation_y_trans",
    "mutation_y_range", "track_ymin", "track_ymax", "stem_points", "curve_k", "base_edges")]
  entry <- list(name = name, projection = annotation, track = syn_id(x), chr = chr,
    transcripts = transcripts, intron_factor = intron_factor, map = map,
    source_rows = source, parameters = parameters, signature = ann@signature, components = components)
  x@plot_cache$genomic_layouts[[name]] <- entry
  x
}

.get_genomic_layout <- function(x, name) {
  if (!methods::is(x, "SynIndividual")) stop("A prepared genomic layout belongs to a SynIndividual.", call. = FALSE)
  .genomic_name(name, "genomic_layout")
  entry <- x@plot_cache$genomic_layouts[[name]]
  if (is.null(entry)) stop("Unknown prepared genomic layout `", name, "`.", call. = FALSE)
  ann <- .find_genomic_projection(x, entry$projection, "mutation")
  if (is.null(ann) || !identical(entry$signature, ann@signature))
    stop("Prepared genomic layout `", name, "` is stale; prepare it again.", call. = FALSE)
  entry
}

#' Inspect shared prepared genomic coordinates
#' @param x A SynIndividual containing a prepared genomic layout.
#' @param name Prepared layout name.
#' @param component One of mutations, stems, exons, backbone, or domains.
#' @return A data frame in original genomic coordinates.
#' @export
query_genomic_layout <- function(x, name, component = c("mutations", "stems", "exons", "backbone", "domains")) {
  component <- match.arg(component)
  out <- .get_genomic_layout(x, name)$components[[component]]
  attr(out, "ggexon_genomic_layout") <- name
  out
}

.prepared_geometry_arguments <- function(call, arguments) {
  supplied <- intersect(names(call)[-1L], arguments)
  if (length(supplied)) stop("Set geometry in `prepare_genomic_layout()`, not independently in this layer: ",
    paste(supplied, collapse = ", "), ".", call. = FALSE)
}

.prepared_mapping_arguments <- function(mapping) {
  coordinates <- intersect(names(mapping), c("x", "xmin", "xmax", "xend", "y", "ymin", "ymax", "yend"))
  if (length(coordinates)) stop("Prepared geometry supplies coordinates; set coordinate changes in `prepare_genomic_layout()`.", call. = FALSE)
}

.prepared_layout_individuals <- function(x, params, context = NULL) {
  targets <- params$individual %||% params$species
  if (is.null(targets)) targets <- resolve_context_species_params(x, context = context)
  lapply(unique(as.character(targets)), function(target) resolve_syn_individual(x, target))
}

.prepared_exon_layer_data <- function(x, layer) {
  params <- syn_layer_params(layer)
  individuals <- .prepared_layout_individuals(x, params, layer$syn_plot_context %||% NULL)
  entries <- lapply(individuals, .get_genomic_layout, name = params$genomic_layout)
  heights <- unique(vapply(entries, function(entry)
    entry$parameters$track_ymax - entry$parameters$track_ymin, numeric(1)))
  if (length(heights) != 1L) stop("Prepared exon layers must share one exon height.", call. = FALSE)
  explicit <- layer$geom_params$exon_height
  if (!is.null(explicit) && !isTRUE(all.equal(as.numeric(explicit), heights)))
    stop("`exon_height` conflicts with the prepared genomic layout.", call. = FALSE)
  layer$geom_params$exon_height <- heights[[1L]]
  out <- dplyr::bind_rows(lapply(entries, function(entry) entry$components$exons))
  attr(out, "ggexon_genomic_layout") <- params$genomic_layout
  out
}

.prepared_lollipop_component <- function(individual, params, component) {
  entry <- .get_genomic_layout(individual, params$genomic_layout)
  if (!is.null(params$annotation) && !identical(params$annotation, entry$projection))
    stop("`annotation` conflicts with the prepared genomic layout.", call. = FALSE)
  out <- entry$components[[component]]
  if (!is.null(params$score) && component %in% c("mutations", "stems")) {
    heads <- entry$components$mutations
    .lollipop_check_column(heads, params$score, "score")
    out$lollipop_score <- heads[[params$score]][out$.layout_mutation_index]
  }
  out$track <- rep(syn_id(individual), nrow(out))
  out$individual <- out$track
  if (component == "stems" && nrow(out))
    out$lollipop_curve_id <- paste(syn_id(individual), out$lollipop_curve_id, sep = "::")
  attr(out, "ggexon_genomic_layout") <- params$genomic_layout
  out
}

.prepared_mutation_labels <- function(x, params, context = NULL) {
  pieces <- lapply(.prepared_layout_individuals(x, params, context), function(individual) {
    out <- .prepared_lollipop_component(individual, params, "mutations")
    .lollipop_check_column(out, params$label_col %||% "mutation", "label")
    out$x <- out$lollipop_x
    out$y <- out$lollipop_y + (params$label_nudge_y %||% 0.35)
    out$label <- out[[params$label_col %||% "mutation"]]
    out$group <- seq_len(nrow(out))
    out
  })
  out <- dplyr::bind_rows(pieces)
  attr(out, "ggexon_genomic_layout") <- params$genomic_layout
  out
}
