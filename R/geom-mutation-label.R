GeomMutationLabel <- ggproto(
  "GeomMutationLabel",
  GeomText,
  required_aes = c("x", "y", "label"),
  non_missing_aes = "angle",
  default_aes = aes(
    colour = "black",
    family = "sans",
    size = 3,
    angle = 90,
    hjust = 0.5,
    vjust = 0.5,
    alpha = NA,
    fontface = 1,
    lineheight = 1.2
  ),
  extra_params = c(
    "na.rm", "annotation", "individual", "species", "genes", "event_type",
    "min_sample_count", "strains", "mutation", "ids", "mutation_position", "label_col",
    "ref", "alt", "spread_threshold", "mutation_y", "mutation_y_by",
    "mutation_y_strategy", "mutation_y_range", "mutation_y_trans",
    "mutation_y_breaks", "mutation_y_values", "label_nudge_y", "show_empty", "genomic_layout"
  ),
  default_params = function() {
    list(
      annotation = NULL,
      individual = NULL,
      species = NULL,
      genes = NULL,
      ids = NULL,
      event_type = NULL,
      min_sample_count = NULL,
      strains = NULL,
      mutation = NULL,
      mutation_position = "position",
      label_col = "mutation",
      ref = NULL,
      alt = NULL,
      spread_threshold = 7,
      mutation_y = 1,
      mutation_y_by = NULL,
      mutation_y_strategy = "scaled",
      mutation_y_range = c(0.85, 1.45),
      mutation_y_trans = "identity",
      mutation_y_breaks = NULL,
      mutation_y_values = NULL,
      label_nudge_y = 0.35,
      show_empty = FALSE
    )
  },
  syn_data = function(x, layer) {
    params <- syn_layer_params(layer)
    context <- layer$syn_plot_context %||% NULL
    if (!is.null(params$genomic_layout)) return(.prepared_mutation_labels(x, params, context))
    syn_to_mutation_label_df(
      x = x,
      annotation = params$annotation,
      individual = params$individual,
      species = params$species,
      genes = params$genes,
      ids = params$ids,
      event_type = params$event_type,
      min_sample_count = params$min_sample_count,
      strains = params$strains,
      mutation = params$mutation,
      mutation_position = params$mutation_position %||% "position",
      label = params$label_col %||% "mutation",
      ref = params$ref,
      alt = params$alt,
      spread_threshold = params$spread_threshold %||% 7,
      mutation_y = params$mutation_y %||% 1,
      mutation_y_by = params$mutation_y_by,
      mutation_y_strategy = params$mutation_y_strategy %||% "scaled",
      mutation_y_range = params$mutation_y_range %||% c(0.85, 1.45),
      mutation_y_trans = params$mutation_y_trans %||% "identity",
      mutation_y_breaks = params$mutation_y_breaks,
      mutation_y_values = params$mutation_y_values,
      label_nudge_y = params$label_nudge_y %||% 0.35,
      show_empty = params$show_empty %||% FALSE,
      context = context
    )
  },
  syn_default_aes = c("x", "y", "label", "group")
)

#' Draw mutation labels as a dedicated text layer
#'
#' `geom_mutation_label()` is the text companion to mutation lollipop layers.
#' With ordinary data frames it prepares label positions from mutation
#' positions. With `SynIndividual` or `SynSpecies` input it dispatches mutation
#' rows from attached `SynProteinMutationAnnotation` layers.
#'
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard
#'   ggplot2 layer arguments.
#' @param mutations Optional mutation table. Used as `data` when `data` is
#'   `NULL`.
#' @param annotation Optional mutation annotation-layer name.
#' @param individual Optional individual selector for `SynSpecies` input.
#' @param species Alias for `individual`, matching other Syn-aware geoms.
#' @param genes,event_type,min_sample_count,strains,mutation Optional filters
#'   passed to `query_protein_mutations()`.
#' @param ids Optional identifiers matching the mutation annotation's `keytype`.
#'   Use the same selector as `geom_protein_lollipop()` to label one isoform.
#' @param mutation_position Column containing mutation coordinates.
#' @param label Column used for text labels. Defaults to `"mutation"`.
#' @param ref,alt Optional residue/base columns used to build labels when
#'   `label` is `NULL`.
#' @param spread_threshold Minimum x-distance between adjacent labels.
#' @param mutation_y Fixed y coordinate of mutation heads when
#'   `mutation_y_by` is `NULL`.
#' @param mutation_y_by Optional numeric column used to align labels to
#'   multi-height lollipop heads.
#' @param mutation_y_strategy,mutation_y_range,mutation_y_trans,mutation_y_breaks,mutation_y_values
#'   Height-mapping controls shared with `protein_lollipop_data()`.
#' @param label_nudge_y Vertical offset above mutation heads.
#' @param show_empty Logical; keep empty/`NA` labels when `TRUE`.
#' @param genomic_layout Optional prepared genomic layout name. Labels read
#'   stored head positions, with label_nudge_y applied as a text offset.
#'
#' @return A ggplot layer.
#' @export
geom_mutation_label <- function(mapping = NULL,
                                data = NULL,
                                stat = "identity",
                                position = "identity",
                                ...,
                                mutations = NULL,
                                annotation = NULL,
                                individual = NULL,
                                species = NULL,
                                genes = NULL,
                                ids = NULL,
                                event_type = NULL,
                                min_sample_count = NULL,
                                strains = NULL,
                                mutation = NULL,
                                mutation_position = "position",
                                label = "mutation",
                                ref = NULL,
                                alt = NULL,
                                spread_threshold = 7,
                                mutation_y = 1,
                                mutation_y_by = NULL,
                                mutation_y_strategy = c("scaled", "bins"),
                                mutation_y_range = c(0.85, 1.45),
                                mutation_y_trans = "identity",
                                mutation_y_breaks = NULL,
                                mutation_y_values = NULL,
                                label_nudge_y = 0.35,
                                show_empty = FALSE,
                                na.rm = FALSE,
                                show.legend = FALSE,
                                inherit.aes = TRUE, genomic_layout = NULL) {
  if (!is.null(genomic_layout)) {
    .genomic_name(genomic_layout, "genomic_layout")
    .prepared_mapping_arguments(mapping)
    .prepared_mapping_arguments(list(...))
    if (!identical(position, "identity") || !identical(stat, "identity"))
      stop("Prepared geometry requires identity stat and position.", call. = FALSE)
    .prepared_geometry_arguments(match.call(), c("mutation_position", "spread_threshold",
      "mutation_y", "mutation_y_by", "mutation_y_strategy", "mutation_y_range", "mutation_y_trans",
      "mutation_y_breaks", "mutation_y_values", "genes", "ids", "event_type", "min_sample_count",
      "strains", "mutation"))
  }
  if (is.null(data) && !is.null(mutations)) {
    data <- mutations
  }
  if (!is.null(genomic_layout) && !is.null(data) &&
      !methods::is(data, "SynIndividual") && !methods::is(data, "SynSpecies"))
    stop("A prepared genomic layout requires Syn object input.", call. = FALSE)

  if ((is.data.frame(data) || methods::is(data, "DataFrame")) &&
      !methods::is(data, "SynIndividual") &&
      !methods::is(data, "SynSpecies")) {
    data <- mutation_label_data(
      mutations = data,
      mutation_position = mutation_position,
      label = label,
      ref = ref,
      alt = alt,
      spread_threshold = spread_threshold,
      mutation_y = mutation_y,
      mutation_y_by = mutation_y_by,
      mutation_y_strategy = mutation_y_strategy,
      mutation_y_range = mutation_y_range,
      mutation_y_trans = mutation_y_trans,
      mutation_y_breaks = mutation_y_breaks,
      mutation_y_values = mutation_y_values,
      label_nudge_y = label_nudge_y,
      show_empty = show_empty
    )
    if (is.null(mapping)) {
      mapping <- ggplot2::aes(x = .data$x, y = .data$y, label = .data$label)
    }
  }

  params <- Filter(Negate(is.null), list(
    ...,
    na.rm = na.rm,
    genomic_layout = genomic_layout,
    annotation = annotation,
    individual = individual,
    species = species,
    genes = genes,
    ids = ids,
    event_type = event_type,
    min_sample_count = min_sample_count,
    strains = strains,
    mutation = mutation,
    mutation_position = mutation_position,
    label_col = label,
    ref = ref,
    alt = alt,
    spread_threshold = spread_threshold,
    mutation_y = mutation_y,
    mutation_y_by = mutation_y_by,
    mutation_y_strategy = mutation_y_strategy,
    mutation_y_range = mutation_y_range,
    mutation_y_trans = mutation_y_trans,
    mutation_y_breaks = mutation_y_breaks,
    mutation_y_values = mutation_y_values,
    label_nudge_y = label_nudge_y,
    show_empty = show_empty
  ))

  layer(
    data = data,
    mapping = mapping,
    geom = GeomMutationLabel,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    layer_class = LayerSyn,
    params = params
  )
}

mutation_label_data <- function(mutations,
                                mutation_position = "position",
                                label = "mutation",
                                ref = NULL,
                                alt = NULL,
                                spread_threshold = 7,
                                mutation_y = 1,
                                mutation_y_by = NULL,
                                mutation_y_strategy = c("scaled", "bins"),
                                mutation_y_range = c(0.85, 1.45),
                                mutation_y_trans = "identity",
                                mutation_y_breaks = NULL,
                                mutation_y_values = NULL,
                                label_nudge_y = 0.35,
                                show_empty = FALSE,
                                group_cols = NULL) {
  mut_df <- .lollipop_as_data_frame(mutations, "mutations")
  .lollipop_check_column(mut_df, mutation_position, "mutation_position")

  positions <- suppressWarnings(as.numeric(mut_df[[mutation_position]]))
  if (anyNA(positions)) {
    stop("`mutation_position` must identify a numeric mutation-position column.", call. = FALSE)
  }

  label_values <- .lollipop_label_values(
    mut_df = mut_df,
    positions = positions,
    label = label,
    ref = ref,
    alt = alt
  )

  mut_df$x <- .spread_mutation_label_positions(
    data = mut_df,
    positions = positions,
    threshold = spread_threshold,
    group_cols = group_cols
  )
  mut_df$y <- .lollipop_mutation_y_values(
    mut_df = mut_df,
    mutation_y = mutation_y,
    mutation_y_by = mutation_y_by,
    mutation_y_strategy = mutation_y_strategy,
    mutation_y_range = mutation_y_range,
    mutation_y_trans = mutation_y_trans,
    mutation_y_breaks = mutation_y_breaks,
    mutation_y_values = mutation_y_values
  ) +
    .lollipop_scalar_number(label_nudge_y, "label_nudge_y")
  mut_df$label <- label_values

  if (!isTRUE(show_empty)) {
    keep <- !is.na(mut_df$label) & nzchar(as.character(mut_df$label))
    mut_df <- mut_df[keep, , drop = FALSE]
  }

  rownames(mut_df) <- NULL
  mut_df
}

syn_to_mutation_label_df <- function(x,
                                     annotation = NULL,
                                     individual = NULL,
                                     species = NULL,
                                     genes = NULL,
                                     ids = NULL,
                                     event_type = NULL,
                                     min_sample_count = NULL,
                                     strains = NULL,
                                     mutation = NULL,
                                     mutation_position = "position",
                                     label = "mutation",
                                     ref = NULL,
                                     alt = NULL,
                                     spread_threshold = 7,
                                     mutation_y = 1,
                                     mutation_y_by = NULL,
                                     mutation_y_strategy = c("scaled", "bins"),
                                     mutation_y_range = c(0.85, 1.45),
                                     mutation_y_trans = "identity",
                                     mutation_y_breaks = NULL,
                                     mutation_y_values = NULL,
                                     label_nudge_y = 0.35,
                                     show_empty = FALSE,
                                     context = NULL) {
  target_individual <- individual %||% species
  if (methods::is(x, "SynSpecies") && is.null(target_individual)) {
    target_individual <- resolve_context_species_params(x, species = NULL, context = context)
  }
  if (methods::is(x, "SynSpecies")) {
    return(dplyr::bind_rows(lapply(target_individual, function(target) {
      one <- resolve_syn_individual(x, species = target)
      ann <- resolve_syn_protein_mutation_annotation(one, annotation, allow_missing = TRUE)
      if (is.null(ann)) return(data.frame())
      syn_to_mutation_label_df(
        one, annotation = annotation_name(ann), genes = genes, ids = ids,
        event_type = event_type, min_sample_count = min_sample_count,
        strains = strains, mutation = mutation, mutation_position = mutation_position,
        label = label, ref = ref, alt = alt, spread_threshold = spread_threshold,
        mutation_y = mutation_y, mutation_y_by = mutation_y_by,
        mutation_y_strategy = mutation_y_strategy, mutation_y_range = mutation_y_range,
        mutation_y_trans = mutation_y_trans, mutation_y_breaks = mutation_y_breaks,
        mutation_y_values = mutation_y_values, label_nudge_y = label_nudge_y,
        show_empty = show_empty
      )
    })))
  }

  mutations <- query_protein_mutations(
    x,
    annotation = annotation,
    individual = target_individual,
    genes = genes,
    ids = ids,
    event_type = event_type,
    min_sample_count = min_sample_count,
    strains = strains,
    mutation = mutation
  )

  if (nrow(mutations) == 0L) {
    return(data.frame())
  }

  out <- mutation_label_data(
    mutations = mutations,
    mutation_position = mutation_position,
    label = label,
    ref = ref,
    alt = alt,
    spread_threshold = spread_threshold,
    mutation_y = mutation_y,
    mutation_y_by = mutation_y_by,
    mutation_y_strategy = mutation_y_strategy,
    mutation_y_range = mutation_y_range,
    mutation_y_trans = mutation_y_trans,
    mutation_y_breaks = mutation_y_breaks,
    mutation_y_values = mutation_y_values,
    label_nudge_y = label_nudge_y,
    show_empty = show_empty,
    group_cols = if (methods::is(x, "SynIndividual")) {
      ann <- resolve_syn_protein_mutation_annotation(x, annotation)
      ann@keytype
    }
  )

  if (methods::is(x, "SynIndividual")) {
    out$track <- rep(syn_id(x), nrow(out))
  } else if (!"track" %in% names(out)) {
    if ("individual" %in% names(out)) {
      out$track <- as.character(out$individual)
    } else {
      out$track <- NA_character_
    }
  }
  out$PANEL <- rep(1L, nrow(out))
  out
}

.spread_mutation_label_positions <- function(data, positions, threshold = 7,
                                              group_cols = NULL) {
  group_cols <- intersect(group_cols %||%
    c("individual", "track", "gene_id", "gene", "gene_name", "transcript_id", "protein_id"),
    names(data))
  if (length(group_cols) == 0L || length(positions) == 0L) {
    return(.spread_lollipop_positions(positions, threshold = threshold))
  }

  keys <- do.call(
    interaction,
    c(data[group_cols], list(drop = TRUE, lex.order = TRUE, sep = "\r"))
  )
  out <- numeric(length(positions))
  for (key in unique(keys)) {
    idx <- which(keys == key)
    out[idx] <- .spread_lollipop_positions(positions[idx], threshold = threshold)
  }
  out
}
