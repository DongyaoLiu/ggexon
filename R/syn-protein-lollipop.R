.syn_protein_lollipop_layer <- function(component, data, mapping, params, style,
                                         show.legend) {
  parent <- switch(component, stems = GeomLine, backbone = GeomRect,
    domains = GeomRect, mutations = GeomPoint)
  geom <- ggproto(
    paste0("GeomProteinLollipop_", component), parent,
    extra_params = unique(c(parent$extra_params, names(params))),
    syn_default_aes = "track",
    syn_data = function(x, layer) {
      .syn_protein_lollipop_component(x, syn_layer_params(layer), component,
        context = layer$syn_plot_context %||% NULL)
    }
  )
  explicit_syn <- methods::is(data, "SynIndividual") || methods::is(data, "SynSpecies")
  result <- layer(data = if (explicit_syn) NULL else data,
    mapping = mapping, geom = geom, stat = "identity",
    position = "identity", inherit.aes = FALSE, layer_class = LayerSyn,
    show.legend = show.legend, params = c(params, style))
  if (explicit_syn) result$data <- data
  result
}

.syn_protein_lollipop_component <- function(x, params, component, context = NULL) {
  targets <- params$individual %||% params$species
  explicit_targets <- !is.null(targets)
  if (is.null(targets)) {
    targets <- resolve_context_species_params(x, context = context)
  }
  if (length(targets) == 0L) {
    stop("No individuals are available for protein lollipops.", call. = FALSE)
  }
  pieces <- lapply(unique(as.character(targets)), function(target) {
    individual <- resolve_syn_individual(x, species = target)
    ann <- resolve_syn_protein_mutation_annotation(individual, params$annotation,
      allow_missing = methods::is(x, "SynSpecies") && !explicit_targets)
    if (is.null(ann)) return(NULL)
    selected <- query_protein_mutations(ann, genes = params$genes, ids = params$ids)
    .lollipop_check_column(selected, ann@keytype, "keytype")
    keys <- unique(as.character(selected[[ann@keytype]]))
    keys <- keys[!is.na(keys) & nzchar(keys)]
    if (length(keys) == 0L) {
      keys <- params$ids %||% if (ann@keytype == "gene_id") params$genes
    }
    if (length(keys) != 1L) {
      stop("Select one protein per individual with `genes` or `ids`; ",
        "protein-coordinate tracks cannot combine different proteins.", call. = FALSE)
    }
    .syn_lollipop_check_identity(selected)

    domains <- .syn_lollipop_domains(individual, params, selected, ann@keytype, keys)
    protein_length <- .syn_lollipop_protein_length(individual, params$protein_length,
      selected, domains, keys)
    mutations <- query_protein_mutations(
      ann, genes = params$genes, ids = keys, event_type = params$event_type,
      min_sample_count = params$min_sample_count, strains = params$strains,
      mutation = params$mutation
    )
    preparation <- params[intersect(names(params),
      setdiff(names(formals(protein_lollipop_data)), c("mutations", "domains")))]
    preparation$protein_length <- protein_length
    prepared <- do.call(protein_lollipop_data,
      c(list(mutations = mutations, domains = domains), preparation))
    if (any(prepared$mutations$lollipop_position > protein_length) ||
        any(prepared$domains$lollipop_end > protein_length)) {
      stop("Mutation or domain coordinates exceed `protein_length`.", call. = FALSE)
    }
    out <- prepared[[component]]
    if (component == "domains" && nrow(out) == 0L) {
      out <- data.frame(lollipop_start = numeric(), lollipop_end = numeric(),
        lollipop_ymin = numeric(), lollipop_ymax = numeric(), lollipop_domain = character())
    }
    out$track <- rep(syn_id(individual), nrow(out))
    out$individual <- out$track
    if (component == "stems" && nrow(out) > 0L) {
      out$lollipop_curve_id <- paste(target, out$lollipop_curve_id, sep = "::")
    }
    out
  })
  pieces <- Filter(Negate(is.null), pieces)
  if (length(pieces) == 0L) {
    stop("No matching SynProteinMutationAnnotation is attached to the selected individuals.",
      call. = FALSE)
  }
  dplyr::bind_rows(pieces)
}

.syn_lollipop_domains <- function(individual, params, mutations, keytype, keys) {
  if (!is.null(params$domains) && !is.null(params$domain_annotation)) {
    stop("Supply only one of `domains` and `domain_annotation`.", call. = FALSE)
  }
  if (!is.null(params$domains) && !methods::is(params$domains, "SynProteinDomainAnnotation")) {
    out <- .lollipop_as_data_frame(params$domains, "domains")
    return(.syn_lollipop_filter_domains(out, mutations, keytype, keys))
  }
  ann <- params$domains %||% resolve_syn_domain_annotation(
    individual, params$domain_annotation, allow_missing = is.null(params$domain_annotation))
  if (is.null(ann)) {
    return(NULL)
  }
  domain_ids <- if (ann@keytype == keytype) keys else {
    if (!ann@keytype %in% names(mutations)) {
      stop("Domain and mutation annotations need matching identifier keys. ",
        "Add `", ann@keytype, "` to the mutation records or use `map_domain_ids()`.",
        call. = FALSE)
    }
    unique(as.character(mutations[[ann@keytype]]))
  }
  domain_ids <- domain_ids[!is.na(domain_ids) & nzchar(domain_ids)]
  if (length(domain_ids) != 1L) {
    stop("Select one protein or transcript before resolving its domains.", call. = FALSE)
  }
  out <- as.data.frame(query_domains(ann, ids = domain_ids), stringsAsFactors = FALSE)
  .syn_lollipop_filter_domains(out, mutations, keytype, keys)
}

.syn_lollipop_filter_domains <- function(domains, mutations, keytype, keys) {
  columns <- intersect(unique(c(keytype, "protein_id", "transcript_id", "gene_id")),
    names(domains))
  matched <- FALSE
  for (column in columns) {
    values <- if (column == keytype) keys else as.character(mutations[[column]])
    values <- unique(values[!is.na(values) & nzchar(values)])
    if (length(values) == 0L) next
    domains <- domains[as.character(domains[[column]]) %in% values, , drop = FALSE]
    matched <- TRUE
  }
  if (length(columns) > 0L && !matched) {
    stop("Domain and mutation annotations need matching identifier keys.", call. = FALSE)
  }
  .syn_lollipop_check_identity(domains)
  domains
}

.syn_lollipop_check_identity <- function(data) {
  for (column in intersect(c("protein_id", "transcript_id"), names(data))) {
    values <- unique(as.character(data[[column]]))
    values <- values[!is.na(values) & nzchar(values)]
    if (length(values) > 1L) {
      stop("Select one protein or one transcript using an annotation keyed by ",
        "`protein_id` or `transcript_id` and the `ids` selector.", call. = FALSE)
    }
  }
  invisible(NULL)
}

.syn_lollipop_protein_length <- function(individual, explicit, mutations, domains, keys) {
  if (!is.null(explicit)) {
    value <- .lollipop_scalar_number(explicit, "protein_length")
  } else {
    lengths <- c(mutations$protein_length, domains$protein_length)
    lengths <- unique(suppressWarnings(as.numeric(lengths)))
    lengths <- lengths[is.finite(lengths) & lengths > 0]
    if (length(lengths) > 1L) {
      stop("Conflicting stored protein lengths; select one protein or supply `protein_length`.",
        call. = FALSE)
    }
    if (length(lengths) == 1L) {
      value <- lengths[[1L]]
    } else {
      sequences <- protein_seq(individual)
      sequence_ids <- unique(c(keys, as.character(mutations$protein_id),
        as.character(mutations$transcript_id)))
      hits <- intersect(names(sequences), sequence_ids)
      if (length(hits) != 1L) {
        stop("Supply `protein_length`, store a `protein_length` column, or attach ",
          "one matching protein sequence. The last mutation is not the full protein length.",
          call. = FALSE)
      }
      value <- nchar(sub("\\*$", "", as.character(sequences[hits])))
    }
  }
  if (!is.finite(value) || value <= 0) {
    stop("`protein_length` must be finite and positive.", call. = FALSE)
  }
  unname(value)
}
