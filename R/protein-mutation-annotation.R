#' Read protein-mutation count summaries
#'
#' Reads a tab-delimited protein-mutation summary table and normalizes common
#' columns used by ggexon. Hash notations such as `C#316#H` are parsed into
#' `ref`, `position`, `alt`, and `mutation` columns.
#'
#' @param file Path to a tab-delimited mutation-count table.
#' @param individual_col Column used to identify individuals/strains. `"auto"`
#'   checks `individual`, `species`, `strain`, `strains`, and `id`, in that
#'   order.
#' @param as_long Logical; when `TRUE`, return one row per mutation-individual
#'   pair. When `FALSE`, return the summary table and store the long index as
#'   an attribute.
#'
#' @return A normalized `data.frame`.
#' @export
read_protein_mutation_counts <- function(file,
                                         individual_col = "auto",
                                         as_long = FALSE) {
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    stop("`file` must be a single non-empty character value.", call. = FALSE)
  }
  if (!file.exists(file)) {
    stop("Mutation file does not exist: ", file, call. = FALSE)
  }
  if (!is.logical(as_long) || length(as_long) != 1L || is.na(as_long)) {
    stop("`as_long` must be a single TRUE/FALSE value.", call. = FALSE)
  }

  tbl <- utils::read.delim(
    file = file,
    sep = "\t",
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    quote = "",
    comment.char = ""
  )
  tbl <- .normalize_protein_mutation_table(tbl)
  resolved_col <- .resolve_protein_mutation_individual_col(tbl, individual_col)
  individual_index <- .protein_mutation_individual_index(tbl, resolved_col)

  attr(tbl, "individual_col") <- resolved_col
  attr(tbl, "individual_index") <- individual_index

  if (isTRUE(as_long)) {
    return(.protein_mutation_as_long(tbl, individual_index))
  }

  tbl
}

#' Query protein-mutation annotations
#'
#' @param x A `SynProteinMutationAnnotation`, `SynIndividual`, or `SynSpecies`.
#' @param annotation Optional annotation-layer name. Defaults to the first
#'   attached `SynProteinMutationAnnotation`.
#' @param individual Optional individual name(s) when `x` is a `SynSpecies`.
#' @param genes Optional gene identifiers.
#' @param event_type Optional mutation event type filter.
#' @param min_sample_count Optional minimum `sample_count`.
#' @param strains Optional strain/individual identifiers to match against a
#'   routed mutation table.
#' @param mutation Optional mutation labels, for example `"C316H"`.
#' @param protein_ranges Optional protein-coordinate windows such as
#'   `"100-160"`. Values outside the inferred or supplied protein boundaries
#'   are clamped to those boundaries.
#' @param domains Optional protein-domain interval table used with
#'   `protein_domains`.
#' @param protein_domains Optional domain names. Matching domain intervals are
#'   converted to protein-coordinate windows.
#' @param ref Optional reference amino acid filter. Values must be one-letter
#'   amino-acid codes.
#' @param protein_length Optional protein length used to clamp
#'   `protein_ranges`.
#'
#' @return A `data.frame` of matching mutation records.
#' @export
query_protein_mutations <- function(x,
                                    annotation = NULL,
                                    individual = NULL,
                                    genes = NULL,
                                    event_type = NULL,
                                    min_sample_count = NULL,
                                    strains = NULL,
                                    mutation = NULL,
                                    protein_ranges = NULL,
                                    domains = NULL,
                                    protein_domains = NULL,
                                    ref = NULL,
                                    protein_length = NULL) {
  if (methods::is(x, "SynProteinMutationAnnotation")) {
    out <- protein_mutation_data(x)
    index <- protein_mutation_individual_index(x)
    if (nrow(index) == 0L) {
      index <- attr(out, "individual_index", exact = TRUE) %||% index
    }
    return(.filter_protein_mutation_query(
      out,
      genes = genes,
      event_type = event_type,
      min_sample_count = min_sample_count,
      strains = strains,
      mutation = mutation,
      protein_ranges = protein_ranges,
      domains = domains,
      protein_domains = protein_domains,
      ref = ref,
      protein_length = protein_length,
      individual_index = index
    ))
  }

  if (methods::is(x, "SynIndividual")) {
    ann <- resolve_syn_protein_mutation_annotation(x, annotation = annotation)
    return(query_protein_mutations(
      ann,
      genes = genes,
      event_type = event_type,
      min_sample_count = min_sample_count,
      strains = strains,
      mutation = mutation,
      protein_ranges = protein_ranges,
      domains = domains,
      protein_domains = protein_domains,
      ref = ref,
      protein_length = protein_length
    ))
  }

  if (methods::is(x, "SynSpecies")) {
    targets <- individual %||% names(individuals(x))
    targets <- as.character(targets)
    targets <- targets[!is.na(targets) & nzchar(targets)]
    if (length(targets) == 0L) {
      return(data.frame())
    }

    out <- lapply(targets, function(individual_id) {
      if (!individual_id %in% names(individuals(x))) {
        return(data.frame())
      }
      ann <- tryCatch(
        resolve_syn_protein_mutation_annotation(
          individuals(x)[[individual_id]],
          annotation = annotation,
          allow_missing = TRUE
        ),
        error = function(e) NULL
      )
      if (is.null(ann)) {
        return(data.frame())
      }
      one <- query_protein_mutations(
        ann,
        genes = genes,
        event_type = event_type,
        min_sample_count = min_sample_count,
        strains = strains,
        mutation = mutation,
        protein_ranges = protein_ranges,
        domains = domains,
        protein_domains = protein_domains,
        ref = ref,
        protein_length = protein_length
      )
      if (nrow(one) > 0L && !"individual" %in% names(one)) {
        one$individual <- individual_id
      }
      one
    })
    return(.bind_rows_base(out))
  }

  stop(
    "`query_protein_mutations()` expects a SynProteinMutationAnnotation, SynIndividual, or SynSpecies object.",
    call. = FALSE
  )
}

#' Subset protein-mutation tables
#'
#' `subset_protein_mutations()` filters normalized protein-mutation data frames
#' by individual/strain IDs, protein-coordinate windows, domain names, and
#' reference amino acids. Protein-domain filters are converted to coordinate
#' windows before filtering.
#'
#' @param mutations A data frame returned by `read_protein_mutation_counts()` or
#'   another data frame with at least a protein-position column.
#' @param individuals Optional strain/species/individual IDs. A single string,
#'   character vector, or list is accepted.
#' @param protein_ranges Optional protein-coordinate windows such as
#'   `"10-50"`. A single string, character vector, or list is accepted. Bounds
#'   are validated and clamped to the inferred protein boundaries.
#' @param domains Optional protein-domain interval table, `S4Vectors::DataFrame`,
#'   or `SynProteinDomainAnnotation` used with `protein_domains`.
#' @param protein_domains Optional domain names. Matching domain intervals are
#'   treated like protein-coordinate windows.
#' @param ref Optional reference amino acids. Values are matched against `ref`
#'   or common reference-amino-acid columns.
#' @param position Mutation protein-coordinate column.
#' @param protein_start Lower protein-coordinate boundary.
#' @param protein_length Optional protein length used as the upper coordinate
#'   boundary. When `NULL`, the upper boundary is inferred from mutation and
#'   domain coordinates.
#' @param domain_start,domain_end Domain interval start/end columns.
#' @param domain Optional domain-name column. When omitted, a common domain
#'   column name is inferred.
#' @param individual_index Optional long mutation-individual index. Defaults to
#'   the `individual_index` attribute created by `read_protein_mutation_counts()`.
#'
#' @return A filtered mutation `data.frame`.
#' @export
subset_protein_mutations <- function(mutations,
                                     individuals = NULL,
                                     protein_ranges = NULL,
                                     domains = NULL,
                                     protein_domains = NULL,
                                     ref = NULL,
                                     position = "position",
                                     protein_start = 1,
                                     protein_length = NULL,
                                     domain_start = "start",
                                     domain_end = "end",
                                     domain = NULL,
                                     individual_index = attr(mutations, "individual_index", exact = TRUE)) {
  out <- .protein_mutation_table_as_data_frame(mutations, "mutations")
  original_attrs <- attributes(mutations)
  if (nrow(out) == 0L) {
    return(out)
  }

  if (!is.null(individuals)) {
    out <- .subset_protein_mutations_by_individual(
      out,
      individuals = individuals,
      individual_index = individual_index
    )
  }

  if (!is.null(ref)) {
    out <- .subset_protein_mutations_by_ref(out, ref = ref)
  }

  if (!is.null(protein_ranges) || !is.null(protein_domains)) {
    out <- .subset_protein_mutations_by_coordinates(
      out,
      position = position,
      protein_ranges = protein_ranges,
      domains = domains,
      protein_domains = protein_domains,
      protein_start = protein_start,
      protein_length = protein_length,
      domain_start = domain_start,
      domain_end = domain_end,
      domain = domain
    )
  }

  rownames(out) <- NULL
  attr(out, "individual_col") <- original_attrs$individual_col %||% attr(mutations, "individual_col", exact = TRUE)
  attr(out, "individual_index") <- .protein_mutation_filter_index(
    individual_index,
    source_row_id = out$source_row_id %||% integer()
  )
  out
}

#' Add protein-mutation annotations
#'
#' Attaches protein-coordinate mutation summaries to a `SynIndividual`, or
#' dispatches them across child individuals of a `SynSpecies` when an
#' individual/strain column is available.
#'
#' @param x A `SynIndividual` or `SynSpecies` object.
#' @param mutation_file Path to a protein-mutation count table.
#' @param name Annotation-layer name.
#' @param keytype Identifier column used to match mutation rows to features.
#' @param individual Optional individual name(s). For a `SynIndividual`, this
#'   defaults to `syn_id(x)`. For a `SynSpecies`, this limits the import.
#' @param individual_col Individual/strain column, or `"auto"`.
#' @param all Logical. For `SynSpecies`, when `TRUE`, import all routed
#'   individuals by default.
#' @param create_missing Logical. For `SynSpecies`, create annotation-only
#'   `SynIndividual` objects for routed individuals that are not already stored.
#' @param metadata Optional metadata list.
#'
#' @return The updated `SynIndividual` or `SynSpecies`.
#' @export
add_protein_mutation_annotation <- function(x,
                                            mutation_file,
                                            name = "protein_mutations",
                                            keytype = "gene_id",
                                            individual = NULL,
                                            individual_col = "auto",
                                            all = TRUE,
                                            create_missing = TRUE,
                                            metadata = list()) {
  mutation_data <- read_protein_mutation_counts(
    file = mutation_file,
    individual_col = individual_col,
    as_long = FALSE
  )
  resolved_col <- attr(mutation_data, "individual_col", exact = TRUE)
  individual_index <- attr(mutation_data, "individual_index", exact = TRUE)

  if (methods::is(x, "SynIndividual")) {
    target <- individual %||% syn_id(x)
    target <- as.character(target)
    if (length(target) != 1L || is.na(target) || !nzchar(target)) {
      stop("`individual` must be one non-empty value for SynIndividual imports.", call. = FALSE)
    }
    subset <- .protein_mutation_subset_for_individual(
      mutation_data,
      individual = target,
      individual_index = individual_index
    )
    subset_index <- .protein_mutation_filter_index(individual_index, subset$source_row_id)
    ann <- SynProteinMutationAnnotation(
      name = name,
      mutation_file = mutation_file,
      keytype = keytype,
      mutation_data = subset,
      individual_index = subset_index,
      metadata = .protein_mutation_metadata(
        metadata = metadata,
        individual_col = resolved_col,
        source_rows = nrow(mutation_data),
        imported_rows = nrow(subset),
        individual = target
      ),
      lazy = FALSE
    )
    return(add_annotation(x, ann, set_active = FALSE))
  }

  if (methods::is(x, "SynSpecies")) {
    if (!is.logical(all) || length(all) != 1L || is.na(all)) {
      stop("`all` must be a single TRUE/FALSE value.", call. = FALSE)
    }
    if (!is.logical(create_missing) || length(create_missing) != 1L || is.na(create_missing)) {
      stop("`create_missing` must be a single TRUE/FALSE value.", call. = FALSE)
    }

    targets <- .protein_mutation_targets_for_synspecies(
      x = x,
      mutation_data = mutation_data,
      individual = individual,
      individual_index = individual_index,
      all = all
    )
    if (length(targets) == 0L) {
      return(x)
    }

    for (target in targets) {
      if (!target %in% names(individuals(x))) {
        if (!isTRUE(create_missing)) {
          next
        }
        x <- add_individual(
          x,
          SynIndividual(genome_file = genome_waiver(), id = target)
        )
      }

      subset <- if (is.null(resolved_col)) {
        mutation_data
      } else {
        .protein_mutation_subset_for_individual(
          mutation_data,
          individual = target,
          individual_index = individual_index
        )
      }
      subset_index <- .protein_mutation_filter_index(individual_index, subset$source_row_id)
      ann <- SynProteinMutationAnnotation(
        name = name,
        mutation_file = mutation_file,
        keytype = keytype,
        mutation_data = subset,
        individual_index = subset_index,
        metadata = .protein_mutation_metadata(
          metadata = metadata,
          individual_col = resolved_col,
          source_rows = nrow(mutation_data),
          imported_rows = nrow(subset),
          individual = target
        ),
        lazy = FALSE
      )
      x@individuals[[target]] <- add_annotation(x@individuals[[target]], ann, set_active = FALSE)
    }

    validObject(x)
    return(x)
  }

  stop(
    "`add_protein_mutation_annotation()` expects a SynIndividual or SynSpecies object.",
    call. = FALSE
  )
}

#' Access protein-mutation data
#'
#' @param x A `SynProteinMutationAnnotation` object.
#'
#' @return A normalized mutation `data.frame`.
#' @export
setGeneric("protein_mutation_data", function(x) standardGeneric("protein_mutation_data"))
setMethod("protein_mutation_data", "SynProteinMutationAnnotation", function(x) {
  if (is.null(x@mutation_data)) {
    mutation_data <- read_protein_mutation_counts(source_file(x))
    x@mutation_data <- mutation_data
    x@individual_index <- attr(mutation_data, "individual_index", exact = TRUE)
    x@loaded <- TRUE
  }
  x@mutation_data
})

setGeneric("protein_mutation_individual_index", function(x) {
  standardGeneric("protein_mutation_individual_index")
})
setMethod("protein_mutation_individual_index", "SynProteinMutationAnnotation", function(x) {
  x@individual_index %||% data.frame(source_row_id = integer(), individual = character())
})

resolve_syn_protein_mutation_annotation <- function(x,
                                                    annotation = NULL,
                                                    allow_missing = FALSE) {
  if (!methods::is(x, "SynIndividual")) {
    stop("Protein mutation annotations require a SynIndividual object.", call. = FALSE)
  }

  if (!is.null(annotation)) {
    if (!annotation %in% annotation_names(x)) {
      if (allow_missing) {
        return(NULL)
      }
      stop("Unknown annotation layer: ", annotation, call. = FALSE)
    }
    ann <- get_annotation(x, annotation)
    if (!methods::is(ann, "SynProteinMutationAnnotation")) {
      if (allow_missing) {
        return(NULL)
      }
      stop(
        "Annotation layer `",
        annotation,
        "` is not a SynProteinMutationAnnotation.",
        call. = FALSE
      )
    }
    return(ann)
  }

  ann_names <- annotation_names(x)
  for (ann_name in ann_names) {
    ann <- get_annotation(x, ann_name)
    if (methods::is(ann, "SynProteinMutationAnnotation")) {
      return(ann)
    }
  }

  if (allow_missing) {
    return(NULL)
  }
  stop("No SynProteinMutationAnnotation layer is attached to this SynIndividual.", call. = FALSE)
}

.normalize_protein_mutation_table <- function(tbl) {
  if (!is.data.frame(tbl)) {
    stop("Mutation data must be a data.frame.", call. = FALSE)
  }
  if (nrow(tbl) == 0L) {
    tbl$source_row_id <- integer()
    return(tbl)
  }

  names(tbl) <- trimws(names(tbl))
  tbl$source_row_id <- seq_len(nrow(tbl))

  gene_col <- .find_named_column(tbl, c("gene_id", "gene", "gene_name"))
  if (!is.null(gene_col) && !"gene_id" %in% names(tbl)) {
    tbl$gene_id <- as.character(tbl[[gene_col]])
  }

  notation_col <- .find_named_column(
    tbl,
    c("mutation_hash_or_notation", "mutation", "aa_change", "protein_change", "notation")
  )
  parsed <- .parse_protein_mutation_notation(
    if (is.null(notation_col)) rep(NA_character_, nrow(tbl)) else as.character(tbl[[notation_col]])
  )

  if (!"ref" %in% names(tbl)) {
    tbl$ref <- parsed$ref
  } else {
    tbl$ref <- .coalesce_chr(as.character(tbl$ref), parsed$ref)
  }
  if (!"alt" %in% names(tbl)) {
    tbl$alt <- parsed$alt
  } else {
    tbl$alt <- .coalesce_chr(as.character(tbl$alt), parsed$alt)
  }
  if (!"position" %in% names(tbl)) {
    tbl$position <- parsed$position
  } else {
    tbl$position <- suppressWarnings(as.integer(tbl$position))
    tbl$position[is.na(tbl$position)] <- parsed$position[is.na(tbl$position)]
  }

  if (!"mutation" %in% names(tbl)) {
    tbl$mutation <- ifelse(
      !is.na(tbl$ref) & nzchar(tbl$ref) &
        !is.na(tbl$position) &
        !is.na(tbl$alt) & nzchar(tbl$alt),
      paste0(tbl$ref, tbl$position, tbl$alt),
      if (is.null(notation_col)) NA_character_ else as.character(tbl[[notation_col]])
    )
  }

  count_col <- .find_named_column(tbl, c("sample_count", "count", "n"))
  if (!is.null(count_col)) {
    tbl$sample_count <- suppressWarnings(as.integer(tbl[[count_col]]))
  }
  if ("length" %in% names(tbl)) {
    tbl$length <- suppressWarnings(as.integer(tbl$length))
  }
  if ("event_type" %in% names(tbl)) {
    tbl$event_type <- as.character(tbl$event_type)
  }

  tbl
}

.parse_protein_mutation_notation <- function(x) {
  ref <- rep(NA_character_, length(x))
  alt <- rep(NA_character_, length(x))
  position <- rep(NA_integer_, length(x))

  hash_parts <- strsplit(x, "#", fixed = TRUE)
  hash_ok <- vapply(hash_parts, length, integer(1)) == 3L
  if (any(hash_ok)) {
    ref[hash_ok] <- vapply(hash_parts[hash_ok], `[[`, character(1), 1L)
    position[hash_ok] <- suppressWarnings(as.integer(vapply(hash_parts[hash_ok], `[[`, character(1), 2L)))
    alt[hash_ok] <- vapply(hash_parts[hash_ok], `[[`, character(1), 3L)
  }

  plain_idx <- which(!hash_ok & !is.na(x) & nzchar(x))
  if (length(plain_idx) > 0L) {
    matched <- regexec("^([A-Za-z*?.-]+)([0-9]+)([A-Za-z*?.-]+)$", x[plain_idx])
    pieces <- regmatches(x[plain_idx], matched)
    ok <- vapply(pieces, length, integer(1)) == 4L
    if (any(ok)) {
      idx <- plain_idx[ok]
      ref[idx] <- vapply(pieces[ok], `[[`, character(1), 2L)
      position[idx] <- suppressWarnings(as.integer(vapply(pieces[ok], `[[`, character(1), 3L)))
      alt[idx] <- vapply(pieces[ok], `[[`, character(1), 4L)
    }
  }

  data.frame(ref = ref, position = position, alt = alt, stringsAsFactors = FALSE)
}

.resolve_protein_mutation_individual_col <- function(tbl, individual_col = "auto") {
  if (is.null(individual_col)) {
    return(NULL)
  }
  if (!is.character(individual_col) || length(individual_col) != 1L ||
      is.na(individual_col) || !nzchar(individual_col)) {
    stop("`individual_col` must be NULL, 'auto', or one column name.", call. = FALSE)
  }
  if (identical(individual_col, "auto")) {
    return(.find_named_column(tbl, c("individual", "species", "strain", "strains", "id")))
  }
  if (!individual_col %in% names(tbl)) {
    stop("Individual column not found: ", individual_col, call. = FALSE)
  }
  individual_col
}

.protein_mutation_individual_index <- function(tbl, individual_col = NULL) {
  if (is.null(individual_col) || !individual_col %in% names(tbl)) {
    return(data.frame(source_row_id = integer(), individual = character(), stringsAsFactors = FALSE))
  }

  out <- lapply(seq_len(nrow(tbl)), function(i) {
    values <- .split_individual_values(tbl[[individual_col]][[i]])
    if (length(values) == 0L) {
      return(data.frame(source_row_id = integer(), individual = character()))
    }
    data.frame(
      source_row_id = tbl$source_row_id[[i]],
      individual = values,
      stringsAsFactors = FALSE
    )
  })
  out <- .bind_rows_base(out)
  if (nrow(out) == 0L) {
    return(data.frame(source_row_id = integer(), individual = character(), stringsAsFactors = FALSE))
  }
  unique(out)
}

.protein_mutation_as_long <- function(tbl, individual_index) {
  if (is.null(individual_index) || nrow(individual_index) == 0L) {
    return(tbl[0L, , drop = FALSE])
  }
  long <- merge(individual_index, tbl, by = "source_row_id", all.x = TRUE, sort = FALSE)
  rownames(long) <- NULL
  long
}

.protein_mutation_subset_for_individual <- function(tbl, individual, individual_index = NULL) {
  if (is.null(individual_index) || nrow(individual_index) == 0L) {
    out <- tbl
    rownames(out) <- NULL
    return(out)
  }
  source_ids <- individual_index$source_row_id[individual_index$individual %in% individual]
  source_ids <- unique(source_ids)
  out <- tbl[tbl$source_row_id %in% source_ids, , drop = FALSE]
  rownames(out) <- NULL
  out
}

.protein_mutation_filter_index <- function(individual_index, source_row_id) {
  if (is.null(individual_index) || nrow(individual_index) == 0L) {
    return(data.frame(source_row_id = integer(), individual = character(), stringsAsFactors = FALSE))
  }
  out <- individual_index[individual_index$source_row_id %in% source_row_id, , drop = FALSE]
  rownames(out) <- NULL
  out
}

.protein_mutation_targets_for_synspecies <- function(x,
                                                     mutation_data,
                                                     individual = NULL,
                                                     individual_index = NULL,
                                                     all = TRUE) {
  if (!is.null(individual)) {
    individual <- as.character(individual)
    individual <- individual[!is.na(individual) & nzchar(individual)]
    return(unique(individual))
  }

  if (!isTRUE(all)) {
    stop(
      "Provide `individual` or set `all = TRUE` when adding protein mutations to a SynSpecies.",
      call. = FALSE
    )
  }

  if (!is.null(individual_index) && nrow(individual_index) > 0L) {
    return(sort(unique(as.character(individual_index$individual))))
  }

  targets <- names(individuals(x))
  targets <- targets[!is.na(targets) & nzchar(targets)]
  if (length(targets) == 0L) {
    stop(
      "Cannot dispatch a mutation table without an individual column into an empty SynSpecies.",
      call. = FALSE
    )
  }
  targets
}

.protein_mutation_metadata <- function(metadata,
                                       individual_col,
                                       source_rows,
                                       imported_rows,
                                       individual) {
  out <- metadata
  out$individual_col <- individual_col
  out$source_rows <- source_rows
  out$imported_rows <- imported_rows
  out$individual <- individual
  out
}

.filter_protein_mutation_query <- function(tbl,
                                           genes = NULL,
                                           event_type = NULL,
                                           min_sample_count = NULL,
                                           strains = NULL,
                                           mutation = NULL,
                                           protein_ranges = NULL,
                                           domains = NULL,
                                           protein_domains = NULL,
                                           ref = NULL,
                                           protein_length = NULL,
                                           individual_index = NULL) {
  out <- as.data.frame(tbl, stringsAsFactors = FALSE)
  if (nrow(out) == 0L) {
    return(out)
  }

  if (!is.null(genes)) {
    genes <- as.character(genes)
    gene_cols <- intersect(c("gene_id", "gene", "gene_name"), names(out))
    if (length(gene_cols) == 0L) {
      return(out[0L, , drop = FALSE])
    }
    keep <- Reduce(`|`, lapply(gene_cols, function(col) as.character(out[[col]]) %in% genes))
    out <- out[keep, , drop = FALSE]
  }

  if (!is.null(event_type) && "event_type" %in% names(out)) {
    out <- out[as.character(out$event_type) %in% as.character(event_type), , drop = FALSE]
  }

  if (!is.null(min_sample_count)) {
    if (!"sample_count" %in% names(out)) {
      return(out[0L, , drop = FALSE])
    }
    out <- out[!is.na(out$sample_count) & out$sample_count >= min_sample_count, , drop = FALSE]
  }

  if (!is.null(mutation) && "mutation" %in% names(out)) {
    out <- out[as.character(out$mutation) %in% as.character(mutation), , drop = FALSE]
  }

  if (!is.null(strains) || !is.null(protein_ranges) ||
      !is.null(protein_domains) || !is.null(ref)) {
    out <- subset_protein_mutations(
      out,
      individuals = strains,
      protein_ranges = protein_ranges,
      domains = domains,
      protein_domains = protein_domains,
      ref = ref,
      protein_length = protein_length,
      individual_index = individual_index
    )
  }

  rownames(out) <- NULL
  out
}

.protein_mutation_table_as_data_frame <- function(x, arg) {
  if (methods::is(x, "SynProteinMutationAnnotation")) {
    return(as.data.frame(protein_mutation_data(x), stringsAsFactors = FALSE))
  }
  if (methods::is(x, "DataFrame")) {
    return(as.data.frame(x, stringsAsFactors = FALSE))
  }
  if (is.data.frame(x)) {
    return(as.data.frame(x, stringsAsFactors = FALSE))
  }
  stop("`", arg, "` must be a data frame-like object.", call. = FALSE)
}

.subset_protein_mutations_by_individual <- function(tbl,
                                                    individuals,
                                                    individual_index = NULL) {
  individuals <- .protein_mutation_character_values(individuals, "individuals")
  if (length(individuals) == 0L) {
    return(tbl[0L, , drop = FALSE])
  }

  if (!is.null(individual_index) && nrow(individual_index) > 0L &&
      "source_row_id" %in% names(tbl)) {
    source_ids <- unique(individual_index$source_row_id[
      as.character(individual_index$individual) %in% individuals
    ])
    return(tbl[tbl$source_row_id %in% source_ids, , drop = FALSE])
  }

  individual_cols <- intersect(c("individual", "species", "strain", "id", "strains"), names(tbl))
  if (length(individual_cols) == 0L) {
    return(tbl[0L, , drop = FALSE])
  }

  keep <- rep(FALSE, nrow(tbl))
  for (col in individual_cols) {
    if (identical(col, "strains")) {
      keep <- keep | vapply(tbl[[col]], function(value) {
        any(.split_individual_values(value) %in% individuals)
      }, logical(1))
    } else {
      keep <- keep | as.character(tbl[[col]]) %in% individuals
    }
  }
  tbl[keep, , drop = FALSE]
}

.subset_protein_mutations_by_ref <- function(tbl, ref) {
  ref <- to_upper_ascii(.protein_mutation_character_values(ref, "ref"))
  invalid <- !grepl("^[A-Z]$", ref)
  if (any(invalid)) {
    stop("`ref` values must be one-letter amino-acid codes.", call. = FALSE)
  }

  ref_col <- .find_named_column(tbl, c("ref", "reference", "reference_aa", "ref_aa", "from"))
  if (is.null(ref_col)) {
    return(tbl[0L, , drop = FALSE])
  }
  tbl[to_upper_ascii(as.character(tbl[[ref_col]])) %in% ref, , drop = FALSE]
}

.subset_protein_mutations_by_coordinates <- function(tbl,
                                                     position = "position",
                                                     protein_ranges = NULL,
                                                     domains = NULL,
                                                     protein_domains = NULL,
                                                     protein_start = 1,
                                                     protein_length = NULL,
                                                     domain_start = "start",
                                                     domain_end = "end",
                                                     domain = NULL) {
  if (!position %in% names(tbl)) {
    stop("Column `", position, "` was not found in mutation data.", call. = FALSE)
  }
  positions <- suppressWarnings(as.numeric(tbl[[position]]))
  if (anyNA(positions)) {
    stop("`position` must identify a numeric protein-coordinate column.", call. = FALSE)
  }

  domain_df <- .protein_mutation_domain_df(
    domains = domains,
    domain_start = domain_start,
    domain_end = domain_end,
    domain = domain
  )
  bounds <- .protein_mutation_bounds(
    positions = positions,
    domain_df = domain_df,
    protein_start = protein_start,
    protein_length = protein_length
  )

  ranges <- data.frame(start = numeric(), end = numeric())
  if (!is.null(protein_ranges)) {
    ranges <- rbind(
      ranges,
      .parse_protein_mutation_ranges(protein_ranges, bounds = bounds)
    )
  }
  if (!is.null(protein_domains)) {
    ranges <- rbind(
      ranges,
      .protein_mutation_ranges_from_domains(
        domain_df = domain_df,
        protein_domains = protein_domains,
        bounds = bounds
      )
    )
  }

  if (nrow(ranges) == 0L) {
    return(tbl[0L, , drop = FALSE])
  }

  keep <- vapply(positions, function(position) {
    any(position >= ranges$start & position <= ranges$end)
  }, logical(1))
  tbl[keep, , drop = FALSE]
}

.protein_mutation_domain_df <- function(domains,
                                        domain_start = "start",
                                        domain_end = "end",
                                        domain = NULL) {
  if (is.null(domains)) {
    return(data.frame(start = numeric(), end = numeric(), domain = character()))
  }
  if (methods::is(domains, "SynProteinDomainAnnotation")) {
    domains <- query_domains(domains)
  }
  domain_df <- .protein_mutation_table_as_data_frame(domains, "domains")
  if (nrow(domain_df) == 0L) {
    return(data.frame(start = numeric(), end = numeric(), domain = character()))
  }

  if (!domain_start %in% names(domain_df)) {
    stop("Column `", domain_start, "` was not found in domain data.", call. = FALSE)
  }
  if (!domain_end %in% names(domain_df)) {
    stop("Column `", domain_end, "` was not found in domain data.", call. = FALSE)
  }

  domain <- domain %||% .find_named_column(
    domain_df,
    c(
      "domain", "Domain", "domain_name", "motif", "interpro_description",
      "signature_description", "interpro_accession", "signature_accession",
      "analysis", "Model", "model", "name", "lollipop_domain"
    )
  )
  if (is.null(domain)) {
    stop("Could not infer a domain-name column. Supply `domain` explicitly.", call. = FALSE)
  }

  out <- data.frame(
    start = suppressWarnings(as.numeric(domain_df[[domain_start]])),
    end = suppressWarnings(as.numeric(domain_df[[domain_end]])),
    domain = as.character(domain_df[[domain]]),
    stringsAsFactors = FALSE
  )
  if (anyNA(out$start) || anyNA(out$end)) {
    stop("Domain start/end columns must be numeric.", call. = FALSE)
  }
  flipped <- out$start > out$end
  if (any(flipped)) {
    old_start <- out$start[flipped]
    out$start[flipped] <- out$end[flipped]
    out$end[flipped] <- old_start
  }
  out
}

.protein_mutation_bounds <- function(positions,
                                     domain_df = NULL,
                                     protein_start = 1,
                                     protein_length = NULL) {
  protein_start <- suppressWarnings(as.numeric(protein_start))
  if (length(protein_start) != 1L || is.na(protein_start) || !is.finite(protein_start)) {
    stop("`protein_start` must be a finite numeric scalar.", call. = FALSE)
  }

  if (!is.null(protein_length)) {
    protein_length <- suppressWarnings(as.numeric(protein_length))
    if (length(protein_length) != 1L || is.na(protein_length) || !is.finite(protein_length)) {
      stop("`protein_length` must be a finite numeric scalar.", call. = FALSE)
    }
    if (protein_length < protein_start) {
      stop("`protein_length` must be greater than or equal to `protein_start`.", call. = FALSE)
    }
    return(c(start = protein_start, end = protein_length))
  }

  candidates <- positions[is.finite(positions)]
  if (!is.null(domain_df) && nrow(domain_df) > 0L) {
    candidates <- c(candidates, domain_df$end[is.finite(domain_df$end)])
  }
  if (length(candidates) == 0L) {
    stop("Cannot infer protein-coordinate boundaries from empty data.", call. = FALSE)
  }
  c(start = protein_start, end = max(candidates, protein_start))
}

.parse_protein_mutation_ranges <- function(protein_ranges, bounds) {
  ranges <- .protein_mutation_character_values(protein_ranges, "protein_ranges")
  parsed <- lapply(ranges, function(range) {
    matched <- regexec("^\\s*([0-9]+)\\s*-\\s*([0-9]+)\\s*$", range)
    pieces <- regmatches(range, matched)[[1L]]
    if (length(pieces) != 3L) {
      stop(
        "`protein_ranges` values must use the form 'start-end'. Invalid value: ",
        range,
        call. = FALSE
      )
    }
    start <- as.numeric(pieces[[2L]])
    end <- as.numeric(pieces[[3L]])
    if (start > end) {
      old_start <- start
      start <- end
      end <- old_start
    }
    start <- max(bounds[["start"]], min(start, bounds[["end"]]))
    end <- max(bounds[["start"]], min(end, bounds[["end"]]))
    data.frame(start = start, end = end)
  })
  do.call(rbind, parsed)
}

.protein_mutation_ranges_from_domains <- function(domain_df,
                                                  protein_domains,
                                                  bounds) {
  if (is.null(domain_df) || nrow(domain_df) == 0L) {
    stop("`domains` must be supplied when filtering by `protein_domains`.", call. = FALSE)
  }
  protein_domains <- .protein_mutation_character_values(protein_domains, "protein_domains")
  hit <- domain_df[as.character(domain_df$domain) %in% protein_domains, , drop = FALSE]
  if (nrow(hit) == 0L) {
    return(data.frame(start = numeric(), end = numeric()))
  }
  start <- pmax(bounds[["start"]], pmin(hit$start, bounds[["end"]]))
  end <- pmax(bounds[["start"]], pmin(hit$end, bounds[["end"]]))
  data.frame(start = start, end = end)
}

.protein_mutation_character_values <- function(x, arg) {
  if (is.null(x)) {
    return(character())
  }
  values <- unlist(x, recursive = TRUE, use.names = FALSE)
  values <- as.character(values)
  values <- trimws(values)
  values <- values[!is.na(values) & nzchar(values)]
  unique(values)
}

.find_named_column <- function(tbl, candidates) {
  normalized_names <- .normalize_column_name(names(tbl))
  normalized_candidates <- .normalize_column_name(candidates)
  hit <- match(normalized_candidates, normalized_names, nomatch = 0L)
  hit <- hit[hit > 0L]
  if (length(hit) == 0L) {
    return(NULL)
  }
  names(tbl)[hit[[1L]]]
}

.normalize_column_name <- function(x) {
  x <- to_lower_ascii(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

.coalesce_chr <- function(x, y) {
  missing_x <- is.na(x) | !nzchar(x)
  x[missing_x] <- y[missing_x]
  x
}

.split_individual_values <- function(x) {
  if (length(x) == 0L || is.na(x) || !nzchar(as.character(x))) {
    return(character())
  }
  values <- unlist(strsplit(as.character(x), ",", fixed = TRUE), use.names = FALSE)
  values <- trimws(values)
  values[!is.na(values) & nzchar(values)]
}

.bind_rows_base <- function(x) {
  x <- Filter(function(one) is.data.frame(one) && nrow(one) > 0L, x)
  if (length(x) == 0L) {
    return(data.frame())
  }
  out <- do.call(rbind, x)
  rownames(out) <- NULL
  out
}
