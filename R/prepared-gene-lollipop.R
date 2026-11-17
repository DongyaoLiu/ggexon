#' Prepare a shared gene-lollipop grid
#'
#' Selects native gene features and assigns a common, device-independent grid
#' to all participating individuals. Empty cells remain reserved. Automatic
#' ordinal copy correspondence is a display convention, not copy orthology.
#'
#' @param x A SynIndividual or SynSpecies.
#' @param name Name of the prepared grid.
#' @param individuals Optional participating individual IDs.
#' @param annotation Optional source feature-annotation name; defaults to each
#'   individual's active feature annotation.
#' @param genes Optional gene IDs/names, or a list named by individual.
#' @param where Named list of metadata values. Alternatives within each field
#'   are combined with OR; different fields are combined with AND.
#' @param key Optional metadata column identifying shared genes.
#' @param homology Optional stored homology-annotation names. Explicit `key`
#'   and explicit `homology` cannot be combined.
#' @param copy_key Optional metadata column identifying corresponding copies.
#' @param reference Reference individual ID. Required for multiple individuals
#'   unless a unique common homology reference can be inferred.
#' @param nrow,ncol Positive integer grid dimensions. Supply at least one.
#' @param order Optional complete shared-key order, overriding reference/flanks.
#' @param slots Optional partial table with `shared_key`, optional `copy_id`,
#'   `head_row` and `head_col`. Rows are numbered bottom to top.
#' @param label,fill Optional source metadata columns. Defaults prefer nonempty
#'   `plot_label` and `plot_fill`, then gene names/IDs and grey fill.
#' @return The updated Syn object. Inspect it with [query_gene_lollipop()].
#' @export
prepare_gene_lollipop <- function(x, name = "gene_lollipop", individuals = NULL,
    annotation = NULL, genes = NULL, where = NULL, key = NULL, homology = NULL,
    copy_key = NULL, reference = NULL, nrow = NULL, ncol = NULL, order = NULL,
    slots = NULL, label = NULL, fill = NULL) {
  .genomic_name(name, "name")
  all <- .gene_lollipop_individuals(x)
  ids <- if (is.null(individuals)) names(all) else unique(as.character(individuals))
  if (!length(ids) || anyNA(ids) || any(!ids %in% names(all)))
    stop("`individuals` must select existing individuals.", call. = FALSE)
  all <- all[ids]
  for (arg in c("annotation", "key", "copy_key", "label", "fill")) {
    value <- get(arg)
    if (!is.null(value)) .genomic_name(value, arg)
  }
  if (!is.null(where) && (!is.list(where) || is.null(names(where)) ||
      anyNA(names(where)) || any(!nzchar(names(where))) || anyDuplicated(names(where))))
    stop("`where` must be a named list of metadata values.", call. = FALSE)
  if (is.list(genes) && (is.null(names(genes)) || any(!names(genes) %in% ids) || anyDuplicated(names(genes))))
    stop("List `genes` must be named by participating individual.", call. = FALSE)
  if (!is.null(key) && !is.null(homology))
    stop("Supply explicit `key` or explicit `homology`, not both.", call. = FALSE)
  homologies <- .gene_lollipop_homologies(x, ids, homology, use = is.null(key))
  if (is.null(reference)) {
    refs <- unique(vapply(homologies, function(h) h@reference_species, character(1)))
    reference <- if (length(ids) == 1L) ids else if (length(refs) == 1L) refs else NULL
  }
  if (length(reference) != 1L || is.na(reference) || !reference %in% ids)
    stop("Supply `reference` as one participating individual; no unique reference can be inferred.", call. = FALSE)
  homologies <- homologies[vapply(homologies, function(h) identical(h@reference_species, reference), logical(1))]
  if (!is.null(homology) && length(homologies) != length(homology))
    stop("Selected homology annotations must map participating individuals to `reference`.", call. = FALSE)
  recipes <- lapply(ids, function(id) list(annotation = annotation %||% active_feature_annotation(all[[id]]),
    genes = if (is.list(genes)) genes[[id]] else genes, where = where))
  names(recipes) <- ids
  sources <- lapply(ids, function(id) .gene_lollipop_source_rows(all[[id]], recipes[[id]]))
  names(sources) <- ids
  signatures <- vapply(sources, rlang::hash, character(1))
  # Capture source copy identities before derived fields overwrite metadata
  # names such as copy_id, individual, label, fill or shared_key.
  source_copies <- if (is.null(copy_key)) NULL else
    unlist(lapply(sources, .gene_lollipop_column, column = copy_key), use.names = FALSE)
  rows <- dplyr::bind_rows(lapply(ids, function(id) {
    out <- .gene_lollipop_gene_rows(sources[[id]], id, recipes[[id]]$annotation, label, fill)
    out$shared_key <- if (is.null(key)) {
      if (id == reference || !nrow(out)) out$gene_id else paste0(id, "::", out$gene_id)
    } else {
      .gene_lollipop_column(sources[[id]], key)
    }
    missing <- is.na(out$shared_key) | !nzchar(out$shared_key)
    out$shared_key[missing] <- paste0(id, "::", out$gene_id[missing])
    out
  }))
  if (!nrow(rows)) stop("No gene features matched the preparation selectors.", call. = FALSE)
  if (is.null(key)) rows <- .gene_lollipop_homology_keys(rows, homologies, reference)
  keys <- .gene_lollipop_key_order(rows, reference, order)
  rows$copy_id <- character(nrow(rows))
  groups <- split(seq_len(nrow(rows)), .gene_lollipop_slot_key(rows$individual, rows$shared_key))
  for (idx in groups) {
    idx <- idx[base::order(rows$x[idx], rows$gene_id[idx])]
    copies <- if (is.null(copy_key)) as.character(seq_along(idx)) else source_copies[idx]
    if (anyNA(copies) || any(!nzchar(copies)) || anyDuplicated(copies))
      stop("`copy_key` must be complete and unique within each individual/shared-key group.", call. = FALSE)
    rows$copy_id[idx] <- copies
  }
  slot_rows <- dplyr::bind_rows(lapply(keys, function(shared) {
    selected <- rows[rows$shared_key == shared, , drop = FALSE]
    # Prefer the reference's native copy order; append nonreference copies
    # deterministically. Ordinal copies retain numerical order above nine.
    selected <- selected[base::order(selected$individual != reference,
      match(selected$individual, ids), selected$x, selected$gene_id), , drop = FALSE]
    copies <- unique(selected$copy_id)
    if (is.null(copy_key)) copies <- as.character(seq_len(max(as.integer(copies))))
    data.frame(shared_key = shared, copy_id = copies, stringsAsFactors = FALSE)
  }))
  slot_rows$slot_key <- .gene_lollipop_slot_key(slot_rows$shared_key, slot_rows$copy_id)
  slot_rows$slot_order <- seq_len(nrow(slot_rows))
  slot_rows$order_auto <- is.null(order)
  slot_rows$copy_auto <- is.null(copy_key)
  dims <- .gene_lollipop_dimensions(nrow, ncol, nrow(slot_rows))
  slot_rows <- .gene_lollipop_assign_cells(slot_rows, slots, dims)
  rows$slot_key <- .gene_lollipop_slot_key(rows$shared_key, rows$copy_id)
  at <- match(rows$slot_key, slot_rows$slot_key)
  for (field in c("head_row", "head_col", "slot_order", "order_auto", "copy_auto", "slot_explicit"))
    rows[[field]] <- slot_rows[[field]][at]
  slot_rows$occupied_by <- lapply(slot_rows$slot_key, function(k) ids[ids %in% rows$individual[rows$slot_key == k]])
  slot_rows$n_occupants <- lengths(slot_rows$occupied_by)
  recipe <- list(individuals = ids, sources = recipes, key = key,
    homology = names(homologies), homology_auto = is.null(key) && is.null(homology),
    copy_key = copy_key, reference = reference,
    key_order = keys, order = order, nrow = dims[[1L]], ncol = dims[[2L]],
    slots = slots, label = label, fill = fill)
  homology_signature <- .gene_lollipop_homology_signature(homologies)
  grid <- rlang::hash(list(recipe, signatures, homology_signature, slot_rows))
  rows$grid_nrow <- slot_rows$grid_nrow <- dims[[1L]]
  rows$grid_ncol <- slot_rows$grid_ncol <- dims[[2L]]
  rows$lollipop_grid <- slot_rows$lollipop_grid <- grid
  for (id in ids) {
    ind <- all[[id]]
    ind@plot_cache$gene_lollipop_layouts[[name]] <- list(name = name, grid = grid,
      genes = rows[rows$individual == id, , drop = FALSE], slots = slot_rows, labels = rows$label,
      recipe = recipe, signatures = signatures, homology_signature = homology_signature)
    ind@metadata$.ggexon_gene_lollipop[[name]] <- list(grid = grid,
      annotation = recipes[[id]]$annotation)
    all[[id]] <- ind
  }
  if (methods::is(x, "SynIndividual")) return(all[[1L]])
  x@individuals[ids] <- all
  x
}

#' Inspect a prepared gene-lollipop grid
#'
#' Source changes and cleared caches require preparation again. Slot occupancy
#' describes selected annotation features and does not establish gene absence.
#' An extracted individual retains the complete shared grid.
#' @param x A SynIndividual or SynSpecies containing a preparation.
#' @param name Optional preparation name; inferred only when unambiguous.
#' @param component Return native `"genes"` or the common `"slots"` table.
#' @return A data frame. `occupied_by` is a list column of individual IDs.
#' @export
query_gene_lollipop <- function(x, name = NULL, component = c("genes", "slots")) {
  component <- match.arg(component)
  all <- .gene_lollipop_individuals(x)
  name <- .gene_lollipop_name(all, name)
  entries <- .gene_lollipop_entries(x, all, name)
  if (component == "slots") return(entries[[1L]]$slots)
  out <- dplyr::bind_rows(lapply(entries, `[[`, "genes"))
  rownames(out) <- NULL
  out
}

.gene_lollipop_individuals <- function(x) {
  if (methods::is(x, "SynIndividual")) return(stats::setNames(list(x), syn_id(x)))
  if (methods::is(x, "SynSpecies")) return(individuals(x))
  stop("Supply a SynIndividual or SynSpecies.", call. = FALSE)
}

.gene_lollipop_column <- function(rows, column) {
  if (!column %in% names(rows)) stop("Source gene metadata has no column `", column, "`.", call. = FALSE)
  if (is.list(rows[[column]]) || methods::is(rows[[column]], "List"))
    stop("Source metadata column `", column, "` must have one value per gene.", call. = FALSE)
  as.character(rows[[column]])
}

.gene_lollipop_source_rows <- function(individual, recipe) {
  annotation <- get_annotation(individual, recipe$annotation)
  if (!methods::is(annotation, "SynFeatureAnnotation"))
    stop("The source must be a SynFeatureAnnotation.", call. = FALSE)
  gr <- query_features(annotation, genes = recipe$genes, feature_type = "gene", all = TRUE)
  meta <- as.data.frame(S4Vectors::mcols(gr), stringsAsFactors = FALSE)
  keep <- rep(TRUE, length(gr))
  for (field in names(recipe$where)) {
    values <- .gene_lollipop_column(meta, field)
    keep <- keep & values %in% recipe$where[[field]]
  }
  out <- data.frame(seqnames = as.character(GenomeInfoDb::seqnames(gr)),
    xmin = as.numeric(IRanges::start(gr)), xmax = as.numeric(IRanges::end(gr)),
    strand = as.character(BiocGenerics::strand(gr)), stringsAsFactors = FALSE)
  extra <- setdiff(names(meta), c(names(out), "PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y"))
  out[extra] <- meta[extra]
  out <- out[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

.gene_lollipop_gene_rows <- function(source, id, annotation, label, fill) {
  out <- source
  meta <- S4Vectors::DataFrame(source)
  out$gene_id <- .coalesce_character_cols(meta, c("gene_id", "ID", "gene_name", "Name"))
  if (anyNA(out$gene_id) || any(!nzchar(out$gene_id)) || anyDuplicated(out$gene_id))
    stop("Selected genes require unique stable feature IDs within each individual.", call. = FALSE)
  out$gene_name <- .coalesce_character_cols(meta, c("gene_name", "Name", "gene_id", "ID"))
  out$label <- if (is.null(label)) .coalesce_character_cols(meta, c("plot_label", "gene_name", "Name", "gene_id", "ID")) else .gene_lollipop_column(source, label)
  out$fill <- if (is.null(fill)) .coalesce_character_cols(meta, c("plot_fill", "fill")) else .gene_lollipop_column(source, fill)
  out$fill[is.na(out$fill) | !nzchar(out$fill)] <- "grey75"
  out$x <- (out$xmin + out$xmax) / 2
  out$y <- rep(0, nrow(out))
  out$genomic_x <- out$x
  out$genomic_xmin <- out$xmin
  out$genomic_xmax <- out$xmax
  out$track <- out$individual <- rep(id, nrow(out))
  out$source_annotation <- rep(annotation, nrow(out))
  out
}

.gene_lollipop_homologies <- function(x, ids, names = NULL, use = TRUE) {
  all <- if (methods::is(x, "SynSpecies")) homology_annotations(x) else list()
  if (!is.null(names)) {
    if (!is.character(names) || !length(names) || anyNA(names) || any(!names %in% base::names(all)))
      stop("`homology` must name stored compatible homology annotations.", call. = FALSE)
    return(all[unique(names)])
  }
  if (!use) return(list())
  all[vapply(all, function(h) h@reference_species %in% ids && h@query_species %in% ids, logical(1))]
}

.gene_lollipop_homology_signature <- function(homologies) {
  rlang::hash(lapply(homologies, function(h) list(reference = h@reference_species,
    query = h@query_species, table = h@homology_table)))
}

.gene_lollipop_id_aliases <- function(rows) {
  columns <- intersect(c("gene_id", "gene_name", "transcript_id", "Parent", "ID",
    "homology_query_aliases"), names(rows))
  aliases <- lapply(seq_len(nrow(rows)), function(i) character())
  for (column in columns) {
    values <- .homology_split_alias_values(rows[[column]])
    for (i in seq_along(aliases)) {
      normalized <- .normalize_gene_id(values[[i]])
      aliases[[i]] <- unique(c(aliases[[i]], normalized[!is.na(normalized) & nzchar(normalized)]))
    }
  }
  aliases
}

.gene_lollipop_homology_keys <- function(rows, homologies, reference) {
  ref <- rows[rows$individual == reference, , drop = FALSE]
  ref_aliases <- .gene_lollipop_id_aliases(ref)
  aliases <- .gene_lollipop_id_aliases(rows)
  for (i in which(rows$individual != reference)) {
    hits <- character()
    for (h in homologies) {
      if (!identical(h@query_species, rows$individual[[i]])) next
      table <- h@homology_table
      # Only query-side table hits establish identity. In particular, matching
      # display labels to table reference names is not homology evidence.
      match_rows <- .normalize_gene_id(table$query_gene) %in% aliases[[i]]
      mapped <- as.character(table$reference_gene[match_rows])
      mapped <- mapped[!is.na(mapped) & nzchar(mapped)]
      for (j in seq_along(mapped)) {
        normalized <- .normalize_gene_id(mapped[[j]])
        at <- which(vapply(ref_aliases, function(a) normalized %in% a, logical(1)))
        if (length(at) > 1L)
          stop("Ambiguous reference homology identity `", mapped[[j]],
            "`; supply an explicit metadata `key`.", call. = FALSE)
        mapped[[j]] <- if (length(at)) ref$gene_id[[at]] else normalized
      }
      hits <- c(hits, mapped)
    }
    hits <- unique(hits)
    if (length(hits) > 1L) stop("Conflicting homology mappings for `", rows$gene_id[[i]],
      "`; select explicit `homology` annotations.", call. = FALSE)
    if (length(hits)) rows$shared_key[[i]] <- hits[[1L]]
  }
  rows
}

.gene_lollipop_key_order <- function(rows, reference, explicit) {
  keys <- unique(rows$shared_key)
  if (!is.null(explicit)) {
    if (!is.character(explicit) || anyNA(explicit) || anyDuplicated(explicit) || !setequal(keys, explicit))
      stop("`order` must be a complete ordering of all shared keys: ", paste(keys, collapse = ", "), ".", call. = FALSE)
    return(explicit)
  }
  ref <- rows[rows$individual == reference, , drop = FALSE]
  ref <- ref[base::order(ref$x, ref$gene_id), , drop = FALSE]
  reference_keys <- unique(ref$shared_key)
  edges <- matrix(character(), ncol = 2L)
  add <- function(a, b) {
    if (length(a) && length(b) && a != b) edges <<- rbind(edges, c(a, b))
  }
  if (length(reference_keys) > 1L)
    for (j in seq_len(length(reference_keys) - 1L)) add(reference_keys[[j]], reference_keys[[j + 1L]])
  for (idx in split(seq_len(nrow(rows)), .gene_lollipop_slot_key(rows$individual, rows$seqnames))) {
    idx <- idx[base::order(rows$x[idx], rows$gene_id[idx])]
    sequence <- rows$shared_key[idx]
    # Consecutive copies of a key contribute one flank, while separated copies
    # must remain visible to detect a private run enclosed by the same key.
    sequence <- sequence[c(TRUE, sequence[-1L] != sequence[-length(sequence)])]
    missing <- !sequence %in% reference_keys
    runs <- split(which(missing), cumsum(c(TRUE, diff(which(missing)) != 1L)))
    for (run in runs) {
      if (!length(run)) next
      first <- min(run)
      last <- max(run)
      left <- if (first > 1L) sequence[[first - 1L]] else character()
      right <- if (last < length(sequence)) sequence[[last + 1L]] else character()
      chain <- c(left, sequence[run], right)
      # Orient only this private run using its reference flanks. Native
      # coordinates/strands and rearranged reference genes remain untouched.
      if (length(left) && length(right) &&
          match(left, reference_keys) > match(right, reference_keys)) chain <- rev(chain)
      if (length(chain) > 1L)
        for (j in seq_len(length(chain) - 1L)) add(chain[[j]], chain[[j + 1L]])
    }
  }

  remaining <- keys
  result <- character()
  while (length(remaining)) {
    incoming <- edges[edges[, 1L] %in% remaining & edges[, 2L] %in% remaining, 2L]
    available <- setdiff(remaining, incoming)
    if (length(available) != 1L) {
      unresolved <- setdiff(remaining, reference_keys)
      if (!length(unresolved)) unresolved <- remaining
      flanks <- if (nrow(edges)) paste(apply(edges, 1, paste, collapse = " -> "), collapse = "; ") else "none"
      stop("Reference-flank order is ambiguous or contradictory for ", paste(unresolved, collapse = ", "),
        ". Flanking constraints: ", flanks, ". Supply a complete `order`.", call. = FALSE)
    }
    result <- c(result, available)
    remaining <- setdiff(remaining, available)
  }
  result
}

.gene_lollipop_dimensions <- function(rows, cols, count) {
  for (value in list(rows, cols)) if (!is.null(value) &&
      (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 1 || value != floor(value)))
    stop("`nrow` and `ncol` must be positive integers.", call. = FALSE)
  if (is.null(rows) && is.null(cols)) stop("Supply at least one of `nrow` or `ncol`.", call. = FALSE)
  rows <- rows %||% ceiling(count / cols)
  cols <- cols %||% ceiling(count / rows)
  if (rows * cols < count) stop("The declared grid has insufficient capacity for all gene-copy slots.", call. = FALSE)
  as.integer(c(rows, cols))
}

.gene_lollipop_slot_key <- function(key, copy) paste0(nchar(key), ":", key, ":", nchar(copy), ":", copy)

.gene_lollipop_assign_cells <- function(out, overrides, dims) {
  nr <- dims[[1L]]
  nc <- dims[[2L]]
  fixed <- rep(NA_integer_, nrow(out))
  if (!is.null(overrides)) {
    if (!is.data.frame(overrides)) stop("`slots` must be a data frame.", call. = FALSE)
    aliases <- c(key = "shared_key", copy = "copy_id", row = "head_row", col = "head_col")
    for (from in names(aliases)) if (from %in% names(overrides) && !aliases[[from]] %in% names(overrides))
      names(overrides)[names(overrides) == from] <- aliases[[from]]
    required <- c("shared_key", "head_row", "head_col")
    if (!all(required %in% names(overrides))) stop("`slots` requires shared_key, head_row and head_col.", call. = FALSE)
    if (!"copy_id" %in% names(overrides)) {
      if (any(out$shared_key[duplicated(out$shared_key)] %in% overrides$shared_key))
        stop("Supply `copy_id` for duplicated shared keys in `slots`.", call. = FALSE)
      overrides$copy_id <- out$copy_id[match(overrides$shared_key, out$shared_key)]
    }
    at <- match(.gene_lollipop_slot_key(overrides$shared_key, overrides$copy_id), out$slot_key)
    if (anyNA(at) || anyDuplicated(at)) stop("`slots` contains unknown or repeated key/copy identities.", call. = FALSE)
    r <- overrides$head_row
    c <- overrides$head_col
    if (!is.numeric(r) || !is.numeric(c) || any(!is.finite(r)) || any(!is.finite(c)) ||
        any(r < 1 | r > nr | r != floor(r)) || any(c < 1 | c > nc | c != floor(c)))
      stop("Explicit slots require integer rows/columns inside the declared grid.", call. = FALSE)
    position <- (c - 1L) * nr + nr - r + 1L
    if (anyDuplicated(position)) stop("Explicit slots collide in the same cell.", call. = FALSE)
    fixed[at] <- as.integer(position)
  }
  position <- fixed
  previous <- 0L
  for (i in seq_len(nrow(out))) {
    if (is.na(position[[i]])) position[[i]] <- previous + 1L
    if (position[[i]] <= previous || position[[i]] > nr * nc)
      stop("Explicit slots contradict the requested order or leave insufficient capacity.", call. = FALSE)
    previous <- position[[i]]
  }
  out$head_row <- as.integer(nr - (position - 1L) %% nr)
  out$head_col <- as.integer((position - 1L) %/% nr + 1L)
  out$slot_explicit <- !is.na(fixed)
  out
}

.gene_lollipop_names <- function(all) {
  unique(unlist(lapply(all, function(ind) union(names(ind@plot_cache$gene_lollipop_layouts),
    names(ind@metadata$.ggexon_gene_lollipop))), use.names = FALSE))
}

.gene_lollipop_name <- function(all, name = NULL, allow_none = FALSE) {
  available <- .gene_lollipop_names(all)
  if (!is.null(name)) {
    .genomic_name(name, "lollipop_layout")
    if (!name %in% available) stop("Unknown prepared gene-lollipop layout `", name, "`.", call. = FALSE)
    return(name)
  }
  if (!length(available)) {
    if (allow_none) return(NULL)
    stop("No prepared gene-lollipop grid is available; run `prepare_gene_lollipop()`.", call. = FALSE)
  }
  if (length(available) > 1L) stop("Multiple prepared gene-lollipop grids are applicable; supply a name with `lollipop_layout`.", call. = FALSE)
  available[[1L]]
}

.gene_lollipop_entries <- function(x, all, name) {
  entries <- lapply(all, function(ind) {
    entry <- ind@plot_cache$gene_lollipop_layouts[[name]]
    registered <- ind@metadata$.ggexon_gene_lollipop[[name]]
    if (is.null(entry) && !is.null(registered))
      stop("Prepared gene-lollipop grid `", name, "` was invalidated by a cleared cache; run `prepare_gene_lollipop()` again.", call. = FALSE)
    if (is.null(entry)) return(NULL)
    if (!is.null(registered) && !identical(entry$grid, registered$grid))
      stop("Prepared gene-lollipop grids are incompatible; prepare them again.", call. = FALSE)
    current <- tryCatch(rlang::hash(.gene_lollipop_source_rows(ind,
      entry$recipe$sources[[syn_id(ind)]])), error = function(e) NULL)
    if (is.null(current) || !identical(current, unname(entry$signatures[[syn_id(ind)]])))
      stop("Prepared gene-lollipop grid `", name, "` is stale: source features or metadata changed; run `prepare_gene_lollipop()` again.", call. = FALSE)
    entry
  })
  entries <- Filter(Negate(is.null), entries)
  if (!length(entries)) stop("No applicable prepared gene-lollipop grid `", name, "`.", call. = FALSE)
  grids <- unique(vapply(entries, `[[`, character(1), "grid"))
  if (length(grids) != 1L) stop("Prepared gene-lollipop grid identities are incompatible; prepare them together again.", call. = FALSE)
  if (methods::is(x, "SynSpecies")) {
    entry <- entries[[1L]]
    source_ids <- intersect(entry$recipe$individuals, names(individuals(x)))
    # Even when dispatching only one track, validate all available participants.
    extra_ids <- setdiff(source_ids, names(all))
    if (length(setdiff(intersect(source_ids, names(all)), names(entries))))
      stop("A participating gene-lollipop grid is missing; prepare participants together again.", call. = FALSE)
    if (length(extra_ids)) {
      extra <- .gene_lollipop_entries(individuals(x)[[extra_ids[[1L]]]], individuals(x)[extra_ids], name)
      if (length(extra) != length(extra_ids) ||
          any(vapply(extra, function(e) !identical(e$grid, entry$grid), logical(1))))
        stop("Prepared gene-lollipop grid identities are incompatible; prepare them together again.", call. = FALSE)
    }
    homology_names <- entry$recipe$homology
    current_homologies <- if (isTRUE(entry$recipe$homology_auto)) {
      discovered <- .gene_lollipop_homologies(x, entry$recipe$individuals)
      discovered[vapply(discovered, function(h) identical(h@reference_species, entry$recipe$reference), logical(1))]
    } else homology_annotations(x)[homology_names]
    if (any(!homology_names %in% base::names(homology_annotations(x))) ||
        !identical(entry$homology_signature, .gene_lollipop_homology_signature(current_homologies)))
      stop("Prepared gene-lollipop homology inputs changed; run `prepare_gene_lollipop()` again.", call. = FALSE)
  }
  entries
}

.gene_lollipop_prepared_data <- function(x, layer) {
  params <- syn_layer_params(layer)
  context <- layer$syn_plot_context %||% NULL
  tracks <- resolve_context_species_params(x, params$species, context)
  ids <- vapply(tracks, function(track) .context_individual_for_track(x, track, context), character(1))
  all <- .gene_lollipop_individuals(x)
  relevant <- all[intersect(unique(ids), names(all))]
  name <- .gene_lollipop_name(relevant, params$lollipop_layout, allow_none = TRUE)
  if (is.null(name)) return(NULL)
  entries <- .gene_lollipop_entries(x, relevant, name)
  out <- dplyr::bind_rows(lapply(seq_along(tracks), function(i) {
    entry <- entries[[ids[[i]]]]
    if (is.null(entry)) return(NULL)
    rows <- entry$genes
    window <- normalize_syn_window_request(x, species = tracks[[i]], chr = params$chr,
      subset = params$subset, context = context, geom = "geom_gene_lollipop")
    keep <- rep(TRUE, nrow(rows))
    if (!is.null(window$chr)) keep <- keep & rows$seqnames %in% window$chr
    if (!is.null(window$start)) keep <- keep & rows$x >= window$start
    if (!is.null(window$end)) keep <- keep & rows$x <= window$end
    rows <- rows[keep, , drop = FALSE]
    rows$track <- rep(tracks[[i]], nrow(rows))
    rows
  }))
  # Keep an empty prepared result distinguishable from absent preparation.
  if (!nrow(out)) out <- entries[[1L]]$genes[0, , drop = FALSE]
  attr(out, "ggexon_gene_lollipop") <- name
  attr(out, "gene_lollipop_labels") <- entries[[1L]]$labels
  out
}
