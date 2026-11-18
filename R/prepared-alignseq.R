#' Store anchored DNA sequence detail on a Syn object
#'
#' Prepare native nucleotide and event rows from two already aligned DNA
#' strings using [alignseq_data()]. The participating individuals store the
#' prepared rows and input recipe under the supplied name in [plot_cache()].
#' Preparing an existing name replaces that alignment across the supplied
#' object.
#'
#' @inheritParams alignseq_data
#' @param x A SynIndividual or SynSpecies.
#' @param anchors A data frame with one row per aligned sequence and columns
#'   `sequence`, `start`, and `strand` as in [alignseq_data()]. The optional
#'   `track` column identifies existing individuals and defaults to the
#'   sequence names. Two sequences can refer to the same individual. Optional
#'   `chr` and `y` columns supply chromosome and vertical position.
#' @param name Name of the prepared alignment.
#' @param boundaries Optional data frame with `sequence`, `position`, and
#'   `label` columns. Each position identifies the native genomic base after
#'   which to draw a boundary in the supplied alignment orientation. Use an
#'   empty label to draw an unlabelled boundary.
#' @return The updated Syn object. Inspect it with [query_alignseq()] or
#'   display it with [geom_alignseq()].
#' @export
prepare_alignseq <- function(x, aligned, anchors, reference, name = "alignseq",
                             regions = NULL, details = NULL, flank = 3,
                             boundaries = NULL) {
  .genomic_name(name, "name")
  all <- .prepared_alignseq_individuals(x)
  rows <- alignseq_data(aligned, anchors, reference, regions, details, flank)
  ids <- unique(as.character(anchors[["track"]] %||% anchors$sequence))
  if (any(!ids %in% names(all))) {
    stop("`anchors$track` must identify existing individuals",
      " (defaulting to the aligned sequence names).", call. = FALSE)
  }
  rows$individual <- rows$track
  rows$boundary_after <- rep(FALSE, nrow(rows))
  rows$boundary_label <- rep("", nrow(rows))
  if (!is.null(boundaries)) {
    rows <- .prepared_alignseq_boundaries(rows, aligned, anchors, boundaries)
  }

  recipe <- list(aligned = aligned, anchors = anchors, reference = reference,
    regions = regions, details = details, flank = flank, boundaries = boundaries)
  for (id in names(all)) {
    ind <- all[[id]]
    if (id %in% ids) {
      data <- rows[rows$individual == id, , drop = FALSE]
      rownames(data) <- NULL
      ind@plot_cache[["alignseq"]][[name]] <- list(data = data, recipe = recipe)
      ind@metadata[[".ggexon_alignseq"]][[name]] <- TRUE
    } else {
      ind@plot_cache[["alignseq"]][[name]] <- NULL
      ind@metadata[[".ggexon_alignseq"]][[name]] <- NULL
    }
    all[[id]] <- ind
  }
  if (methods::is(x, "SynIndividual")) return(all[[1L]])
  x@individuals <- all
  x
}

#' Query stored DNA sequence detail
#'
#' Select prepared sequence rows in native genomic coordinates. Run
#' [prepare_alignseq()] again if the preparation was cleared from the plot
#' cache.
#'
#' @param x A SynIndividual or SynSpecies containing a prepared alignment.
#' @param name Optional preparation name. Omit it when the selected individuals
#'   have exactly one prepared alignment name.
#' @param individual Optional character vector of individual IDs.
#' @param chr Optional character vector of chromosome names.
#' @param start,end Optional inclusive lower and upper bounds on the native
#'   coordinate `x`, including half-base positions for gaps.
#' @return A data frame with the [alignseq_data()] columns and `individual`,
#'   `boundary_after`, and `boundary_label`. Empty selections retain the
#'   same columns and types.
#' @export
query_alignseq <- function(x, name = NULL, individual = NULL, chr = NULL,
                           start = NULL, end = NULL) {
  all <- .prepared_alignseq_individuals(x)
  selected <- all
  if (!is.null(individual)) {
    if (!is.character(individual) || any(!individual %in% names(all))) {
      stop("`individual` must select existing individuals.", call. = FALSE)
    }
    selected <- all[names(all) %in% individual]
  }
  if (!is.null(chr) && (!is.character(chr) || anyNA(chr) || any(!nzchar(chr)))) {
    stop("`chr` must contain chromosome names.", call. = FALSE)
  }
  for (bound in c("start", "end")) {
    value <- get(bound)
    if (!is.null(value) && (!is.numeric(value) || length(value) != 1L || !is.finite(value))) {
      stop("`", bound, "` must be one finite genomic coordinate.", call. = FALSE)
    }
  }
  if (!is.null(start) && !is.null(end) && start > end) {
    stop("`start` must be less than or equal to `end`.", call. = FALSE)
  }

  name <- .prepared_alignseq_name(selected, name, all)
  entries <- .prepared_alignseq_entries(selected, name)
  tables <- lapply(entries, `[[`, "data")
  if (!length(tables)) {
    # A named alignment can be present on individuals outside the selection.
    # Use a cached table to preserve the column types for the empty result.
    remaining <- all[setdiff(names(all), names(selected))]
    template <- Filter(Negate(is.null), lapply(remaining, function(ind) {
      ind@plot_cache[["alignseq"]][[name]]$data
    }))
    if (!length(template)) {
      stop("Prepared alignment `", name, "` has a cleared cache; run `prepare_alignseq()` again.",
        call. = FALSE)
    }
    tables <- list(template[[1L]][0, , drop = FALSE])
  }
  out <- dplyr::bind_rows(tables)
  keep <- rep(TRUE, nrow(out))
  if (!is.null(chr)) keep <- keep & out$chr %in% chr
  if (!is.null(start)) keep <- keep & out$x >= start
  if (!is.null(end)) keep <- keep & out$x <= end
  out <- out[keep, , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "ggexon_panel_role") <- "annotation"
  out
}

.prepared_alignseq_individuals <- function(x) {
  if (methods::is(x, "SynIndividual")) return(stats::setNames(list(x), syn_id(x)))
  if (methods::is(x, "SynSpecies")) return(individuals(x))
  stop("Supply a SynIndividual or SynSpecies.", call. = FALSE)
}

.prepared_alignseq_name <- function(selected, name = NULL, all = selected) {
  candidates <- if (is.null(name)) selected else all
  available <- unique(unlist(lapply(candidates, function(ind) {
    union(names(ind@plot_cache[["alignseq"]]), names(ind@metadata[[".ggexon_alignseq"]]))
  }), use.names = FALSE))
  if (!is.null(name)) {
    .genomic_name(name, "name")
    if (!name %in% available) {
      stop("Unknown prepared alignment `", name, "`; run `prepare_alignseq()`.",
        call. = FALSE)
    }
    return(name)
  }
  if (!length(available)) {
    stop("No prepared alignment is available on the selected individuals; run `prepare_alignseq()`.",
      call. = FALSE)
  }
  if (length(available) > 1L) {
    stop("Multiple prepared alignments are applicable; supply a name with `name`",
      " in `query_alignseq()` or `alignment` in `geom_alignseq()`.", call. = FALSE)
  }
  available[[1L]]
}

.prepared_alignseq_entries <- function(selected, name) {
  entries <- Filter(Negate(is.null), lapply(selected, .prepared_alignseq_entry, name = name))
  if (length(entries) > 1L) {
    recipe <- entries[[1L]][["recipe"]]
    compatible <- vapply(entries, function(entry) identical(entry[["recipe"]], recipe), logical(1))
    if (!all(compatible)) {
      stop("Prepared alignment `", name, "` contains incompatible recipes; ",
        "run `prepare_alignseq()` on the comparison or use distinct names.", call. = FALSE)
    }
  }
  entries
}

.prepared_alignseq_entry <- function(ind, name) {
  entry <- ind@plot_cache[["alignseq"]][[name]]
  if (is.null(entry) && isTRUE(ind@metadata[[".ggexon_alignseq"]][[name]])) {
    stop("Prepared alignment `", name, "` has a cleared cache for `", syn_id(ind),
      "`; run `prepare_alignseq()` again.", call. = FALSE)
  }
  entry
}

.prepared_alignseq_boundaries <- function(rows, aligned, anchors, boundaries) {
  fields <- c("sequence", "position", "label")
  if (!is.data.frame(boundaries) || !all(fields %in% names(boundaries))) {
    stop("`boundaries` must be a data frame with sequence, position, and label columns.",
      call. = FALSE)
  }
  sequence <- as.character(boundaries$sequence)
  if (any(!sequence %in% names(aligned))) {
    stop("Each boundary sequence must be present in `aligned`.", call. = FALSE)
  }
  position <- boundaries$position
  if (!is.numeric(position) || any(!is.finite(position)) ||
      any(position < 1 | position != floor(position))) {
    stop("`boundaries$position` must contain positive integer genomic coordinates.",
      call. = FALSE)
  }
  label <- as.character(boundaries$label)
  if (anyNA(label) || anyDuplicated(boundaries[c("sequence", "position")])) {
    stop("Supply one boundary label per sequence and position.", call. = FALSE)
  }

  for (id in unique(sequence)) {
    a <- match(id, anchors$sequence)
    first <- anchors$start[a]
    step <- if (anchors$strand[a] == "+") 1 else -1
    last <- first + step * (nchar(gsub("-", "", aligned[[id]], fixed = TRUE)) - 1L)
    index <- which(sequence == id)
    if (any(position[index] < min(first, last) | position[index] > max(first, last))) {
      stop("Each boundary must identify a native genomic base in its aligned sequence.",
        call. = FALSE)
    }
    selected <- which(rows$sequence == id)
    matched <- match(rows$position[selected], position[index])
    marked <- !is.na(matched)
    rows$boundary_after[selected[marked]] <- TRUE
    rows$boundary_label[selected[marked]] <- label[index[matched[marked]]]
  }
  rows
}
