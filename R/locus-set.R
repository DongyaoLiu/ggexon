#' SynLocusSet class
#'
#' `SynLocusSet` stores a species-level table of comparable genomic windows.
#' It is designed for multi-locus or paralog grids where one `SynSpecies`
#' individual may appear in several panels, one per focal locus or inferred
#' syntenic window.
#'
#' @slot locus_table A data frame with one row per comparable locus window.
#'   Required columns are `locus_id`, `individual`, `seqname`, `start`, `end`,
#'   `row_group`, `col_group`, and `track`.
#'
#' @include SynAnnotation-class.R SynIndividual-class.R
#' @exportClass SynLocusSet
setClass(
  "SynLocusSet",
  contains = "SynSpeAnnotation",
  slots = c(locus_table = "data.frame"),
  prototype = list(
    annotation_scope = "species",
    lazy = FALSE,
    loaded = TRUE,
    source_file = "<locus_set>",
    locus_table = data.frame(
      locus_id = character(),
      individual = character(),
      seqname = character(),
      start = numeric(),
      end = numeric(),
      row_group = character(),
      col_group = character(),
      track = character(),
      stringsAsFactors = FALSE
    )
  ),
  validity = function(object) {
    problems <- character()
    table <- object@locus_table
    required <- c("locus_id", "individual", "seqname", "start", "end", "row_group", "col_group", "track")
    missing <- setdiff(required, names(table))
    if (length(missing) > 0L) {
      problems <- c(
        problems,
        paste0("`locus_table` is missing required columns: ", paste(missing, collapse = ", "), ".")
      )
    }
    if (length(missing) == 0L && nrow(table) > 0L) {
      character_cols <- c("locus_id", "individual", "seqname", "row_group", "col_group", "track")
      for (col in character_cols) {
        values <- as.character(table[[col]])
        if (any(is.na(values) | !nzchar(values))) {
          problems <- c(problems, paste0("`locus_table$", col, "` must not contain missing or empty values."))
        }
      }
      starts <- suppressWarnings(as.numeric(table$start))
      ends <- suppressWarnings(as.numeric(table$end))
      if (any(!is.finite(starts)) || any(!is.finite(ends))) {
        problems <- c(problems, "`locus_table$start` and `locus_table$end` must be finite numeric values.")
      } else if (any(starts > ends)) {
        problems <- c(problems, "`locus_table$start` must be less than or equal to `locus_table$end`.")
      }
      if (any(duplicated(as.character(table$track)))) {
        problems <- c(problems, "`locus_table$track` must be unique.")
      }
    }
    if (length(problems) == 0L) TRUE else problems
  }
)

.normalize_locus_table <- function(loci) {
  if (!is.data.frame(loci)) {
    stop("`loci` must be a data frame.", call. = FALSE)
  }
  loci <- as.data.frame(loci, stringsAsFactors = FALSE)

  aliases <- c(seq_id = "seqname", chr = "seqname", xlim_chr = "seqname", xlim_min = "start", xlim_max = "end")
  for (from in names(aliases)) {
    to <- aliases[[from]]
    if (!to %in% names(loci) && from %in% names(loci)) {
      loci[[to]] <- loci[[from]]
    }
  }
  if (!"individual" %in% names(loci) && "species_id" %in% names(loci)) {
    loci$individual <- loci$species_id
  }
  if (!"locus_id" %in% names(loci)) {
    if ("focal_label" %in% names(loci)) {
      loci$locus_id <- paste(loci$individual %||% "", loci$focal_label, sep = "_")
    } else {
      stop("`loci` must contain `locus_id`.", call. = FALSE)
    }
  }
  if (!"row_group" %in% names(loci)) {
    loci$row_group <- loci$individual
  }
  if (!"col_group" %in% names(loci)) {
    loci$col_group <- if ("focal_label" %in% names(loci)) loci$focal_label else loci$locus_id
  }
  if (!"track" %in% names(loci)) {
    loci$track <- paste(loci$individual, loci$col_group, sep = "__")
  }
  if (!"strand" %in% names(loci)) {
    loci$strand <- "."
  }
  if (!"focal_gene" %in% names(loci)) {
    loci$focal_gene <- loci$col_group
  }
  if (!"focal_start" %in% names(loci)) {
    loci$focal_start <- NA_real_
  }
  if (!"focal_end" %in% names(loci)) {
    loci$focal_end <- NA_real_
  }
  if (!"anchor_genes" %in% names(loci)) {
    loci$anchor_genes <- NA_character_
  }
  if (!"window_source" %in% names(loci)) {
    loci$window_source <- "manual"
  }
  if (!"panel_label" %in% names(loci)) {
    loci$panel_label <- paste(loci$row_group, loci$col_group, sep = " ")
  }

  character_cols <- intersect(
    c(
      "locus_id", "individual", "seqname", "row_group", "col_group", "track",
      "strand", "focal_gene", "anchor_genes", "window_source", "panel_label"
    ),
    names(loci)
  )
  for (col in character_cols) {
    loci[[col]] <- as.character(loci[[col]])
  }
  numeric_cols <- intersect(c("start", "end", "focal_start", "focal_end"), names(loci))
  for (col in numeric_cols) {
    loci[[col]] <- suppressWarnings(as.numeric(loci[[col]]))
  }

  required <- c("locus_id", "individual", "seqname", "start", "end", "row_group", "col_group", "track")
  missing <- setdiff(required, names(loci))
  if (length(missing) > 0L) {
    stop("`loci` must contain columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  swap <- !is.na(loci$start) & !is.na(loci$end) & loci$start > loci$end
  if (any(swap)) {
    old_start <- loci$start[swap]
    loci$start[swap] <- loci$end[swap]
    loci$end[swap] <- old_start
  }
  rownames(loci) <- NULL
  loci
}

#' Construct a locus-window set
#'
#' @param name Short unique locus-set name.
#' @param loci Data frame with one row per comparable window.
#' @param source_file Optional source label.
#' @param metadata Optional metadata list.
#'
#' @return A `SynLocusSet` object.
#' @export
SynLocusSet <- function(name,
                        loci,
                        source_file = "<locus_set>",
                        metadata = list()) {
  loci <- .normalize_locus_table(loci)
  new(
    "SynLocusSet",
    name = name,
    source_file = source_file,
    locus_table = loci,
    metadata = metadata
  )
}

#' Retrieve the locus table from a SynLocusSet
#'
#' @param x A `SynLocusSet` object.
#'
#' @return A data frame with one row per locus window.
#' @export
setGeneric("locus_table", function(x) standardGeneric("locus_table"))
#' @rdname locus_table
setMethod("locus_table", "SynLocusSet", function(x) x@locus_table)

#' @export
#' @rdname ggexon-show
setMethod("show", "SynLocusSet", function(object) {
  cat("An object of class \"SynLocusSet\"\n")
  cat("  name:", annotation_name(object), "\n")
  cat("  loci:", nrow(object@locus_table), "\n")
  cat("  rows:", length(unique(object@locus_table$row_group)), "\n")
  cat("  columns:", length(unique(object@locus_table$col_group)), "\n")
})

.normalize_locus_anchor_map <- function(anchors, loci) {
  loci <- unique(as.character(loci))
  if (is.null(anchors)) {
    return(stats::setNames(vector("list", length(loci)), loci))
  }
  if (is.character(anchors)) {
    return(stats::setNames(rep(list(unique(anchors)), length(loci)), loci))
  }
  if (!is.list(anchors)) {
    stop("`anchors` must be NULL, a character vector, or a named list.", call. = FALSE)
  }
  if (is.null(names(anchors)) || anyNA(names(anchors)) || any(!nzchar(names(anchors)))) {
    stop("List-style `anchors` must be named by locus.", call. = FALSE)
  }
  out <- stats::setNames(vector("list", length(loci)), loci)
  for (locus in loci) {
    out[[locus]] <- unique(as.character(anchors[[locus]] %||% character()))
  }
  out
}

.syn_locus_gene_table <- function(individual, annotation = NULL, feature_type = "gene") {
  if (!methods::is(individual, "SynIndividual")) {
    stop("Expected a SynIndividual object.", call. = FALSE)
  }
  if (!is.null(annotation)) {
    old_active <- active_feature_annotation(individual)
    individual <- set_active_feature_annotation(individual, annotation)
    on.exit({
      try(set_active_feature_annotation(individual, old_active), silent = TRUE)
    }, add = TRUE)
  }
  gr <- query_features(individual, feature_type = feature_type, all = TRUE)
  if (length(gr) == 0L) {
    return(data.frame())
  }
  meta <- as.data.frame(S4Vectors::mcols(gr), stringsAsFactors = FALSE)
  coalesce_meta <- function(candidates) {
    cols <- intersect(candidates, names(meta))
    if (length(cols) == 0L) {
      return(rep(NA_character_, length(gr)))
    }
    out <- rep(NA_character_, length(gr))
    for (col in cols) {
      value <- as.character(meta[[col]])
      fill <- is.na(out) | !nzchar(out)
      out[fill] <- value[fill]
    }
    out
  }
  data.frame(
    seqname = as.character(GenomeInfoDb::seqnames(gr)),
    start = IRanges::start(gr),
    end = IRanges::end(gr),
    strand = as.character(BiocGenerics::strand(gr)),
    gene_id = coalesce_meta(c("gene_id", "ID", "Name")),
    gene_name = coalesce_meta(c("gene_name", "gene", "Name", "gene_id", "ID")),
    stringsAsFactors = FALSE
  )
}

.syn_locus_match_gene_rows <- function(genes, query, prefix = TRUE) {
  if (!is.data.frame(genes) || nrow(genes) == 0L || length(query) == 0L) {
    return(genes[FALSE, , drop = FALSE])
  }
  query <- unique(to_upper_ascii(as.character(query)))
  query <- query[!is.na(query) & nzchar(query)]
  values <- unique(c("gene_id", "gene_name"))
  values <- values[values %in% names(genes)]
  match_row <- rep(FALSE, nrow(genes))
  for (col in values) {
    x <- to_upper_ascii(as.character(genes[[col]]))
    x[is.na(x)] <- ""
    exact <- x %in% query
    if (isTRUE(prefix)) {
      prefix_match <- Reduce(`|`, lapply(query, function(q) startsWith(x, q)), init = rep(FALSE, length(x)))
      exact <- exact | prefix_match
    }
    match_row <- match_row | exact
  }
  genes[match_row, , drop = FALSE]
}

.syn_locus_pick_feature <- function(genes, query) {
  hits <- .syn_locus_match_gene_rows(genes, query, prefix = FALSE)
  if (nrow(hits) == 0L) {
    return(NULL)
  }
  hits$width <- hits$end - hits$start + 1
  hits <- hits[order(-hits$width, hits$seqname, hits$start), , drop = FALSE]
  hits[1L, , drop = FALSE]
}

.syn_locus_pick_anchor_cluster <- function(anchor_hits, flank) {
  if (!is.data.frame(anchor_hits) || nrow(anchor_hits) == 0L) {
    return(NULL)
  }
  anchor_hits$mid <- (anchor_hits$start + anchor_hits$end) / 2
  anchor_hits$anchor_label <- to_upper_ascii(anchor_hits$anchor_label)
  candidates <- list()
  k <- 1L
  for (seqname in unique(anchor_hits$seqname)) {
    rows <- anchor_hits[anchor_hits$seqname == seqname, , drop = FALSE]
    rows <- rows[order(rows$mid, rows$start), , drop = FALSE]
    for (i in seq_len(nrow(rows))) {
      for (j in i:nrow(rows)) {
        span <- max(rows$end[i:j]) - min(rows$start[i:j]) + 1
        if (span > max(1, flank) * 2 && j > i) {
          break
        }
        candidates[[k]] <- rows[i:j, , drop = FALSE]
        k <- k + 1L
      }
    }
  }
  if (!length(candidates)) {
    return(anchor_hits[1L, , drop = FALSE])
  }
  scores <- data.frame(
    index = seq_along(candidates),
    anchors = vapply(candidates, function(x) length(unique(x$anchor_label)), integer(1)),
    rows = vapply(candidates, nrow, integer(1)),
    span = vapply(candidates, function(x) max(x$end) - min(x$start) + 1, numeric(1))
  )
  scores <- scores[order(-scores$anchors, -scores$rows, scores$span), , drop = FALSE]
  candidates[[scores$index[[1L]]]]
}

#' Infer comparable locus windows from Syn annotations
#'
#' Searches each individual in a `SynSpecies` for direct focal genes and, when
#' missing, falls back to anchor genes to infer a local syntenic window. This
#' helper returns a `SynLocusSet`; attach it with [add_locus_set()] and call
#' [use_locus_grid()] to store the corresponding panel layout.
#'
#' @param x A `SynSpecies` object.
#' @param loci Character vector of focal loci / grid columns.
#' @param anchors Optional character vector used for every locus, or a named
#'   list of anchor genes keyed by `loci`.
#' @param name Locus-set name.
#' @param individual Optional individuals to include. Defaults to all.
#' @param reference Optional reference individual recorded in metadata.
#' @param flank Number of bases to add around direct focal genes and anchor
#'   clusters.
#' @param annotation Optional feature annotation layer to query.
#' @param feature_type Feature type used for gene rows. Defaults to `"gene"`.
#' @param prefix_anchors Treat anchor names as gene-prefixes. Useful for gene
#'   families such as `NBPF`.
#' @param missing What to do when neither a focal gene nor anchors are found.
#'
#' @return A `SynLocusSet` object.
#' @export
infer_locus_windows <- function(x,
                                loci,
                                anchors = NULL,
                                name = "locus_windows",
                                individual = NULL,
                                reference = NULL,
                                flank = 450000,
                                annotation = NULL,
                                feature_type = "gene",
                                prefix_anchors = TRUE,
                                missing = c("drop", "error")) {
  if (!methods::is(x, "SynSpecies")) {
    stop("`infer_locus_windows()` expects a SynSpecies object.", call. = FALSE)
  }
  missing <- match.arg(missing)
  if (!is.character(loci) || length(loci) == 0L || anyNA(loci) || any(!nzchar(loci))) {
    stop("`loci` must be a non-empty character vector.", call. = FALSE)
  }
  if (!is.numeric(flank) || length(flank) != 1L || is.na(flank) || flank < 0) {
    stop("`flank` must be one non-negative numeric value.", call. = FALSE)
  }
  available <- names(individuals(x))
  if (is.null(individual)) {
    individual <- available
  } else {
    individual <- unique(as.character(individual))
    unknown <- setdiff(individual, available)
    if (length(unknown) > 0L) {
      stop("`individual` values are not present in `x`: ", paste(unknown, collapse = ", "), call. = FALSE)
    }
  }
  anchor_map <- .normalize_locus_anchor_map(anchors, loci)
  gene_tables <- lapply(individual, function(id) {
    .syn_locus_gene_table(individuals(x)[[id]], annotation = annotation, feature_type = feature_type)
  })
  names(gene_tables) <- individual

  rows <- list()
  row_i <- 1L
  missing_labels <- character()
  for (id in individual) {
    genes <- gene_tables[[id]]
    for (locus in loci) {
      focal <- .syn_locus_pick_feature(genes, locus)
      if (!is.null(focal)) {
        start <- max(1, focal$start[[1L]] - flank)
        end <- focal$end[[1L]] + flank
        rows[[row_i]] <- data.frame(
          locus_id = paste(id, locus, sep = "__"),
          individual = id,
          seqname = focal$seqname[[1L]],
          start = start,
          end = end,
          row_group = id,
          col_group = locus,
          track = paste(id, locus, sep = "__"),
          strand = focal$strand[[1L]],
          focal_gene = locus,
          focal_start = focal$start[[1L]],
          focal_end = focal$end[[1L]],
          anchor_genes = paste(anchor_map[[locus]], collapse = ","),
          window_source = "direct_focal",
          panel_label = paste(id, locus),
          stringsAsFactors = FALSE
        )
        row_i <- row_i + 1L
        next
      }

      anchors_for_locus <- anchor_map[[locus]]
      anchor_hits <- .syn_locus_match_gene_rows(genes, anchors_for_locus, prefix = prefix_anchors)
      if (nrow(anchor_hits) > 0L) {
        anchor_hits$anchor_label <- vapply(seq_len(nrow(anchor_hits)), function(i) {
          hit_values <- to_upper_ascii(c(anchor_hits$gene_id[[i]], anchor_hits$gene_name[[i]]))
          hit_values[is.na(hit_values)] <- ""
          label <- anchors_for_locus[
            vapply(
              to_upper_ascii(anchors_for_locus),
              function(anchor) {
                any(startsWith(hit_values, anchor)) ||
                  any(hit_values == anchor)
              },
              logical(1)
            )
          ]
          if (length(label) == 0L) anchor_hits$gene_name[[i]] else label[[1L]]
        }, character(1))
        cluster <- .syn_locus_pick_anchor_cluster(anchor_hits, flank = flank)
        rows[[row_i]] <- data.frame(
          locus_id = paste(id, locus, sep = "__"),
          individual = id,
          seqname = cluster$seqname[[1L]],
          start = max(1, min(cluster$start) - flank),
          end = max(cluster$end) + flank,
          row_group = id,
          col_group = locus,
          track = paste(id, locus, sep = "__"),
          strand = ".",
          focal_gene = locus,
          focal_start = NA_real_,
          focal_end = NA_real_,
          anchor_genes = paste(unique(cluster$anchor_label), collapse = ","),
          window_source = "anchor_inferred",
          panel_label = paste(id, locus),
          stringsAsFactors = FALSE
        )
        row_i <- row_i + 1L
        next
      }

      missing_labels <- c(missing_labels, paste(id, locus, sep = ":"))
    }
  }

  if (length(missing_labels) > 0L && identical(missing, "error")) {
    stop("No focal gene or anchor window found for: ", paste(missing_labels, collapse = ", "), call. = FALSE)
  }
  if (!length(rows)) {
    stop("No locus windows were inferred.", call. = FALSE)
  }

  SynLocusSet(
    name = name,
    loci = do.call(rbind, rows),
    metadata = list(
      reference = reference,
      anchors = anchor_map,
      flank = flank,
      missing = missing_labels
    )
  )
}

.resolve_locus_set <- function(x, locus_set = NULL) {
  if (methods::is(locus_set, "SynLocusSet")) {
    return(locus_set)
  }
  if (!methods::is(x, "SynSpecies")) {
    stop("Expected a SynSpecies object.", call. = FALSE)
  }
  sets <- locus_sets(x)
  if (length(sets) == 0L) {
    stop("No SynLocusSet objects are attached to this SynSpecies.", call. = FALSE)
  }
  if (is.null(locus_set)) {
    if (length(sets) != 1L) {
      stop("Provide `locus_set` when multiple SynLocusSet objects are attached.", call. = FALSE)
    }
    return(sets[[1L]])
  }
  if (!is.character(locus_set) || length(locus_set) != 1L || is.na(locus_set) || !nzchar(locus_set)) {
    stop("`locus_set` must be a SynLocusSet object or one attached locus-set name.", call. = FALSE)
  }
  out <- sets[[locus_set]]
  if (is.null(out)) {
    stop("No SynLocusSet named `", locus_set, "` is attached.", call. = FALSE)
  }
  out
}

#' Build a grid SynLayout from a locus set
#'
#' @param x A `SynSpecies` or `SynLocusSet` object.
#' @param locus_set Optional attached locus-set name or `SynLocusSet` object
#'   when `x` is a `SynSpecies`.
#' @param row_order Optional row order for `row_group`.
#' @param col_order Optional column order for `col_group`.
#' @param free Free-scale settings for the returned `SynLayout`.
#'
#' @return A `SynLayout` object.
#' @export
locus_grid_layout <- function(x,
                              locus_set = NULL,
                              row_order = NULL,
                              col_order = NULL,
                              free = list(x = TRUE, y = FALSE)) {
  set <- if (methods::is(x, "SynLocusSet")) x else .resolve_locus_set(x, locus_set)
  loci <- locus_table(set)
  row_order <- row_order %||% unique(loci$row_group)
  col_order <- col_order %||% unique(loci$col_group)
  loci$row_index <- match(loci$row_group, row_order)
  loci$col_index <- match(loci$col_group, col_order)
  loci <- loci[!is.na(loci$row_index) & !is.na(loci$col_index), , drop = FALSE]
  loci <- loci[order(loci$row_index, loci$col_index), , drop = FALSE]
  if (nrow(loci) == 0L) {
    stop("No locus windows remain after applying `row_order` and `col_order`.", call. = FALSE)
  }

  panels <- data.frame(
    PANEL = seq_len(nrow(loci)),
    ROW = loci$row_index,
    COL = loci$col_index,
    track = loci$track,
    panel_type = "annotation",
    species = loci$individual,
    individual = loci$individual,
    locus_id = loci$locus_id,
    row_group = loci$row_group,
    col_group = loci$col_group,
    focal_gene = loci$focal_gene,
    window_source = loci$window_source,
    xlim_chr = loci$seqname,
    xlim_min = loci$start,
    xlim_max = loci$end,
    stringsAsFactors = FALSE
  )
  SynLayout(
    panels = panels,
    layout_type = "locus_grid",
    free = free,
    metadata = list(
      locus_set = annotation_name(set),
      row_order = row_order,
      col_order = col_order
    )
  )
}

#' Store a locus-grid layout on a SynSpecies
#'
#' @param x A `SynSpecies` object.
#' @param locus_set Optional attached locus-set name or `SynLocusSet` object.
#' @param row_order Optional row order.
#' @param col_order Optional column order.
#' @param free Free-scale settings passed to [locus_grid_layout()].
#'
#' @return The updated `SynSpecies`.
#' @export
use_locus_grid <- function(x,
                           locus_set = NULL,
                           row_order = NULL,
                           col_order = NULL,
                           free = list(x = TRUE, y = FALSE)) {
  if (!methods::is(x, "SynSpecies")) {
    stop("`use_locus_grid()` expects a SynSpecies object.", call. = FALSE)
  }
  species_layout(x) <- locus_grid_layout(
    x,
    locus_set = locus_set,
    row_order = row_order,
    col_order = col_order,
    free = free
  )
  x
}
