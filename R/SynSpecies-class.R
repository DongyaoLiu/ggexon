#' SynSpecies and alignment classes
#'
#' `SynSpecies` stores a collection of `SynIndividual` objects together with
#' pairwise and multiple-species alignment layers that describe relationships
#' across species.
#'
#' @keywords internal
NULL

#' @exportClass SynPairAlignment
setClass(
  "SynPairAlignment",
  slots = c(
    name = "character",
    query_individual = "character",
    target_individual = "character",
    file = "character",
    format = "character",
    data = "ANY",
    metadata = "list"
  ),
  prototype = list(
    name = NA_character_,
    query_individual = NA_character_,
    target_individual = NA_character_,
    file = NA_character_,
    format = "paf",
    data = NULL,
    metadata = list()
  ),
  validity = function(object) {
    problems <- character()
    if (length(object@name) != 1L || is.na(object@name) || !nzchar(object@name)) {
      problems <- c(problems, "`name` must be a single non-empty character value.")
    }
    if (length(object@query_individual) != 1L || is.na(object@query_individual) ||
        !nzchar(object@query_individual)) {
      problems <- c(problems, "`query_individual` must be a single non-empty character value.")
    }
    if (length(object@target_individual) != 1L || is.na(object@target_individual) ||
        !nzchar(object@target_individual)) {
      problems <- c(problems, "`target_individual` must be a single non-empty character value.")
    }
    if (length(object@file) != 1L || is.na(object@file) || !nzchar(object@file)) {
      problems <- c(problems, "`file` must be a single non-empty character value.")
    }
    if (length(object@format) != 1L || !(object@format %in% c("paf"))) {
      problems <- c(problems, "`format` must currently be 'paf'.")
    }
    if (length(problems) == 0L) TRUE else problems
  }
)

#' @exportClass SynMultiAlignment
setClass(
  "SynMultiAlignment",
  slots = c(
    name = "character",
    individuals = "character",
    file = "character",
    format = "character",
    data = "ANY",
    metadata = "list"
  ),
  prototype = list(
    name = NA_character_,
    individuals = character(),
    file = NA_character_,
    format = "maf",
    data = NULL,
    metadata = list()
  ),
  validity = function(object) {
    problems <- character()
    if (length(object@name) != 1L || is.na(object@name) || !nzchar(object@name)) {
      problems <- c(problems, "`name` must be a single non-empty character value.")
    }
    if (length(object@individuals) < 2L || any(is.na(object@individuals)) ||
        any(!nzchar(object@individuals))) {
      problems <- c(problems, "`individuals` must contain at least two non-empty names.")
    }
    if (length(object@file) != 1L || is.na(object@file) || !nzchar(object@file)) {
      problems <- c(problems, "`file` must be a single non-empty character value.")
    }
    if (length(object@format) != 1L || !(object@format %in% c("maf"))) {
      problems <- c(problems, "`format` must currently be 'maf'.")
    }
    if (length(problems) == 0L) TRUE else problems
  }
)

#' @exportClass SynSpecies
setClass(
  "SynSpecies",
  slots = c(
    name = "character",
    individuals = "list",
    pairwise_alignments = "list",
    multiple_alignments = "list",
    metadata = "list",
    layout = "ANY"
  ),
  prototype = list(
    name = NA_character_,
    individuals = list(),
    pairwise_alignments = list(),
    multiple_alignments = list(),
    metadata = list(),
    layout = NULL
  ),
  validity = function(object) {
    problems <- character()
    if (length(object@name) != 1L || is.na(object@name) || !nzchar(object@name)) {
      problems <- c(problems, "`name` must be a single non-empty character value.")
    }
    if (length(object@individuals) > 0L) {
      bad_individuals <- !vapply(object@individuals, methods::is, logical(1), class2 = "SynIndividual")
      if (any(bad_individuals)) {
        problems <- c(problems, "`individuals` must be a list of SynIndividual objects.")
      }
    }
    if (length(object@pairwise_alignments) > 0L) {
      bad_pairs <- !vapply(object@pairwise_alignments, methods::is, logical(1), class2 = "SynPairAlignment")
      if (any(bad_pairs)) {
        problems <- c(problems, "`pairwise_alignments` must be a list of SynPairAlignment objects.")
      }
    }
    if (length(object@multiple_alignments) > 0L) {
      bad_multi <- !vapply(object@multiple_alignments, methods::is, logical(1), class2 = "SynMultiAlignment")
      if (any(bad_multi)) {
        problems <- c(problems, "`multiple_alignments` must be a list of SynMultiAlignment objects.")
      }
    }
    if (!is.null(object@layout)) {
      if (!is.data.frame(object@layout)) {
        problems <- c(problems, "`layout` must be a data.frame or NULL.")
      } else {
        required_layout_cols <- c("PANEL", "ROW", "COL", "track")
        missing_layout_cols <- setdiff(required_layout_cols, colnames(object@layout))
        if (length(missing_layout_cols) > 0L) {
          problems <- c(
            problems,
            paste0(
              "`layout` is missing required columns: ",
              paste(missing_layout_cols, collapse = ", "),
              "."
            )
          )
        }
      }
    }
    if (length(problems) == 0L) TRUE else problems
  }
)

#' Constructor for SynPairAlignment
#'
#' @param name Alignment label.
#' @param query_individual Query-side individual name.
#' @param target_individual Target-side individual name.
#' @param file Path to the PAF file.
#' @param metadata Optional metadata list.
#'
#' @return A `SynPairAlignment` object.
#' @export
SynPairAlignment <- function(name,
                             query_individual,
                             target_individual,
                             file,
                             metadata = list()) {
  new(
    "SynPairAlignment",
    name = name,
    query_individual = query_individual,
    target_individual = target_individual,
    file = file,
    metadata = metadata
  )
}

#' Constructor for SynMultiAlignment
#'
#' @param name Alignment label.
#' @param individuals Character vector of included individuals.
#' @param file Path to the MAF file.
#' @param metadata Optional metadata list.
#'
#' @return A `SynMultiAlignment` object.
#' @export
SynMultiAlignment <- function(name,
                              individuals,
                              file,
                              metadata = list()) {
  new(
    "SynMultiAlignment",
    name = name,
    individuals = individuals,
    file = file,
    metadata = metadata
  )
}

#' Constructor for SynSpecies
#'
#' @param name Species collection label.
#' @param metadata Optional metadata list.
#'
#' @return A `SynSpecies` object.
#' @export
SynSpecies <- function(name, metadata = list()) {
  new("SynSpecies", name = name, metadata = metadata)
}

#' @export
setMethod("show", "SynSpecies", function(object) {
  cat("An object of class \"SynSpecies\"\n")
  cat("  name:", object@name, "\n")
  cat("  individuals:", length(object@individuals), "\n")
  cat("  pairwise_alignments:", length(object@pairwise_alignments), "\n")
  cat("  multiple_alignments:", length(object@multiple_alignments), "\n")
})

setGeneric("species_name", function(x) standardGeneric("species_name"))
setMethod("species_name", "SynSpecies", function(x) x@name)

setGeneric("individuals", function(x) standardGeneric("individuals"))
setMethod("individuals", "SynSpecies", function(x) x@individuals)

setGeneric("pairwise_alignments", function(x) standardGeneric("pairwise_alignments"))
setMethod("pairwise_alignments", "SynSpecies", function(x) x@pairwise_alignments)

setGeneric("multiple_alignments", function(x) standardGeneric("multiple_alignments"))
setMethod("multiple_alignments", "SynSpecies", function(x) x@multiple_alignments)

setGeneric("species_layout", function(x) standardGeneric("species_layout"))
setMethod("species_layout", "SynSpecies", function(x) x@layout)

setGeneric("alignment_name", function(x) standardGeneric("alignment_name"))
setMethod("alignment_name", "SynPairAlignment", function(x) x@name)
setMethod("alignment_name", "SynMultiAlignment", function(x) x@name)

setGeneric("alignment_file", function(x) standardGeneric("alignment_file"))
setMethod("alignment_file", "SynPairAlignment", function(x) x@file)
setMethod("alignment_file", "SynMultiAlignment", function(x) x@file)

setGeneric("query_individual", function(x) standardGeneric("query_individual"))
setMethod("query_individual", "SynPairAlignment", function(x) x@query_individual)

setGeneric("target_individual", function(x) standardGeneric("target_individual"))
setMethod("target_individual", "SynPairAlignment", function(x) x@target_individual)

setGeneric("alignment_individuals", function(x) standardGeneric("alignment_individuals"))
setMethod("alignment_individuals", "SynPairAlignment", function(x) {
  c(query_individual(x), target_individual(x))
})
setMethod("alignment_individuals", "SynMultiAlignment", function(x) x@individuals)

setGeneric("pairwise_alignment_data", function(x, ...) standardGeneric("pairwise_alignment_data"))
setMethod("pairwise_alignment_data", "SynPairAlignment", function(x, alignment = NULL, ...) {
  .pairwise_alignment_data_impl(x = x, species_obj = NULL, ...)
})
setMethod("pairwise_alignment_data", "SynSpecies", function(x, alignment = NULL, ...) {
  pair <- .resolve_pairwise_alignment_arg(x = x, alignment = alignment)
  .pairwise_alignment_data_impl(x = pair, species_obj = x, ...)
})

#' Add a SynIndividual to a SynSpecies object
#'
#' @param x A `SynSpecies` object.
#' @param individual A `SynIndividual` object.
#'
#' @return An updated `SynSpecies` object.
#' @export
add_individual <- function(x, individual) {
  if (!methods::is(x, "SynSpecies")) {
    stop("`add_individual()` expects a SynSpecies object.", call. = FALSE)
  }
  if (!methods::is(individual, "SynIndividual")) {
    stop("`individual` must be a SynIndividual object.", call. = FALSE)
  }
  entries <- x@individuals
  entries[[syn_id(individual)]] <- individual
  x@individuals <- entries
  validObject(x)
  x
}

#' Add a pairwise alignment to a SynSpecies object
#'
#' @param x A `SynSpecies` object.
#' @param alignment A `SynPairAlignment` object.
#'
#' @return An updated `SynSpecies` object.
#' @export
add_pairwise_alignment <- function(x, alignment) {
  if (!methods::is(x, "SynSpecies")) {
    stop("`add_pairwise_alignment()` expects a SynSpecies object.", call. = FALSE)
  }
  if (!methods::is(alignment, "SynPairAlignment")) {
    stop("`alignment` must be a SynPairAlignment object.", call. = FALSE)
  }
  entries <- x@pairwise_alignments
  entries[[alignment_name(alignment)]] <- alignment
  x@pairwise_alignments <- entries
  validObject(x)
  x
}

#' Add a multiple alignment to a SynSpecies object
#'
#' @param x A `SynSpecies` object.
#' @param alignment A `SynMultiAlignment` object.
#'
#' @return An updated `SynSpecies` object.
#' @export
add_multiple_alignment <- function(x, alignment) {
  if (!methods::is(x, "SynSpecies")) {
    stop("`add_multiple_alignment()` expects a SynSpecies object.", call. = FALSE)
  }
  if (!methods::is(alignment, "SynMultiAlignment")) {
    stop("`alignment` must be a SynMultiAlignment object.", call. = FALSE)
  }
  entries <- x@multiple_alignments
  entries[[alignment_name(alignment)]] <- alignment
  x@multiple_alignments <- entries
  validObject(x)
  x
}

#' Store a ggexon panel layout on a `SynSpecies` object
#'
#' @param x A `SynSpecies` object.
#' @param value A layout `data.frame` or `NULL`.
#'
#' @return The updated `SynSpecies` object.
#' @export
setGeneric("species_layout<-", function(x, value) standardGeneric("species_layout<-"))
setReplaceMethod("species_layout", "SynSpecies", function(x, value) {
  if (!is.null(value) && !is.data.frame(value)) {
    stop("`species_layout<-` expects a data.frame or NULL.", call. = FALSE)
  }
  x@layout <- value
  validObject(x)
  x
})

#' Compute and store the ggexon chain layout on a `SynSpecies`
#'
#' @param x A `SynSpecies` object.
#' @param vars Facet vars. Defaults to `ggplot2::vars(track)`.
#' @param free List with logical `x` and `y` entries controlling scale grouping.
#'
#' @return The updated `SynSpecies` object.
#' @export
store_chain_layout <- function(x,
                               vars = ggplot2::vars(track),
                               free = list(x = FALSE, y = FALSE)) {
  if (!methods::is(x, "SynSpecies")) {
    stop("`store_chain_layout()` expects a SynSpecies object.", call. = FALSE)
  }
  species_layout(x) <- synspecies_chain_layout(x, vars = vars, free = free)
  x
}

#' Subset a pairwise alignment by query/target regions
#'
#' @param x A `SynSpecies` or `SynPairAlignment` object.
#' @param subset Named character vector/list with one region per species, e.g.
#'   `c(XZ1516 = "RagTag_V:21550000-21680000", N2 = "V:20450000-20451000")`.
#' @param alignment Optional alignment name when `x` is a `SynSpecies`.
#'
#' @return A filtered PAF-like `data.frame`.
#' @export
subset_pairwise_alignment <- function(x, subset, alignment = NULL) {
  pairwise_alignment_data(x, alignment = alignment, subset = subset)
}

#' Filter a pairwise alignment by minimum PAF alignment length
#'
#' @param x A `SynSpecies` or `SynPairAlignment` object.
#' @param filter Minimum `alen` to keep.
#' @param alignment Optional alignment name when `x` is a `SynSpecies`.
#'
#' @return A filtered PAF-like `data.frame`.
#' @export
filter_pairwise_alignment <- function(x, filter = 200, alignment = NULL) {
  pairwise_alignment_data(x, alignment = alignment, filter = filter)
}

#' Subset a comparative window from a `SynSpecies` object
#'
#' Uses a reference species and genomic window to find overlapping pairwise
#' alignments, derives the linked window on the partner genome from the
#' dominant PAF cluster, and trims both annotation layers plus the retained link
#' rows to the matched comparative region.
#'
#' @param x A `SynSpecies` object.
#' @param reference_species Individual name used as the starting coordinate
#'   system.
#' @param chr Chromosome/seqname on the reference species.
#' @param start Start coordinate on the reference species.
#' @param end End coordinate on the reference species.
#' @param alignment Optional pairwise alignment name. Required when multiple
#'   pairwise alignments exist and you want to choose a specific pair.
#' @param max_target_gap Optional maximum gap used when chaining nearby PAF hits
#'   on the partner genome. Defaults to `max(50000, 2 * window_width)`.
#'
#' @return A list with `windows`, `annotations`, and `links`.
#' @export
subset_synspecies_window <- function(x,
                                     reference_species,
                                     chr,
                                     start,
                                     end,
                                     alignment = NULL,
                                     max_target_gap = NULL) {
  if (!methods::is(x, "SynSpecies")) {
    stop("`subset_synspecies_window()` expects a SynSpecies object.", call. = FALSE)
  }
  if (!reference_species %in% names(individuals(x))) {
    stop(
      "`reference_species` must be one of: ",
      paste(names(individuals(x)), collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.numeric(start) || !is.numeric(end) || length(start) != 1L || length(end) != 1L) {
    stop("`start` and `end` must be numeric scalars.", call. = FALSE)
  }

  ref_start <- as.integer(min(start, end))
  ref_end <- as.integer(max(start, end))
  pair <- .resolve_subset_pairwise_alignment(
    x = x,
    reference_species = reference_species,
    alignment = alignment
  )

  query_species <- query_individual(pair)
  target_species <- target_individual(pair)
  if (!reference_species %in% c(query_species, target_species)) {
    stop(
      "Reference species ", reference_species,
      " is not part of pairwise alignment ", alignment_name(pair), ".",
      call. = FALSE
    )
  }

  partner_species <- setdiff(c(query_species, target_species), reference_species)[1L]
  reference_individual <- individuals(x)[[reference_species]]
  partner_individual <- individuals(x)[[partner_species]]

  ref_chr <- resolve_syn_seqname(reference_individual, chr)
  paf <- .read_pairwise_paf(alignment_file(pair))

  reference_on_query <- identical(reference_species, query_species)
  ref_cols <- if (reference_on_query) c(chr = "qchr", start = "qstart", end = "qend") else c(chr = "tchr", start = "tstart", end = "tend")
  partner_cols <- if (reference_on_query) c(chr = "tchr", start = "tstart", end = "tend") else c(chr = "qchr", start = "qstart", end = "qend")

  ref_chr_paf <- .resolve_paf_seqname(ref_chr, unique(as.character(paf[[ref_cols[["chr"]]]])))
  hits <- paf[
    as.character(paf[[ref_cols[["chr"]]]]) == ref_chr_paf &
      paf[[ref_cols[["start"]]]] < ref_end &
      paf[[ref_cols[["end"]]]] > ref_start,
    ,
    drop = FALSE
  ]

  if (nrow(hits) == 0L) {
    stop(
      "No PAF records overlap ", reference_species, ":", ref_chr, ":",
      ref_start, "-", ref_end, ".",
      call. = FALSE
    )
  }

  target_gap <- max_target_gap %||% max(50000L, as.integer((ref_end - ref_start + 1L) * 2L))
  cluster_hits <- .select_dominant_paf_cluster(
    hits = hits,
    ref_start = ref_start,
    ref_end = ref_end,
    ref_cols = ref_cols,
    partner_cols = partner_cols,
    max_target_gap = as.integer(target_gap)
  )

  partner_chr <- unique(as.character(cluster_hits[[partner_cols[["chr"]]]]))
  if (length(partner_chr) != 1L) {
    stop("Selected PAF cluster maps to multiple partner chromosomes.", call. = FALSE)
  }
  partner_chr <- resolve_syn_seqname(partner_individual, partner_chr[[1L]])
  partner_start <- min(cluster_hits[[partner_cols[["start"]]]])
  partner_end <- max(cluster_hits[[partner_cols[["end"]]]])

  cluster_hits <- cluster_hits[
    cluster_hits[[partner_cols[["start"]]]] < partner_end &
      cluster_hits[[partner_cols[["end"]]]] > partner_start,
    ,
    drop = FALSE
  ]

  windows <- list()
  windows[[reference_species]] <- data.frame(
    chr = ref_chr,
    start = ref_start,
    end = ref_end,
    stringsAsFactors = FALSE
  )
  windows[[partner_species]] <- data.frame(
    chr = partner_chr,
    start = as.integer(partner_start),
    end = as.integer(partner_end),
    stringsAsFactors = FALSE
  )

  annotations <- list()
  annotations[[reference_species]] <- .subset_annotation_window(
    reference_individual,
    chr = ref_chr,
    start = ref_start,
    end = ref_end
  )
  annotations[[partner_species]] <- .subset_annotation_window(
    partner_individual,
    chr = partner_chr,
    start = as.integer(partner_start),
    end = as.integer(partner_end)
  )

  cluster_hits$qspecies <- query_species
  cluster_hits$tspecies <- target_species
  cluster_hits$track <- paste0("link_", alignment_name(pair))

  list(
    windows = windows,
    annotations = annotations,
    links = cluster_hits
  )
}

.resolve_subset_pairwise_alignment <- function(x, reference_species, alignment = NULL) {
  pair_list <- pairwise_alignments(x)
  if (length(pair_list) == 0L) {
    stop("The SynSpecies object does not contain any pairwise alignments.", call. = FALSE)
  }

  if (is.null(alignment)) {
    if (length(pair_list) == 1L) {
      return(pair_list[[1L]])
    }
    synspecies_chain_species_order(x)
    stop(
      "For SynSpecies chains with multiple pairwise alignments, supply `alignment` to choose the pair for subsetting.",
      call. = FALSE
    )
  }

  if (!alignment %in% names(pair_list)) {
    stop(
      "Unknown pairwise alignment: ", alignment,
      ". Available alignments: ", paste(names(pair_list), collapse = ", "),
      call. = FALSE
    )
  }

  pair_list[[alignment]]
}

.resolve_pairwise_alignment_arg <- function(x, alignment = NULL) {
  pair_list <- pairwise_alignments(x)
  if (length(pair_list) == 0L) {
    stop("The SynSpecies object does not contain any pairwise alignments.", call. = FALSE)
  }

  if (is.null(alignment)) {
    if (length(pair_list) == 1L) {
      return(pair_list[[1L]])
    }
    stop(
      "Supply `alignment` to choose one pairwise alignment from: ",
      paste(names(pair_list), collapse = ", "),
      call. = FALSE
    )
  }

  if (!alignment %in% names(pair_list)) {
    stop(
      "Unknown pairwise alignment: ", alignment,
      ". Available alignments: ", paste(names(pair_list), collapse = ", "),
      call. = FALSE
    )
  }

  pair_list[[alignment]]
}

.read_pairwise_paf <- function(path) {
  paf <- utils::read.delim(
    path,
    header = FALSE,
    sep = "\t",
    stringsAsFactors = FALSE,
    quote = ""
  )
  if (ncol(paf) < 12L) {
    stop("PAF file must contain at least 12 columns: ", path, call. = FALSE)
  }

  colnames(paf)[seq_len(12L)] <- c(
    "qchr", "qlen", "qstart", "qend", "strand",
    "tchr", "tlen", "tstart", "tend", "nmatch", "alen", "mapq"
  )
  numeric_cols <- c("qlen", "qstart", "qend", "tlen", "tstart", "tend", "nmatch", "alen", "mapq")
  for (col in numeric_cols) {
    paf[[col]] <- as.integer(paf[[col]])
  }
  paf
}

.pairwise_alignment_data_impl <- function(x,
                                          species_obj = NULL,
                                          subset = NULL,
                                          filter = NULL) {
  paf <- .read_pairwise_paf(alignment_file(x))

  if (!is.null(subset)) {
    subset_specs <- .parse_pairwise_subset(
      subset = subset,
      pair = x,
      species_obj = species_obj,
      paf = paf
    )

    qspec <- subset_specs[[query_individual(x)]]
    tspec <- subset_specs[[target_individual(x)]]

    paf <- paf[
      as.character(paf$qchr) == qspec$chr &
        paf$qstart < qspec$end &
        paf$qend > qspec$start &
        as.character(paf$tchr) == tspec$chr &
        paf$tstart < tspec$end &
        paf$tend > tspec$start,
      ,
      drop = FALSE
    ]
  }

  if (!is.null(filter)) {
    if (!is.numeric(filter) || length(filter) != 1L || is.na(filter) || filter < 0) {
      stop("`filter` must be a non-negative numeric scalar.", call. = FALSE)
    }
    paf <- paf[paf$alen >= as.integer(filter), , drop = FALSE]
  }

  paf$qspecies <- query_individual(x)
  paf$tspecies <- target_individual(x)
  paf$track <- paste0("link_", alignment_name(x))
  rownames(paf) <- NULL
  paf
}

.parse_pairwise_subset <- function(subset, pair, species_obj = NULL, paf) {
  if (is.list(subset) && !is.atomic(subset)) {
    subset <- unlist(subset, use.names = TRUE)
  }
  if (!is.character(subset) || length(subset) != 2L || is.null(names(subset))) {
    stop(
      "`subset` must be a named character vector/list with one region for query and target species.",
      call. = FALSE
    )
  }

  subset <- subset[alignment_individuals(pair)]
  if (any(is.na(subset))) {
    stop(
      "`subset` must be named with both species in the pairwise alignment: ",
      paste(alignment_individuals(pair), collapse = ", "),
      call. = FALSE
    )
  }

  out <- lapply(names(subset), function(species_name) {
    spec <- .parse_region_string(subset[[species_name]])
    paf_chr_col <- if (identical(species_name, query_individual(pair))) "qchr" else "tchr"
    paf_chr <- .resolve_paf_seqname(spec$chr, unique(as.character(paf[[paf_chr_col]])))

    if (!is.null(species_obj) && methods::is(species_obj, "SynSpecies")) {
      individual <- individuals(species_obj)[[species_name]]
      spec$chr <- resolve_syn_seqname(individual, spec$chr)
    }
    spec$paf_chr <- paf_chr
    spec$chr <- paf_chr
    spec
  })
  names(out) <- names(subset)
  out
}

.parse_region_string <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop("Each `subset` entry must be a single region string.", call. = FALSE)
  }

  region <- gsub("\\s+", "", x)
  region <- chartr("\uFF1A", ":", region)
  region <- gsub(",", "", region, fixed = TRUE)

  m <- regexec("^([^:]+):(\\d+)-(\\d+)$", region)
  hits <- regmatches(region, m)[[1L]]
  if (length(hits) != 4L) {
    stop("Region must look like `chr:start-end`: ", x, call. = FALSE)
  }

  start <- as.integer(hits[[3L]])
  end <- as.integer(hits[[4L]])
  list(
    chr = hits[[2L]],
    start = min(start, end),
    end = max(start, end)
  )
}

.resolve_paf_seqname <- function(chr, available) {
  available <- unique(as.character(available))
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

  stop(
    "Unknown PAF seqname ", chr,
    ". Available seqnames include: ",
    paste(utils::head(available, 10L), collapse = ", "),
    call. = FALSE
  )
}

.select_dominant_paf_cluster <- function(hits,
                                         ref_start,
                                         ref_end,
                                         ref_cols,
                                         partner_cols,
                                         max_target_gap) {
  hits$ref_overlap_bp <- pmax(
    0L,
    pmin(hits[[ref_cols[["end"]]]], ref_end) - pmax(hits[[ref_cols[["start"]]]], ref_start)
  )
  hits <- hits[hits$ref_overlap_bp > 0L, , drop = FALSE]
  if (nrow(hits) == 0L) {
    stop("No PAF records remain after overlap filtering.", call. = FALSE)
  }

  hits <- hits[
    order(
      hits[[partner_cols[["chr"]]]],
      hits$strand,
      hits[[partner_cols[["start"]]]],
      hits[[partner_cols[["end"]]]]
    ),
    ,
    drop = FALSE
  ]

  new_cluster <- c(TRUE, rep(FALSE, nrow(hits) - 1L))
  if (nrow(hits) > 1L) {
    for (i in 2:nrow(hits)) {
      same_chr <- identical(
        as.character(hits[[partner_cols[["chr"]]]][i]),
        as.character(hits[[partner_cols[["chr"]]]][i - 1L])
      )
      same_strand <- identical(as.character(hits$strand[i]), as.character(hits$strand[i - 1L]))
      gap_bp <- hits[[partner_cols[["start"]]]][i] - hits[[partner_cols[["end"]]]][i - 1L]
      new_cluster[i] <- !(same_chr && same_strand && gap_bp <= max_target_gap)
    }
  }
  hits$cluster_id <- cumsum(new_cluster)

  cluster_summary <- stats::aggregate(
    cbind(ref_overlap_bp, alen) ~ cluster_id + strand,
    data = hits,
    FUN = sum
  )
  chr_summary <- stats::aggregate(
    hits[[partner_cols[["start"]]]],
    by = list(cluster_id = hits$cluster_id),
    FUN = min
  )
  colnames(chr_summary)[[2L]] <- "partner_start"
  cluster_summary <- merge(cluster_summary, chr_summary, by = "cluster_id", sort = FALSE)
  cluster_summary <- cluster_summary[
    order(-cluster_summary$ref_overlap_bp, -cluster_summary$alen, cluster_summary$partner_start),
    ,
    drop = FALSE
  ]

  hits[hits$cluster_id == cluster_summary$cluster_id[[1L]], , drop = FALSE]
}

.subset_annotation_window <- function(individual, chr, start, end) {
  individual <- load_annotation(individual)
  gr <- annotation_data(individual)
  window_gr <- GenomicRanges::GRanges(
    seqnames = chr,
    ranges = IRanges::IRanges(start = start, end = end)
  )
  gr[IRanges::overlapsAny(gr, window_gr)]
}
