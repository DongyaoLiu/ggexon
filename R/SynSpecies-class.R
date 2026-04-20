#' SynSpecies, SynLayout, and alignment classes
#'
#' These classes define the comparative object model used by `ggexon`.
#' `SynSpecies` groups multiple `SynIndividual` objects, `SynPairAlignment` and
#' `SynMultiAlignment` store the relationships between those individuals as
#' species-level annotations, and `SynLayout` stores reusable panel-layout
#' metadata for plotting.
#'
#' @name SynSpecies-class-overview
#' @section Class overview:
#' * `SynPairAlignment`: one pairwise alignment between two individuals
#' * `SynMultiAlignment`: one multiple alignment covering several individuals
#' * `SynLayout`: panel layout plus shared layout-scoped plotting defaults
#' * `SynSpecies`: top-level container that binds individuals, alignments, and
#'   an optional stored layout
#'
#' @keywords internal
NULL

#' SynPairAlignment class
#'
#' `SynPairAlignment` stores one pairwise alignment between two
#' `SynIndividual` objects in a `SynSpecies` collection. As a concrete
#' `SynSpeAnnotation`, the object keeps the shared annotation metadata together
#' with the query/target identifiers used to route link panels and optional
#' cached parsed alignment data.
#'
#' @slot name Unique alignment label used to retrieve the object from a
#'   `SynSpecies`.
#' @slot query_individual Query-side `SynIndividual` identifier.
#' @slot target_individual Target-side `SynIndividual` identifier.
#' @slot source_file Path to the alignment file on disk.
#' @slot format Alignment file format. Currently `"paf"` or `"odgi"`.
#' @slot data Optional cached parsed alignment data.
#' @slot metadata Optional user or import metadata.
#'
#' @section Prototype defaults:
#' * `annotation_scope = "species"`
#' * `lazy = TRUE`
#' * `loaded = FALSE`
#' * `format = "paf"`
#' * `data = NULL`
#'
#' @section Validity rules:
#' * `query_individual` and `target_individual` must each be one non-empty
#'   character value.
#' * `query_individual` and `target_individual` must differ.
#' * `format` must currently be `"paf"` or `"odgi"`.
#'
#' @exportClass SynPairAlignment
setClass(
  "SynPairAlignment",
  contains = "SynSpeAnnotation",
  slots = c(
    query_individual = "character",
    target_individual = "character",
    format = "character",
    data = "ANY"
  ),
  prototype = list(
    annotation_scope = "species",
    lazy = TRUE,
    loaded = FALSE,
    query_individual = NA_character_,
    target_individual = NA_character_,
    format = "paf",
    data = NULL
  ),
  validity = function(object) {
    problems <- character()
    if (length(object@query_individual) != 1L || is.na(object@query_individual) ||
        !nzchar(object@query_individual)) {
      problems <- c(problems, "`query_individual` must be a single non-empty character value.")
    }
    if (length(object@target_individual) != 1L || is.na(object@target_individual) ||
        !nzchar(object@target_individual)) {
      problems <- c(problems, "`target_individual` must be a single non-empty character value.")
    }
    if (identical(object@query_individual, object@target_individual)) {
      problems <- c(problems, "`query_individual` and `target_individual` must differ.")
    }
    if (length(object@format) != 1L || !(object@format %in% c("paf", "odgi"))) {
      problems <- c(problems, "`format` must currently be 'paf' or 'odgi'.")
    }
    if (length(problems) == 0L) TRUE else problems
  }
)

#' SynMultiAlignment class
#'
#' `SynMultiAlignment` stores one multiple alignment spanning more than two
#' individuals. Like `SynPairAlignment`, it is a concrete `SynSpeAnnotation`
#' with a source file, optional cached parsed representation, and metadata, but
#' it records a vector of participating individual identifiers instead of
#' query/target sides.
#'
#' @slot name Unique alignment label used to retrieve the object from a
#'   `SynSpecies`.
#' @slot individuals Character vector of included `SynIndividual` identifiers.
#' @slot source_file Path to the alignment file on disk.
#' @slot format Alignment file format. Currently `"maf"` or `"odgi"`.
#' @slot data Optional cached parsed alignment data.
#' @slot metadata Optional user or import metadata.
#'
#' @section Prototype defaults:
#' * `annotation_scope = "species"`
#' * `lazy = TRUE`
#' * `loaded = FALSE`
#' * `individuals = character()`
#' * `format = "maf"`
#' * `data = NULL`
#'
#' @section Validity rules:
#' * `individuals` must contain at least two non-empty character values.
#' * `individuals` must not contain duplicates.
#' * `format` must currently be `"maf"` or `"odgi"`.
#'
#' @exportClass SynMultiAlignment
setClass(
  "SynMultiAlignment",
  contains = "SynSpeAnnotation",
  slots = c(
    individuals = "character",
    format = "character",
    data = "ANY"
  ),
  prototype = list(
    annotation_scope = "species",
    lazy = TRUE,
    loaded = FALSE,
    individuals = character(),
    format = "maf",
    data = NULL
  ),
  validity = function(object) {
    problems <- character()
    if (length(object@individuals) < 2L || any(is.na(object@individuals)) ||
        any(!nzchar(object@individuals))) {
      problems <- c(problems, "`individuals` must contain at least two non-empty names.")
    }
    if (length(unique(object@individuals)) != length(object@individuals)) {
      problems <- c(problems, "`individuals` must not contain duplicates.")
    }
    if (length(object@format) != 1L || !(object@format %in% c("maf", "odgi"))) {
      problems <- c(problems, "`format` must currently be 'maf' or 'odgi'.")
    }
    if (length(problems) == 0L) TRUE else problems
  }
)

#' SynLayout class
#'
#' `SynLayout` stores panel placement information used by `facet_genomics()`
#' together with shared plotting defaults that can be resolved by syn-aware
#' geoms. The `panels` table describes the panel arrangement, while the numeric
#' slots store layout-scoped defaults such as shared exon height or x-axis
#' translation.
#'
#' @slot panels Layout data frame. It must contain `PANEL`, `ROW`, `COL`, and
#'   `track`, and may also contain comparative plotting columns such as
#'   `panel_type`, `species`, `alignment_name`, `tspecies`, `qspecies`,
#'   `t_panel`, and `q_panel`.
#' @slot layout_type Scalar layout strategy label such as `"custom"` or
#'   `"chain"`.
#' @slot free List with logical `x` and `y` entries describing whether scales
#'   should vary across panels.
#' @slot exon_height Shared default exon or gene block height for
#'   layout-aware annotation geoms.
#' @slot y_scale Shared default y scaling for layout-aware annotation geoms.
#' @slot x_translation Shared default x-axis offset for layout-aware annotation
#'   geoms.
#' @slot metadata Optional layout metadata.
#'
#' @section Prototype defaults:
#' * `panels = data.frame()`
#' * `layout_type = "custom"`
#' * `free = list(x = FALSE, y = FALSE)`
#' * `exon_height = NA_real_`
#' * `y_scale = NA_real_`
#' * `x_translation = NA_real_`
#' * `metadata = list()`
#'
#' @section Validity rules:
#' * `panels` must contain at least the columns `PANEL`, `ROW`, `COL`, and
#'   `track`.
#' * `layout_type` must be one non-empty character value.
#' * `free` must be a list with scalar logical `x` and `y` entries.
#' * `exon_height`, `y_scale`, and `x_translation` must each be scalar numeric
#'   values.
#'
#' @exportClass SynLayout
setClass(
  "SynLayout",
  slots = c(
    panels = "data.frame",
    layout_type = "character",
    free = "list",
    exon_height = "numeric",
    y_scale = "numeric",
    x_translation = "numeric",
    metadata = "list"
  ),
  prototype = list(
    panels = data.frame(),
    layout_type = "custom",
    free = list(x = FALSE, y = FALSE),
    exon_height = NA_real_,
    y_scale = NA_real_,
    x_translation = NA_real_,
    metadata = list()
  ),
  validity = function(object) {
    problems <- character()
    required_layout_cols <- c("PANEL", "ROW", "COL", "track")
    missing_layout_cols <- setdiff(required_layout_cols, colnames(object@panels))
    if (length(missing_layout_cols) > 0L) {
      problems <- c(
        problems,
        paste0(
          "`panels` is missing required columns: ",
          paste(missing_layout_cols, collapse = ", "),
          "."
        )
      )
    }
    if (length(object@layout_type) != 1L || is.na(object@layout_type) || !nzchar(object@layout_type)) {
      problems <- c(problems, "`layout_type` must be a single non-empty character value.")
    }
    if (!all(c("x", "y") %in% names(object@free))) {
      problems <- c(problems, "`free` must be a list with `x` and `y` entries.")
    } else if (!is.logical(object@free$x) || length(object@free$x) != 1L ||
               !is.logical(object@free$y) || length(object@free$y) != 1L) {
      problems <- c(problems, "`free$x` and `free$y` must be single logical values.")
    }
    if (!is.numeric(object@exon_height) || length(object@exon_height) != 1L) {
      problems <- c(problems, "`exon_height` must be a single numeric value.")
    }
    if (!is.numeric(object@y_scale) || length(object@y_scale) != 1L) {
      problems <- c(problems, "`y_scale` must be a single numeric value.")
    }
    if (!is.numeric(object@x_translation) || length(object@x_translation) != 1L) {
      problems <- c(problems, "`x_translation` must be a single numeric value.")
    }
    if (length(problems) == 0L) TRUE else problems
  }
)
setClassUnion("NULLOrSynLayout", c("NULL", "SynLayout"))

#' SynSpecies class
#'
#' `SynSpecies` is the top-level comparative container in `ggexon`. It groups
#' named `SynIndividual` objects together with any stored pairwise or multiple
#' alignments, optional metadata, and an optional reusable `SynLayout`.
#'
#' @slot name Scalar species-collection label.
#' @slot individuals Named list of `SynIndividual` objects.
#' @slot pairwise_alignments Named list of `SynPairAlignment` objects.
#' @slot multiple_alignments Named list of `SynMultiAlignment` objects.
#' @slot metadata Optional user or import metadata.
#' @slot layout Optional stored `SynLayout` used by `facet_genomics()` and
#'   syn-aware plot building.
#'
#' @section Prototype defaults:
#' * `individuals = list()`
#' * `pairwise_alignments = list()`
#' * `multiple_alignments = list()`
#' * `metadata = list()`
#' * `layout = NULL`
#'
#' @section Validity rules:
#' * `name` must be one non-empty character value.
#' * `individuals` must contain only `SynIndividual` objects.
#' * `pairwise_alignments` must contain only `SynPairAlignment` objects.
#' * `multiple_alignments` must contain only `SynMultiAlignment` objects.
#' * `layout` must be either `NULL` or a `SynLayout`.
#'
#' @exportClass SynSpecies
setClass(
  "SynSpecies",
  slots = c(
    name = "character",
    individuals = "list",
    pairwise_alignments = "list",
    multiple_alignments = "list",
    metadata = "list",
    layout = "NULLOrSynLayout"
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
    if (!is.null(object@layout) && !methods::is(object@layout, "SynLayout")) {
      problems <- c(problems, "`layout` must be a SynLayout object or NULL.")
    }
    if (length(problems) == 0L) TRUE else problems
  }
)

#' Constructor for SynLayout
#'
#' @param panels Panel layout table. At minimum it must contain `PANEL`, `ROW`,
#'   `COL`, and `track`.
#' @param layout_type Layout strategy label, such as `"custom"` or `"chain"`.
#' @param free List with logical `x` and `y` entries controlling free-scale
#'   behavior across panels.
#' @param exon_height Default shared exon/gene/gene-label height resolved by
#'   syn-aware annotation geoms.
#' @param y_scale Default shared y-axis scaling for layout-aware geoms.
#' @param x_translation Default shared x-axis translation applied to
#'   layout-aware geoms.
#' @param metadata Optional metadata list.
#'
#' @return A `SynLayout` object.
#' @export
SynLayout <- function(panels,
                      layout_type = "custom",
                      free = list(x = FALSE, y = FALSE),
                      exon_height = NA_real_,
                      y_scale = NA_real_,
                      x_translation = NA_real_,
                      metadata = list()) {
  new(
    "SynLayout",
    panels = panels,
    layout_type = layout_type,
    free = free,
    exon_height = exon_height,
    y_scale = y_scale,
    x_translation = x_translation,
    metadata = metadata
  )
}

#' Constructor for SynPairAlignment
#'
#' @param name Alignment label.
#' @param query_individual Query-side individual name.
#' @param target_individual Target-side individual name.
#' @param file Path to the alignment file.
#' @param format Alignment format. Currently `"paf"` or `"odgi"`.
#' @param data Optional cached parsed alignment representation.
#' @param metadata Optional metadata list.
#'
#' @return A `SynPairAlignment` object.
#' @export
SynPairAlignment <- function(name,
                             query_individual,
                             target_individual,
                             file,
                             format = c("paf", "odgi"),
                             data = NULL,
                             metadata = list()) {
  format <- match.arg(format)
  new(
    "SynPairAlignment",
    name = name,
    source_file = file,
    query_individual = query_individual,
    target_individual = target_individual,
    format = format,
    data = data,
    metadata = metadata
  )
}

#' Constructor for SynMultiAlignment
#'
#' @param name Alignment label.
#' @param individuals Character vector of included individuals.
#' @param file Path to the alignment file.
#' @param format Alignment format. Currently `"maf"` and `"odgi"` are
#'   supported.
#' @param data Optional cached parsed alignment representation.
#' @param metadata Optional metadata list.
#'
#' @return A `SynMultiAlignment` object.
#' @export
SynMultiAlignment <- function(name,
                              individuals,
                              file,
                              format = c("maf", "odgi"),
                              data = NULL,
                              metadata = list()) {
  format <- match.arg(format)
  new(
    "SynMultiAlignment",
    name = name,
    source_file = file,
    individuals = individuals,
    format = format,
    data = data,
    lazy = is.null(data),
    loaded = !is.null(data),
    metadata = metadata
  )
}

#' Constructor for SynSpecies
#'
#' @param name Species collection label. If omitted and `annotation_folder` is
#'   supplied, the folder basename is used.
#' @param annotation_folder Optional directory containing `.gff`, `.gff3`, or
#'   `.gtf` files to import immediately as annotation-only `SynIndividual`
#'   objects.
#' @param annotation_format One of `"auto"`, `"gff"`, or `"gtf"`. Used only
#'   when `annotation_folder` is supplied.
#' @param recursive Logical; should annotation discovery recurse into
#'   subfolders? Used only when `annotation_folder` is supplied.
#' @param metadata Optional metadata list.
#'
#' @return A `SynSpecies` object. When `annotation_folder` is provided, the
#'   object is initialized with one annotation-only `SynIndividual` per
#'   supported annotation file found in that folder.
#' @export
SynSpecies <- function(name = NULL,
                       annotation_folder = NULL,
                       annotation_format = c("auto", "gff", "gtf"),
                       recursive = FALSE,
                       metadata = list()) {
  annotation_format <- match.arg(annotation_format)

  if (is.null(name)) {
    if (is.null(annotation_folder)) {
      stop(
        "`name` must be supplied when `annotation_folder` is not provided.",
        call. = FALSE
      )
    }
    name <- .synspecies_name_from_folder(annotation_folder)
  }

  x <- new("SynSpecies", name = name, metadata = metadata)

  if (!is.null(annotation_folder)) {
    x <- add_individuals_from_folder(
      x,
      folder = annotation_folder,
      annotation_format = annotation_format,
      recursive = recursive
    )
  }

  x
}

#' @export
setMethod("show", "SynSpecies", function(object) {
  cat("An object of class \"SynSpecies\"\n")
  cat("  name:", object@name, "\n")
  cat("  individuals:", length(object@individuals), "\n")
  cat("  pairwise_alignments:", length(object@pairwise_alignments), "\n")
  cat("  multiple_alignments:", length(object@multiple_alignments), "\n")
})

#' @export
setMethod("show", "SynLayout", function(object) {
  cat("An object of class \"SynLayout\"\n")
  cat("  layout_type:", object@layout_type, "\n")
  cat("  panels:", nrow(object@panels), "\n")
  cat("  free x/y:", isTRUE(object@free$x), "/", isTRUE(object@free$y), "\n")
  cat("  exon_height:", object@exon_height, "\n")
  cat("  y_scale:", object@y_scale, "\n")
  cat("  x_translation:", object@x_translation, "\n")
})

setAs("SynLayout", "data.frame", function(from) from@panels)
setMethod("as.data.frame", "SynLayout", function(x, ...) x@panels)

infer_syn_layout_type <- function(panels) {
  if ("panel_type" %in% names(panels) && any(panels$panel_type == "link", na.rm = TRUE)) {
    return("chain")
  }
  "custom"
}

infer_syn_layout_free <- function(panels) {
  list(
    x = "SCALE_X" %in% names(panels) && length(unique(stats::na.omit(panels$SCALE_X))) > 1L,
    y = "SCALE_Y" %in% names(panels) && length(unique(stats::na.omit(panels$SCALE_Y))) > 1L
  )
}

as_syn_layout <- function(x,
                          layout_type = NULL,
                          free = NULL,
                          exon_height = NA_real_,
                          y_scale = NA_real_,
                          x_translation = NA_real_,
                          metadata = list()) {
  if (is.null(x)) {
    return(NULL)
  }
  if (methods::is(x, "SynLayout")) {
    return(x)
  }
  if (!is.data.frame(x)) {
    stop("`as_syn_layout()` expects a SynLayout, data.frame, or NULL.", call. = FALSE)
  }

  panels <- .normalize_synspecies_layout_order(x)
  SynLayout(
    panels = panels,
    layout_type = layout_type %||% infer_syn_layout_type(panels),
    free = free %||% infer_syn_layout_free(panels),
    exon_height = exon_height,
    y_scale = y_scale,
    x_translation = x_translation,
    metadata = metadata
  )
}

syn_layout_panels <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (methods::is(x, "SynLayout")) {
    return(x@panels)
  }
  if (is.data.frame(x)) {
    return(x)
  }
  stop("Expected a SynLayout, data.frame, or NULL.", call. = FALSE)
}

setGeneric("species_name", function(x) standardGeneric("species_name"))
setMethod("species_name", "SynSpecies", function(x) x@name)

setGeneric("individuals", function(x) standardGeneric("individuals"))
setMethod("individuals", "SynSpecies", function(x) x@individuals)

#' Return the individual identifiers stored on a `SynSpecies`
#'
#' Returns the names used to index the `SynIndividual` objects attached to a
#' [`SynSpecies`]. These usually match `names(individuals(x))`. If unnamed
#' entries are present, the accessor falls back to `syn_id()` for those
#' objects.
#'
#' @param x A `SynSpecies` object.
#'
#' @return A character vector of individual identifiers in stored order.
#' @export
setGeneric("individual_names", function(x) standardGeneric("individual_names"))
setMethod("individual_names", "SynSpecies", function(x) {
  out <- names(x@individuals)
  if (is.null(out)) {
    out <- rep("", length(x@individuals))
  }

  needs_fallback <- is.na(out) | !nzchar(out)
  if (any(needs_fallback)) {
    out[needs_fallback] <- vapply(x@individuals[needs_fallback], syn_id, character(1))
  }

  out
})

setGeneric("pairwise_alignments", function(x) standardGeneric("pairwise_alignments"))
setMethod("pairwise_alignments", "SynSpecies", function(x) x@pairwise_alignments)

setGeneric("multiple_alignments", function(x) standardGeneric("multiple_alignments"))
setMethod("multiple_alignments", "SynSpecies", function(x) x@multiple_alignments)

setGeneric("species_layout", function(x) standardGeneric("species_layout"))
setMethod("species_layout", "SynSpecies", function(x) x@layout)

setGeneric("alignment_name", function(x) standardGeneric("alignment_name"))
setMethod("alignment_name", "SynPairAlignment", function(x) annotation_name(x))
setMethod("alignment_name", "SynMultiAlignment", function(x) annotation_name(x))

setGeneric("alignment_file", function(x) standardGeneric("alignment_file"))
setMethod("alignment_file", "SynPairAlignment", function(x) source_file(x))
setMethod("alignment_file", "SynMultiAlignment", function(x) source_file(x))

setGeneric("alignment_format", function(x) standardGeneric("alignment_format"))
setMethod("alignment_format", "SynPairAlignment", function(x) x@format)
setMethod("alignment_format", "SynMultiAlignment", function(x) x@format)

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
setMethod("pairwise_alignment_data", "SynPairAlignment", function(x, alignment = NULL, ..., odgi = NULL, python = NULL) {
  .pairwise_alignment_data_impl(
    x = x,
    species_obj = NULL,
    odgi = odgi,
    python = python,
    ...
  )
})
setMethod("pairwise_alignment_data", "SynSpecies", function(x, alignment = NULL, ..., odgi = NULL, python = NULL) {
  pair <- .resolve_pairwise_alignment_arg(x = x, alignment = alignment)
  .pairwise_alignment_data_impl(
    x = pair,
    species_obj = x,
    odgi = odgi,
    python = python,
    ...
  )
})

.annotation_folder_pattern <- function(annotation_format = c("auto", "gff", "gtf")) {
  annotation_format <- match.arg(annotation_format)
  switch(
    annotation_format,
    auto = "\\.(gff3?|gtf)(\\.gz)?$",
    gff = "\\.(gff3?)(\\.gz)?$",
    gtf = "\\.gtf(\\.gz)?$"
  )
}

.annotation_id_from_path <- function(path) {
  file_name <- basename(path)
  if (grepl("\\.gz$", file_name, ignore.case = TRUE)) {
    file_name <- tools::file_path_sans_ext(file_name)
  }
  tools::file_path_sans_ext(file_name)
}

.synspecies_name_from_folder <- function(folder) {
  folder_name <- basename(normalizePath(folder, winslash = "/", mustWork = FALSE))
  if (is.na(folder_name) || !nzchar(folder_name) || identical(folder_name, ".")) {
    return("SynSpecies")
  }
  folder_name
}

.annotation_format_from_path <- function(path) {
  file_name <- basename(path)
  if (grepl("\\.gz$", file_name, ignore.case = TRUE)) {
    file_name <- tools::file_path_sans_ext(file_name)
  }

  ext <- base::tolower(tools::file_ext(file_name))
  switch(
    ext,
    gff = "gff",
    gff3 = "gff",
    gtf = "gtf",
    stop("Unsupported annotation file extension: ", path, call. = FALSE)
  )
}

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

#' Add many annotation files from a folder as SynIndividuals
#'
#' Discovers supported annotation files in a folder and adds one
#' [`SynIndividual`] per file to a [`SynSpecies`] object. Supported extensions
#' are `.gff`, `.gff3`, and `.gtf` (optionally with a trailing `.gz`). When
#' `annotation_format = "auto"`, the format is inferred from the file
#' extension.
#'
#' Individual ids default to the filename stem with the annotation extension
#' removed, so a file such as `N2.gff3` becomes a `SynIndividual` with id
#' `"N2"`.
#'
#' Genome files are waived by default for this convenience import. That makes
#' the helper suitable for annotation-only workflows, while genome-dependent
#' operations can still be added later by replacing or rebuilding the
#' `SynIndividual` objects with FASTA paths.
#'
#' @param x A [`SynSpecies`] object.
#' @param folder Path to a directory containing annotation files.
#' @param annotation_format One of `"auto"`, `"gff"`, or `"gtf"`. When `"auto"`,
#'   files with supported extensions are discovered and each file's format is
#'   inferred from its extension. When `"gff"` or `"gtf"`, only files with
#'   matching extensions are imported.
#' @param recursive Logical; should files be discovered recursively?
#'
#' @return An updated [`SynSpecies`] object.
#' @export
add_individuals_from_folder <- function(x,
                                        folder,
                                        annotation_format = c("auto", "gff", "gtf"),
                                        recursive = FALSE) {
  if (!methods::is(x, "SynSpecies")) {
    stop("`add_individuals_from_folder()` expects a SynSpecies object.", call. = FALSE)
  }

  annotation_format <- match.arg(annotation_format)

  if (!is.character(folder) || length(folder) != 1L || is.na(folder) || !nzchar(folder)) {
    stop("`folder` must be a single non-empty character value.", call. = FALSE)
  }
  if (!dir.exists(folder)) {
    stop("Folder does not exist: ", folder, call. = FALSE)
  }
  if (!is.logical(recursive) || length(recursive) != 1L || is.na(recursive)) {
    stop("`recursive` must be a single TRUE/FALSE value.", call. = FALSE)
  }

  annotation_files <- list.files(
    path = folder,
    pattern = .annotation_folder_pattern(annotation_format),
    full.names = TRUE,
    recursive = recursive,
    ignore.case = TRUE
  )
  annotation_files <- sort(annotation_files)

  if (length(annotation_files) == 0L) {
    stop(
      "No annotation files with supported extensions were found in: ",
      folder,
      call. = FALSE
    )
  }

  ids <- vapply(annotation_files, .annotation_id_from_path, character(1))
  if (anyDuplicated(ids)) {
    duplicated_ids <- unique(ids[duplicated(ids)])
    stop(
      "Annotation filenames resolve to duplicate individual ids: ",
      paste(duplicated_ids, collapse = ", "),
      call. = FALSE
    )
  }

  for (i in seq_along(annotation_files)) {
    file_format <- if (identical(annotation_format, "auto")) {
      .annotation_format_from_path(annotation_files[[i]])
    } else {
      annotation_format
    }

    x <- add_individual(
      x,
      SynIndividual(
        genome_file = genome_waiver(),
        annotation_file = annotation_files[[i]],
        id = ids[[i]],
        annotation_format = file_format
      )
    )
  }

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
#' @param value A `SynLayout`, layout `data.frame`, or `NULL`.
#'
#' @return The updated `SynSpecies` object.
#' @export
setGeneric("species_layout<-", function(x, value) standardGeneric("species_layout<-"))
setReplaceMethod("species_layout", "SynSpecies", function(x, value) {
  x@layout <- as_syn_layout(value)
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

#' Subset one or more individuals in a `SynSpecies` by coordinate string
#'
#' Returns a new [`SynSpecies`] in which selected individuals have their
#' feature annotation layers trimmed according to coordinate strings such as
#' `"XZ1516#V_RagTag:21559983-21620009"`. Individuals not listed in `coords`
#' are left unchanged. Any stored [`SynLayout`] is cleared because the panel
#' metadata may no longer match the new subsetted windows.
#'
#' @param x A `SynSpecies` object.
#' @param coords One or more coordinate strings in the form
#'   `"species#seqname:start-end"`. This can be a single string, a character
#'   vector, or a list of strings.
#' @param annotations One of `"all_feature"` or `"active"`. Passed through to
#'   [subset_individual()].
#'
#' @return A `SynSpecies` object.
#' @export
subset_species <- function(x,
                           coords,
                           annotations = c("all_feature", "active")) {
  if (!methods::is(x, "SynSpecies")) {
    stop("`subset_species()` expects a SynSpecies object.", call. = FALSE)
  }

  annotations <- match.arg(annotations)
  windows <- .parse_species_window_coords(coords)
  missing_species <- setdiff(names(windows), names(individuals(x)))
  if (length(missing_species) > 0L) {
    stop(
      "`coords` references species not present in the SynSpecies object: ",
      paste(missing_species, collapse = ", "),
      call. = FALSE
    )
  }

  out <- x
  subsetted_individuals <- individuals(out)
  for (species_name in names(windows)) {
    window <- windows[[species_name]]
    subsetted_individuals[[species_name]] <- subset_individual(
      subsetted_individuals[[species_name]],
      chr = window$chr,
      start = window$start,
      end = window$end,
      annotations = annotations
    )
  }

  out@individuals <- subsetted_individuals
  out@layout <- NULL
  validObject(out)
  out
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

.pairwise_alignment_table <- function(x, odgi = NULL, python = NULL) {
  paf <- if (!is.null(x@data)) {
    x@data
  } else if (identical(alignment_format(x), "paf")) {
    .read_pairwise_paf(alignment_file(x))
  } else if (identical(alignment_format(x), "odgi")) {
    .read_odgi_pairwise_alignment(
      x,
      odgi = odgi,
      python = python
    )
  } else {
    stop(
      "Unsupported pairwise alignment format: ",
      alignment_format(x),
      call. = FALSE
    )
  }

  if (!"qspecies" %in% names(paf)) {
    paf$qspecies <- query_individual(x)
  }
  if (!"tspecies" %in% names(paf)) {
    paf$tspecies <- target_individual(x)
  }
  if (!"track" %in% names(paf)) {
    paf$track <- paste0("link_", alignment_name(x))
  }

  rownames(paf) <- NULL
  paf
}

.pairwise_alignment_data_impl <- function(x,
                                          species_obj = NULL,
                                          subset = NULL,
                                          filter = NULL,
                                          odgi = NULL,
                                          python = NULL) {
  paf <- .pairwise_alignment_table(x, odgi = odgi, python = python)

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

  rownames(paf) <- NULL
  paf
}

#' Load alignment data into Syn-aware alignment objects
#'
#' Parses supported alignment files and caches the parsed data on alignment
#' objects. Pairwise alignments are currently loaded from PAF files, and
#' multiple alignments can be loaded when `format = "odgi"` points to an ODGI
#' node-table TSV. When `x` is a [`SynSpecies`], every stored pairwise and
#' multiple alignment is loaded and the updated `SynSpecies` object is
#' returned.
#'
#' Unloaded `SynMultiAlignment` objects with `format = "maf"` are not yet
#' supported because the package does not currently provide a MAF parser.
#'
#' @param x A [`SynPairAlignment`], [`SynMultiAlignment`], or [`SynSpecies`]
#'   object.
#' @param odgi Optional path to the `odgi` executable. Used when loading ODGI
#'   multiple alignments from raw `.og` graph files.
#' @param python Optional path to the Python interpreter. Used when loading
#'   ODGI multiple alignments from raw `.og` graph files.
#'
#' @return An updated object of the same class as `x`.
#' @export
load_alignment <- function(x, odgi = NULL, python = NULL) {
  if (methods::is(x, "SynPairAlignment")) {
    if (is.null(x@data)) {
      x@data <- .pairwise_alignment_table(x, odgi = odgi, python = python)
    }
    x@loaded <- TRUE
    x@lazy <- FALSE
    return(x)
  }

  if (methods::is(x, "SynMultiAlignment")) {
    if (is.null(x@data)) {
      if (!identical(alignment_format(x), "odgi")) {
        stop(
          "`load_alignment()` currently supports unloaded SynMultiAlignment objects only when `format = 'odgi'`.",
          call. = FALSE
        )
      }
      x@data <- multiple_alignment_data(x, odgi = odgi, python = python)
    }
    x@loaded <- TRUE
    x@lazy <- FALSE
    return(x)
  }

  if (methods::is(x, "SynSpecies")) {
    pairs <- pairwise_alignments(x)
    if (length(pairs) > 0L) {
      x@pairwise_alignments <- lapply(pairs, load_alignment, odgi = odgi, python = python)
    }

    multis <- multiple_alignments(x)
    if (length(multis) > 0L) {
      x@multiple_alignments <- lapply(multis, load_alignment, odgi = odgi, python = python)
    }

    validObject(x)
    return(x)
  }

  stop(
    "`load_alignment()` expects a SynPairAlignment, SynMultiAlignment, or SynSpecies object.",
    call. = FALSE
  )
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
  query_features(
    individual,
    chr = chr,
    start = start,
    end = end,
    feature_type = NULL
  )
}
