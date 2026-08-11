#' SynSpecies, SynLayout, and alignment classes
#'
#' These classes define the comparative object model used by `ggexon`.
#' `SynSpecies` groups multiple `SynIndividual` objects, `SynPairAlignment` and
#' `SynMultiAlignment` store the relationships between those individuals as
#' species-level annotations, and `SynLayout` stores reusable panel-layout
#' metadata for plotting.
#'
#' @include homology-annotation.R locus-set.R
#' @name SynSpecies-class-overview
#' @section Class overview:
#' * `SynPairAlignment`: one pairwise alignment between two individuals
#' * `SynMultiAlignment`: one multiple alignment covering several individuals
#' * `SynLocusSet`: one table of comparable locus windows for grid layouts
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
#' cached parsed alignment data. For PSL-backed alignments, the cached table can
#' be stored either at the original one-row-per-record level or at a detailed
#' one-row-per-ungapped-block level when loaded with `load_alignment(more =
#' TRUE)`.
#'
#' @slot name Unique alignment label used to retrieve the object from a
#'   `SynSpecies`.
#' @slot query_individual Query-side `SynIndividual` identifier.
#' @slot target_individual Target-side `SynIndividual` identifier.
#' @slot source_file Path to the alignment file on disk.
#' @slot format Alignment file format. Currently `"paf"`, `"psl"`, or `"odgi"`.
#' @slot data Optional cached parsed alignment data. For PSL files this can be
#'   either one row per PSL record or one row per ungapped block, depending on
#'   how the object was loaded.
#' @slot metadata Optional user or import metadata. Loader state such as the
#'   cached PSL detail mode may also be stored here.
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
#' * `format` must currently be `"paf"`, `"psl"`, or `"odgi"`.
#'
#' @section Cached PSL detail modes:
#' * `load_alignment(more = FALSE)` keeps one cached row per PSL record.
#' * `load_alignment(more = TRUE)` expands each PSL record into one cached row
#'   per ungapped block and records that detail level in `metadata$psl_more`.
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
    if (length(object@format) != 1L || !(object@format %in% c("paf", "psl", "odgi"))) {
      problems <- c(problems, "`format` must currently be 'paf', 'psl', or 'odgi'.")
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
#'   `t_panel`, `q_panel`, `x_source_panel`, `SCALE_X`, `SCALE_Y`, and optional
#'   panel-specific x-window columns `xlim_chr`, `xlim_min`, and `xlim_max`.
#' @slot layout_type Scalar layout strategy label such as `"custom"` or
#'   `"chain"`.
#' @slot free List with logical `x` and `y` entries describing whether scales
#'   should vary across panels.
#' @slot exon_height Shared default exon or gene block height for
#'   layout-aware annotation geoms.
#' @slot x_translation Shared default x-axis offset for layout-aware annotation
#'   geoms.
#' @slot metadata Optional layout metadata.
#'
#' @section Prototype defaults:
#' * `panels = data.frame()`
#' * `layout_type = "custom"`
#' * `free = list(x = FALSE, y = FALSE)`
#' * `exon_height = NA_real_`
#' * `x_translation = NA_real_`
#' * `metadata = list()`
#'
#' @section Panel roles and inherited scales:
#' Syn-aware layouts use explicit `panel_type` values such as `"annotation"`,
#' `"coverage"`, and `"link"`. The same public `track` may therefore occur in
#' more than one role. `SCALE_Y` is the authoritative inherited scale-object
#' identity: panels with equal values share training, while panels with
#' different values train independently. Resolved role policies may be kept in
#' `metadata$panel_role_y_policies` so older or serialized layouts preserve
#' their fixed/free interpretation.
#'
#' @section Validity rules:
#' * `panels` must contain at least the columns `PANEL`, `ROW`, `COL`, and
#'   `track`.
#' * when `panels` contains any of `xlim_chr`, `xlim_min`, or `xlim_max`, it
#'   must contain all three columns.
#' * annotation panels with panel-specific x limits must provide complete
#'   `xlim_min` and `xlim_max` values. `xlim_chr` may be missing for numeric
#'   display-only windows that should not drive annotation filtering.
#' * when multiple annotation panels define different x windows,
#'   `free$x` must be `TRUE`.
#' * `layout_type` must be one non-empty character value.
#' * `free` must be a list with scalar logical `x` and `y` entries.
#' * `exon_height` and `x_translation` must each be scalar numeric values.
#'
#' @exportClass SynLayout
setClass(
  "SynLayout",
  slots = c(
    panels = "data.frame",
    layout_type = "character",
    free = "list",
    exon_height = "numeric",
    x_translation = "numeric",
    metadata = "list"
  ),
  prototype = list(
    panels = data.frame(),
    layout_type = "custom",
    free = list(x = FALSE, y = FALSE),
    exon_height = NA_real_,
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
    xlim_cols <- c("xlim_chr", "xlim_min", "xlim_max")
    present_xlim_cols <- intersect(xlim_cols, colnames(object@panels))
    if (length(present_xlim_cols) > 0L && length(present_xlim_cols) != length(xlim_cols)) {
      problems <- c(
        problems,
        "`panels` must contain all of `xlim_chr`, `xlim_min`, and `xlim_max` when panel-specific x limits are used."
      )
    }
    if (length(present_xlim_cols) == length(xlim_cols)) {
      panels <- object@panels
      annotation_rows <- if ("panel_type" %in% colnames(panels)) {
        is.na(panels$panel_type) | panels$panel_type == "annotation"
      } else {
        rep(TRUE, nrow(panels))
      }
      has_any_xlim <- annotation_rows & (
        !is.na(panels$xlim_min) | !is.na(panels$xlim_max) | !is.na(panels$xlim_chr)
      )
      incomplete_xlim <- has_any_xlim & (
        is.na(panels$xlim_min) | is.na(panels$xlim_max)
      )
      if (any(incomplete_xlim)) {
        bad_tracks <- unique(as.character(panels$track[incomplete_xlim]))
        problems <- c(
          problems,
          paste0(
            "Annotation panels with panel-specific x limits must provide `xlim_min` and `xlim_max`. Problem tracks: ",
            paste(bad_tracks, collapse = ", "),
            "."
          )
        )
      }
      complete_xlim <- has_any_xlim & !incomplete_xlim
      if (any(complete_xlim)) {
        xlim_df <- unique(data.frame(
          xlim_chr = as.character(panels$xlim_chr[complete_xlim]),
          xlim_min = as.numeric(panels$xlim_min[complete_xlim]),
          xlim_max = as.numeric(panels$xlim_max[complete_xlim]),
          stringsAsFactors = FALSE
        ))
        bad_order <- xlim_df$xlim_min > xlim_df$xlim_max
        if (any(bad_order)) {
          problems <- c(problems, "`xlim_min` must be less than or equal to `xlim_max` for panel-specific x limits.")
        }
        if (nrow(xlim_df) > 1L && !isTRUE(object@free$x)) {
          problems <- c(
            problems,
            "Set `free$x = TRUE` when multiple annotation panels define different panel-specific x limits."
          )
        }
      }
    }
    if (!is.numeric(object@exon_height) || length(object@exon_height) != 1L) {
      problems <- c(problems, "`exon_height` must be a single numeric value.")
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
#' alignments, optional tree/tree plot objects, optional metadata, and an
#' optional reusable `SynLayout`.
#'
#' @slot name Scalar species-collection label.
#' @slot individuals Named list of `SynIndividual` objects.
#' @slot pairwise_alignments Named list of `SynPairAlignment` objects.
#' @slot multiple_alignments Named list of `SynMultiAlignment` objects.
#' @slot tree Optional tree object, such as an `ape::phylo`.
#' @slot tree_plot Optional tree plot object, such as a rectangular `ggtree`
#'   plot.
#' @slot metadata Optional user or import metadata.
#' @slot layout Optional stored `SynLayout` used by `facet_genomics()` and
#'   syn-aware plot building.
#' @slot homology_annotations Named list of `HomologyAnnotation` objects
#'   storing cross-species gene homology mappings.
#' @slot locus_sets Named list of `SynLocusSet` objects storing comparable
#'   locus windows for multi-locus grid layouts.
#'
#' @section Prototype defaults:
#' * `individuals = list()`
#' * `pairwise_alignments = list()`
#' * `multiple_alignments = list()`
#' * `homology_annotations = list()`
#' * `locus_sets = list()`
#' * `tree = NULL`
#' * `tree_plot = NULL`
#' * `metadata = list()`
#' * `layout = NULL`
#'
#' @section Validity rules:
#' * `name` must be one non-empty character value.
#' * `individuals` must contain only `SynIndividual` objects.
#' * `pairwise_alignments` must contain only `SynPairAlignment` objects.
#' * `multiple_alignments` must contain only `SynMultiAlignment` objects.
#' * `homology_annotations` must contain only `HomologyAnnotation` objects.
#' * `locus_sets` must contain only `SynLocusSet` objects.
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
    homology_annotations = "list",
    locus_sets = "list",
    tree = "ANY",
    tree_plot = "ANY",
    metadata = "list",
    layout = "NULLOrSynLayout"
  ),
  prototype = list(
    name = NA_character_,
    individuals = list(),
    pairwise_alignments = list(),
    multiple_alignments = list(),
    homology_annotations = list(),
    locus_sets = list(),
    tree = NULL,
    tree_plot = NULL,
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
    if (length(object@homology_annotations) > 0L) {
      bad_homology <- !vapply(object@homology_annotations, methods::is, logical(1), class2 = "HomologyAnnotation")
      if (any(bad_homology)) {
        problems <- c(problems, "`homology_annotations` must be a list of HomologyAnnotation objects.")
      }
    }
    if (length(object@locus_sets) > 0L) {
      bad_locus_sets <- !vapply(object@locus_sets, methods::is, logical(1), class2 = "SynLocusSet")
      if (any(bad_locus_sets)) {
        problems <- c(problems, "`locus_sets` must be a list of SynLocusSet objects.")
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
                      x_translation = NA_real_,
                      metadata = list()) {
  new(
    "SynLayout",
    panels = panels,
    layout_type = layout_type,
    free = free,
    exon_height = exon_height,
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
#' @param format Alignment format. Currently `"paf"`, `"psl"`, or `"odgi"`.
#' @param data Optional cached parsed alignment representation. For PSL files
#'   this can be either one row per PSL record or one row per ungapped block.
#' @param metadata Optional metadata list. This may include loader state such as
#'   `psl_more`, although that value is normally managed by
#'   [load_alignment()].
#'
#' @details For PSL-backed alignments, use [load_alignment()] with
#'   `more = TRUE` when you want the cached alignment table expanded to one row
#'   per ungapped block instead of one row per PSL record.
#'
#' @return A `SynPairAlignment` object.
#' @export
SynPairAlignment <- function(name,
                             query_individual,
                             target_individual,
                             file,
                             format = c("paf", "psl", "odgi"),
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
#' @param tree Optional tree object, such as an `ape::phylo`, to reuse for
#'   tree-aligned genomic plots.
#' @param tree_plot Optional rectangular `ggtree` plot to reuse for
#'   tree-aligned genomic plots.
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
                       tree = NULL,
                       tree_plot = NULL,
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

  x <- new(
    "SynSpecies",
    name = name,
    tree = tree,
    tree_plot = tree_plot,
    metadata = metadata
  )

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
#' @rdname ggexon-show
setMethod("show", "SynSpecies", function(object) {
  cat("An object of class \"SynSpecies\"\n")
  cat("  name:", object@name, "\n")
  cat("  individuals:", length(object@individuals), "\n")
  cat("  pairwise_alignments:", length(object@pairwise_alignments), "\n")
  cat("  multiple_alignments:", length(object@multiple_alignments), "\n")
  cat("  homology_annotations:", length(object@homology_annotations), "\n")
  cat("  locus_sets:", length(object@locus_sets), "\n")
  cat("  tree:", !is.null(object@tree), "\n")
  cat("  tree_plot:", !is.null(object@tree_plot), "\n")
})

#' @export
#' @rdname ggexon-show
setMethod("show", "SynLayout", function(object) {
  cat("An object of class \"SynLayout\"\n")
  cat("  layout_type:", object@layout_type, "\n")
  cat("  panels:", nrow(object@panels), "\n")
  cat("  free x/y:", isTRUE(object@free$x), "/", isTRUE(object@free$y), "\n")
  cat("  exon_height:", object@exon_height, "\n")
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

infer_syn_layout_free <- function(panels, panel_role_y_policies = NULL) {
  panel_x_windows <- FALSE
  if (all(c("xlim_chr", "xlim_min", "xlim_max") %in% names(panels))) {
    annotation_rows <- if ("panel_type" %in% names(panels)) {
      is.na(panels$panel_type) | panels$panel_type == "annotation"
    } else {
      rep(TRUE, nrow(panels))
    }
    complete_rows <- annotation_rows &
      !is.na(panels$xlim_min) &
      !is.na(panels$xlim_max)
    if (any(complete_rows)) {
      panel_x_windows <- nrow(unique(data.frame(
        xlim_chr = as.character(panels$xlim_chr[complete_rows]),
        xlim_min = as.numeric(panels$xlim_min[complete_rows]),
        xlim_max = as.numeric(panels$xlim_max[complete_rows]),
        stringsAsFactors = FALSE
      ))) > 1L
    }
  }

  free_y <- FALSE
  if ("SCALE_Y" %in% names(panels)) {
    panel_roles <- link_panel_type(panels)
    present_roles <- unique(panel_roles[!is.na(panel_roles) & nzchar(panel_roles)])

    if (!is.null(panel_role_y_policies)) {
      policy_names <- names(panel_role_y_policies)
      policies <- if (is.null(policy_names)) {
        unlist(panel_role_y_policies, use.names = FALSE)
      } else {
        unlist(
          panel_role_y_policies[intersect(present_roles, policy_names)],
          use.names = FALSE
        )
      }
      free_y <- any(policies == "free_y")
    } else if ("coverage" %in% present_roles) {
      free_y <- any(vapply(present_roles, function(role) {
        ids <- panels$SCALE_Y[panel_roles == role]
        length(unique(stats::na.omit(ids))) > 1L
      }, logical(1)))
    } else {
      free_y <- length(unique(stats::na.omit(panels$SCALE_Y))) > 1L
    }
  }

  list(
    x = ("SCALE_X" %in% names(panels) && length(unique(stats::na.omit(panels$SCALE_X))) > 1L) || panel_x_windows,
    y = free_y
  )
}

as_syn_layout <- function(x,
                          layout_type = NULL,
                          free = NULL,
                          exon_height = NA_real_,
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
    free = free %||% infer_syn_layout_free(
      panels,
      metadata$panel_role_y_policies %||% NULL
    ),
    exon_height = exon_height,
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

.ensure_syn_layout_xlim_cols <- function(panels) {
  if (!"xlim_chr" %in% names(panels)) {
    panels$xlim_chr <- NA_character_
  }
  if (!"xlim_min" %in% names(panels)) {
    panels$xlim_min <- NA_real_
  }
  if (!"xlim_max" %in% names(panels)) {
    panels$xlim_max <- NA_real_
  }
  panels
}

.layout_panel_species_name <- function(panel_rows) {
  if (nrow(panel_rows) != 1L) {
    stop("Expected exactly one panel row.", call. = FALSE)
  }
  if ("species" %in% names(panel_rows) &&
      !is.na(panel_rows$species[[1L]]) &&
      nzchar(panel_rows$species[[1L]])) {
    return(as.character(panel_rows$species[[1L]]))
  }
  as.character(panel_rows$track[[1L]])
}

.resolve_layout_individual_panel <- function(panels, individual) {
  if (!is.character(individual) || length(individual) != 1L || is.na(individual) || !nzchar(individual)) {
    stop("`individual` must be one non-empty character value.", call. = FALSE)
  }

  annotation_rows <- if ("panel_type" %in% names(panels)) {
    is.na(panels$panel_type) | panels$panel_type == "annotation"
  } else {
    rep(TRUE, nrow(panels))
  }
  species_col <- if ("species" %in% names(panels)) {
    as.character(panels$species)
  } else {
    as.character(panels$track)
  }

  hit <- which(annotation_rows & !is.na(species_col) & species_col == individual)
  if (length(hit) != 1L) {
    stop(
      "`individual` must match exactly one annotation panel in the layout.",
      call. = FALSE
    )
  }

  hit
}

.resolve_layout_individual_panels <- function(panels, individuals = NULL) {
  annotation_rows <- if ("panel_type" %in% names(panels)) {
    is.na(panels$panel_type) | panels$panel_type == "annotation"
  } else {
    rep(TRUE, nrow(panels))
  }
  species_col <- if ("species" %in% names(panels)) {
    as.character(panels$species)
  } else {
    as.character(panels$track)
  }

  available <- unique(species_col[annotation_rows & !is.na(species_col) & nzchar(species_col)])
  if (is.null(individuals)) {
    individuals <- available
  } else {
    if (!is.character(individuals) || anyNA(individuals) || any(!nzchar(individuals))) {
      stop("`individual` must contain only non-empty character values.", call. = FALSE)
    }
    missing <- setdiff(unique(individuals), available)
    if (length(missing) > 0L) {
      stop(
        "`individual` values must each match one annotation panel in the layout. Missing: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    individuals <- unique(individuals)
  }

  hits <- vapply(individuals, function(individual) {
    .resolve_layout_individual_panel(panels, individual)
  }, integer(1))
  stats::setNames(as.integer(hits), individuals)
}

.infer_panel_xlim_chr_from_links <- function(x, species_name) {
  if (!methods::is(x, "SynSpecies")) {
    return(NULL)
  }
  if (!species_name %in% names(individuals(x))) {
    return(NULL)
  }

  individual <- individuals(x)[[species_name]]
  pair_list <- pairwise_alignments(x)
  if (length(pair_list) == 0L) {
    return(NULL)
  }

  inferred <- character()
  for (pair in pair_list) {
    if (!(identical(query_individual(pair), species_name) ||
          identical(target_individual(pair), species_name))) {
      next
    }
    paf <- tryCatch(
      .pairwise_alignment_table(pair),
      error = function(e) NULL
    )
    if (!is.data.frame(paf) || nrow(paf) == 0L) {
      next
    }

    chr_col <- if (identical(query_individual(pair), species_name)) "qchr" else "tchr"
    if (!chr_col %in% names(paf)) {
      next
    }
    available <- unique(as.character(paf[[chr_col]]))
    available <- available[!is.na(available) & nzchar(available)]
    if (length(available) == 0L) {
      next
    }

    normalized <- vapply(
      available,
      function(chr) {
        tryCatch(resolve_syn_seqname(individual, chr), error = function(e) NA_character_)
      },
      character(1)
    )
    normalized <- unique(normalized[!is.na(normalized) & nzchar(normalized)])
    if (length(normalized) == 0L) {
      next
    }
    inferred <- c(inferred, normalized)
  }

  inferred <- unique(inferred[!is.na(inferred) & nzchar(inferred)])
  if (length(inferred) == 1L) {
    inferred[[1L]]
  } else {
    NULL
  }
}

.resolve_panel_xlim_chr <- function(x, panels, hit, xlim_chr = NULL) {
  panel_rows <- panels[hit, , drop = FALSE]
  if (nrow(panel_rows) != 1L) {
    stop("Expected exactly one panel row.", call. = FALSE)
  }

  if (!is.null(xlim_chr)) {
    if (!is.character(xlim_chr) || length(xlim_chr) != 1L || is.na(xlim_chr) || !nzchar(xlim_chr)) {
      stop("`xlim_chr` must be one non-empty character value.", call. = FALSE)
    }
    if (methods::is(x, "SynSpecies")) {
      species_name <- .layout_panel_species_name(panel_rows)
      if (!species_name %in% names(individuals(x))) {
        stop(
          "Cannot resolve `xlim_chr` because panel species ", species_name,
          " is not attached to this SynSpecies object.",
          call. = FALSE
        )
      }
      individual <- individuals(x)[[species_name]]
      return(resolve_syn_seqname(individual, xlim_chr))
    }
    return(xlim_chr)
  }

  existing_chr <- panel_rows$xlim_chr[[1L]] %||% NA_character_
  if (!is.na(existing_chr) && nzchar(existing_chr)) {
    return(as.character(existing_chr))
  }

  if (!methods::is(x, "SynSpecies")) {
    stop(
      "Cannot infer panel chromosome from a SynLayout alone unless the panel already stores `xlim_chr`.",
      call. = FALSE
    )
  }

  species_name <- .layout_panel_species_name(panel_rows)
  if (!species_name %in% names(individuals(x))) {
    stop(
      "Cannot infer panel chromosome because panel species ", species_name,
      " is not attached to this SynSpecies object.",
      call. = FALSE
    )
  }

  individual <- individuals(x)[[species_name]]
  inferred_from_links <- .infer_panel_xlim_chr_from_links(x, species_name)
  if (!is.null(inferred_from_links)) {
    return(inferred_from_links)
  }

  ann <- annotation_data(individual)
  if (is.null(ann)) {
    return(NA_character_)
  }
  seqlevels <- unique(as.character(GenomeInfoDb::seqnames(ann)))
  seqlevels <- seqlevels[!is.na(seqlevels) & nzchar(seqlevels)]
  if (length(seqlevels) != 1L) {
    stop(
      "Cannot infer chromosome automatically because annotation track ", species_name,
      " spans multiple seqnames. Seed the layout once with `xlim_chr` or subset the individual first.",
      call. = FALSE
    )
  }

  seqlevels[[1L]]
}

.panel_subset_window <- function(individual, annotation = NULL) {
  if (!methods::is(individual, "SynIndividual")) {
    return(NULL)
  }

  ann_name <- annotation %||% active_feature_annotation(individual)
  if (is.null(ann_name) || is.na(ann_name) || !nzchar(ann_name)) {
    return(NULL)
  }
  if (!ann_name %in% annotation_names(individual)) {
    return(NULL)
  }

  ann <- get_annotation(individual, ann_name)
  if (!methods::is(ann, "SynFeatureAnnotation")) {
    return(NULL)
  }

  window <- annotation_metadata(ann)$subset_window %||% NULL
  if (is.null(window) || !is.list(window)) {
    return(NULL)
  }
  if (!is.character(window$chr) || length(window$chr) != 1L || is.na(window$chr) || !nzchar(window$chr)) {
    return(NULL)
  }
  if (!is.numeric(window$start) || length(window$start) != 1L || is.na(window$start)) {
    return(NULL)
  }
  if (!is.numeric(window$end) || length(window$end) != 1L || is.na(window$end)) {
    return(NULL)
  }

  list(
    chr = as.character(window$chr),
    start = as.numeric(window$start),
    end = as.numeric(window$end)
  )
}

.layout_panel_xlim_sources <- function(layout) {
  sources <- layout@metadata$panel_xlim_source %||% NULL
  if (is.null(sources)) {
    return(stats::setNames(character(), character()))
  }
  if (is.list(sources)) {
    sources <- unlist(sources, use.names = TRUE)
  }
  if (!is.character(sources)) {
    return(stats::setNames(character(), character()))
  }
  names(sources) <- names(sources) %||% rep("", length(sources))
  sources <- sources[!is.na(names(sources)) & nzchar(names(sources))]
  sources
}

.set_layout_panel_xlim_sources <- function(layout, sources) {
  layout@metadata$panel_xlim_source <- sources
  layout
}

.normalize_panel_xlim_map <- function(individual, xlim, arg = "xlim") {
  if (is.null(xlim)) {
    return(list())
  }

  if (is.numeric(xlim)) {
    if (length(individual) != 1L || length(xlim) != 2L || anyNA(xlim)) {
      stop(
        "`", arg, "` must be a numeric vector of length 2 for one individual, or a named list for multiple individuals.",
        call. = FALSE
      )
    }
    return(stats::setNames(list(as.numeric(xlim)), individual))
  }

  if (is.list(xlim)) {
    if (is.null(names(xlim)) || anyNA(names(xlim)) || any(!nzchar(names(xlim)))) {
      stop("`", arg, "` list entries must be named by individual.", call. = FALSE)
    }
    if (!all(names(xlim) %in% individual)) {
      stop("`", arg, "` contains individuals not requested in `individual`.", call. = FALSE)
    }
    return(xlim)
  }

  stop(
    "`", arg, "` must be NULL, a numeric vector of length 2, or a named list keyed by individual.",
    call. = FALSE
  )
}

.normalize_panel_xlim_chr_map <- function(individual, xlim_chr) {
  if (is.null(xlim_chr)) {
    return(list())
  }

  if (is.character(xlim_chr)) {
    if (length(individual) == 1L && length(xlim_chr) == 1L && !is.na(xlim_chr) && nzchar(xlim_chr)) {
      return(stats::setNames(list(as.character(xlim_chr)), individual))
    }
    if (length(xlim_chr) == length(individual) && !is.null(names(xlim_chr))) {
      xlim_chr <- as.list(xlim_chr)
    } else {
      stop(
        "`xlim_chr` must be one non-empty character value for one individual, or a named list/vector for multiple individuals.",
        call. = FALSE
      )
    }
  }

  if (is.list(xlim_chr)) {
    if (is.null(names(xlim_chr)) || anyNA(names(xlim_chr)) || any(!nzchar(names(xlim_chr)))) {
      stop("`xlim_chr` entries must be named by individual.", call. = FALSE)
    }
    if (!all(names(xlim_chr) %in% individual)) {
      stop("`xlim_chr` contains individuals not requested in `individual`.", call. = FALSE)
    }
    for (nm in names(xlim_chr)) {
      val <- xlim_chr[[nm]]
      if (!is.character(val) || length(val) != 1L || is.na(val) || !nzchar(val)) {
        stop("Each `xlim_chr` entry must be one non-empty character value.", call. = FALSE)
      }
    }
    return(xlim_chr)
  }

  stop(
    "`xlim_chr` must be NULL, a single character value, or a named list/vector keyed by individual.",
    call. = FALSE
  )
}

.derive_panel_xlim_from_subset <- function(x, species_name) {
  if (!methods::is(x, "SynSpecies")) {
    return(NULL)
  }
  if (!species_name %in% names(individuals(x))) {
    return(NULL)
  }
  .panel_subset_window(individuals(x)[[species_name]])
}

.layout_free_with_panel_xlim <- function(layout, panels) {
  free <- layout@free
  annotation_rows <- if ("panel_type" %in% names(panels)) {
    is.na(panels$panel_type) | panels$panel_type == "annotation"
  } else {
    rep(TRUE, nrow(panels))
  }
  complete_rows <- annotation_rows &
    !is.na(panels$xlim_min) &
    !is.na(panels$xlim_max)
  if (any(complete_rows)) {
    xlim_df <- unique(data.frame(
      xlim_chr = as.character(panels$xlim_chr[complete_rows]),
      xlim_min = as.numeric(panels$xlim_min[complete_rows]),
      xlim_max = as.numeric(panels$xlim_max[complete_rows]),
      stringsAsFactors = FALSE
    ))
    if (nrow(xlim_df) > 1L) {
      free$x <- TRUE
    }
  }
  free
}

.set_panel_xlim_on_synspecies_or_layout <- function(x,
                                                    individual = NULL,
                                                    xlim = NULL,
                                                    xlim_chr = NULL,
                                                    seed_other_panels = TRUE) {
  if (!(methods::is(x, "SynSpecies") || methods::is(x, "SynLayout"))) {
    stop("`set_panel_xlim()` expects a SynSpecies or SynLayout object.", call. = FALSE)
  }

  layout <- if (methods::is(x, "SynSpecies")) species_layout(x) else x
  if (is.null(layout)) {
    stop("The object does not contain a stored SynLayout.", call. = FALSE)
  }

  panels <- .ensure_syn_layout_xlim_cols(syn_layout_panels(layout))
  explicit_hits <- if (is.null(individual)) {
    stats::setNames(integer(), character())
  } else {
    .resolve_layout_individual_panels(panels, individual)
  }
  explicit_individuals <- names(explicit_hits)
  target_hits <- if (methods::is(x, "SynSpecies") && isTRUE(seed_other_panels)) {
    .resolve_layout_individual_panels(panels, NULL)
  } else {
    explicit_hits
  }
  target_individuals <- names(target_hits)

  if (length(explicit_hits) == 0L && (!is.null(xlim) || !is.null(xlim_chr))) {
    stop("`individual` must be supplied when `xlim` or `xlim_chr` is supplied.", call. = FALSE)
  }
  xlim_map <- .normalize_panel_xlim_map(explicit_individuals, xlim, arg = "xlim")
  xlim_chr_map <- .normalize_panel_xlim_chr_map(explicit_individuals, xlim_chr)
  sources <- .layout_panel_xlim_sources(layout)

  if (methods::is(x, "SynLayout") && length(xlim_map) == 0L) {
    stop(
      "`xlim` must be supplied for SynLayout objects because subset-window metadata is unavailable.",
      call. = FALSE
    )
  }

  for (species_name in target_individuals) {
    hit <- target_hits[[species_name]]
    explicit_xlim <- xlim_map[[species_name]] %||% NULL
    explicit_chr <- xlim_chr_map[[species_name]] %||% NULL
    current_source <- unname(sources[species_name])
    current_source <- if (length(current_source) == 0L) NA_character_ else current_source[[1L]]

    if (!is.null(explicit_xlim)) {
      if (!is.numeric(explicit_xlim) || length(explicit_xlim) != 2L || anyNA(explicit_xlim)) {
        stop("Each `xlim` entry must be a numeric vector of length 2.", call. = FALSE)
      }
      inferred_chr <- .resolve_panel_xlim_chr(x, panels, hit, xlim_chr = explicit_chr)
      panels$xlim_chr[[hit]] <- inferred_chr
      panels$xlim_min[[hit]] <- min(explicit_xlim)
      panels$xlim_max[[hit]] <- max(explicit_xlim)
      sources[species_name] <- "explicit"
      next
    }

    if (identical(current_source, "explicit") && !species_name %in% explicit_individuals) {
      next
    }

    subset_window <- .derive_panel_xlim_from_subset(x, species_name)
    if (is.null(subset_window)) {
      if (species_name %in% explicit_individuals && is.null(explicit_xlim)) {
        stop(
          "Cannot infer `xlim` automatically because the active feature annotation for ",
          species_name,
          " does not store a subset window. Supply `xlim` explicitly or subset the annotation first.",
          call. = FALSE
        )
      }
      next
    }

    inferred_chr <- .resolve_panel_xlim_chr(
      x,
      panels,
      hit,
      xlim_chr = explicit_chr %||% subset_window$chr
    )
    panels$xlim_chr[[hit]] <- inferred_chr
    panels$xlim_min[[hit]] <- subset_window$start
    panels$xlim_max[[hit]] <- subset_window$end
    if (!identical(current_source, "explicit")) {
      sources[species_name] <- "subset"
    }
  }

  updated_layout <- SynLayout(
    panels = panels,
    layout_type = layout@layout_type,
    free = .layout_free_with_panel_xlim(layout, panels),
    exon_height = layout@exon_height,
    x_translation = layout@x_translation,
    metadata = layout@metadata
  )
  updated_layout <- .set_layout_panel_xlim_sources(updated_layout, sources)

  if (methods::is(x, "SynLayout")) {
    return(updated_layout)
  }

  species_layout(x) <- updated_layout
  x
}

.set_panel_xlim_spec <- function(individual = NULL, xlim = NULL, xlim_chr = NULL) {
  structure(
    list(
      individual = individual,
      xlim = xlim,
      xlim_chr = xlim_chr
    ),
    class = "set_panel_xlim_spec"
  )
}

#' Set a panel-specific x window on a stored Syn layout
#'
#' Updates one or more annotation panels in a stored [`SynLayout`] or
#' [`SynSpecies`] layout. When called on a `SynSpecies` with `individual = NULL`
#' and `xlim = NULL`, ggexon searches each annotation panel's active feature
#' layer for subset-window metadata recorded by [`subset_feature_annotation()`]
#' and seeds panel limits from those windows.
#'
#' @param x A [`SynSpecies`], [`SynLayout`], or ggexon plot object.
#' @param individual Optional annotation-panel individual name or names from the
#'   layout table. Defaults to all annotation panels.
#' @param xlim Optional panel limits. Supply a numeric length-2 vector for one
#'   individual, or a named list of length-2 numeric vectors keyed by
#'   individual. When omitted for a `SynSpecies` or ggexon plot backed by a
#'   `SynSpecies`, ggexon reuses coordinates previously stored by
#'   [`subset_feature_annotation()`].
#' @param xlim_chr Optional chromosome / seqname for the panel window. Supply
#'   one character value for one individual, or a named list/vector keyed by
#'   individual.
#'
#' @return An updated object of the same class as `x`.
#' @export
set_panel_xlim <- function(x = NULL, individual = NULL, xlim = NULL, xlim_chr = NULL) {
  if (is.null(x)) {
    return(.set_panel_xlim_spec(
      individual = individual,
      xlim = xlim,
      xlim_chr = xlim_chr
    ))
  }

  if (inherits(x, "ggexon")) {
    if (!methods::is(x@data, "SynSpecies")) {
      stop(
        "`set_panel_xlim()` can only modify ggexon plots backed by a SynSpecies object.",
        call. = FALSE
      )
    }
    x@data <- .set_panel_xlim_on_synspecies_or_layout(
      x@data,
      individual = individual,
      xlim = xlim,
      xlim_chr = xlim_chr,
      seed_other_panels = is.null(individual)
    )
    return(x)
  }

  .set_panel_xlim_on_synspecies_or_layout(
    x,
    individual = individual,
    xlim = xlim,
    xlim_chr = xlim_chr
  )
}

#' @export
ggplot_add.set_panel_xlim_spec <- function(object, plot, ...) {
  if (!is_ggexon(plot)) {
    cli::cli_abort(
      "{.fn set_panel_xlim} additions currently support {.cls ggexon} plots only."
    )
  }

  set_panel_xlim(
    plot,
    individual = object$individual,
    xlim = object$xlim,
    xlim_chr = object$xlim_chr
  )
}

#' Clear a panel-specific x window from a stored Syn layout
#'
#' @param x A [`SynSpecies`] or [`SynLayout`] object.
#' @param individual Annotation-panel individual name from the layout table.
#'
#' @return An updated object of the same class as `x`.
#' @export
clear_panel_xlim <- function(x, individual) {
  if (!(methods::is(x, "SynSpecies") || methods::is(x, "SynLayout"))) {
    stop("`clear_panel_xlim()` expects a SynSpecies or SynLayout object.", call. = FALSE)
  }

  layout <- if (methods::is(x, "SynSpecies")) species_layout(x) else x
  if (is.null(layout)) {
    stop("The object does not contain a stored SynLayout.", call. = FALSE)
  }

  panels <- .ensure_syn_layout_xlim_cols(syn_layout_panels(layout))
  hit <- .resolve_layout_individual_panel(panels, individual)

  panels$xlim_chr[[hit]] <- NA_character_
  panels$xlim_min[[hit]] <- NA_real_
  panels$xlim_max[[hit]] <- NA_real_

  updated_layout <- SynLayout(
    panels = panels,
    layout_type = layout@layout_type,
    free = .layout_free_with_panel_xlim(layout, panels),
    exon_height = layout@exon_height,
    x_translation = layout@x_translation,
    metadata = layout@metadata
  )
  updated_layout <- .set_layout_panel_xlim_sources(
    updated_layout,
    sources = .layout_panel_xlim_sources(updated_layout)[setdiff(
      names(.layout_panel_xlim_sources(updated_layout)),
      individual
    )]
  )

  if (methods::is(x, "SynLayout")) {
    return(updated_layout)
  }

  species_layout(x) <- updated_layout
  x
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
#' @rdname individual_names
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

#' List locus sets attached to a SynSpecies
#'
#' @param x A `SynSpecies` object.
#'
#' @return A named list of `SynLocusSet` objects.
#' @export
setGeneric("locus_sets", function(x) standardGeneric("locus_sets"))
#' @rdname locus_sets
setMethod("locus_sets", "SynSpecies", function(x) x@locus_sets)

#' Access tree objects stored on a `SynSpecies`
#'
#' `species_tree()` returns the stored raw tree object, such as an `ape::phylo`.
#' `species_tree_plot()` returns the stored rectangular `ggtree` plot.
#'
#' @param x A `SynSpecies` object.
#'
#' @return The stored tree or tree plot object, or `NULL`.
#' @export
setGeneric("species_tree", function(x) standardGeneric("species_tree"))
#' @rdname species_tree
setMethod("species_tree", "SynSpecies", function(x) x@tree)

#' @rdname species_tree
#' @export
setGeneric("species_tree_plot", function(x) standardGeneric("species_tree_plot"))
#' @rdname species_tree
setMethod("species_tree_plot", "SynSpecies", function(x) x@tree_plot)

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

#' Retrieve pairwise alignment rows
#'
#' Returns the pairwise alignment table for a [`SynPairAlignment`] object or a
#' stored pairwise alignment inside a [`SynSpecies`] object.
#'
#' @param x A [`SynPairAlignment`] or [`SynSpecies`] object.
#' @param alignment Optional alignment name when `x` is a [`SynSpecies`].
#' @param ... Passed through to the internal alignment-data resolver, including
#'   options such as `subset` or `filter`.
#' @param odgi Optional path to the `odgi` executable when ODGI-backed
#'   alignments need to be loaded.
#' @param python Optional path to the Python interpreter when ODGI-backed
#'   alignments need helper script execution.
#'
#' @return A `data.frame` containing pairwise alignment rows.
#' @export
setGeneric("pairwise_alignment_data", function(x, ...) standardGeneric("pairwise_alignment_data"))
#' @rdname pairwise_alignment_data
setMethod("pairwise_alignment_data", "SynPairAlignment", function(x, alignment = NULL, ..., odgi = NULL, python = NULL) {
  .pairwise_alignment_data_impl(
    x = x,
    species_obj = NULL,
    odgi = odgi,
    python = python,
    ...
  )
})
#' @rdname pairwise_alignment_data
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

#' Add SynIndividual objects to a SynSpecies object
#'
#' @param x A `SynSpecies` object.
#' @param individual A `SynIndividual` object.
#' @param ... Additional `SynIndividual` objects to add.
#'
#' @return An updated `SynSpecies` object.
#' @export
setGeneric("add_individual", function(x, individual, ...) {
  standardGeneric("add_individual")
}, signature = c("x", "individual"))

#' @rdname add_individual
setMethod("add_individual", c("SynSpecies", "SynIndividual"), function(x, individual, ...) {
  new_individuals <- c(list(individual), list(...))
  bad_individuals <- !vapply(
    new_individuals,
    methods::is,
    logical(1),
    class2 = "SynIndividual"
  )
  if (any(bad_individuals)) {
    stop("All inputs after `x` must be SynIndividual objects.", call. = FALSE)
  }

  entries <- x@individuals
  for (individual in new_individuals) {
    entries[[syn_id(individual)]] <- individual
  }
  x@individuals <- entries
  validObject(x)
  x
})

#' @rdname add_individual
setMethod("add_individual", c("SynSpecies", "ANY"), function(x, individual, ...) {
  stop("All inputs after `x` must be SynIndividual objects.", call. = FALSE)
})

#' @rdname add_individual
setMethod("add_individual", c("ANY", "SynIndividual"), function(x, individual, ...) {
  stop("`add_individual()` expects a SynSpecies object.", call. = FALSE)
})

#' @rdname add_individual
setMethod("add_individual", c("ANY", "ANY"), function(x, individual, ...) {
  stop("`add_individual()` expects a SynSpecies object.", call. = FALSE)
})

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
setGeneric("add_individuals_from_folder", function(x,
                                                   folder,
                                                   annotation_format = c("auto", "gff", "gtf"),
                                                   recursive = FALSE) {
  standardGeneric("add_individuals_from_folder")
}, signature = "x")

#' @rdname add_individuals_from_folder
setMethod("add_individuals_from_folder", "SynSpecies", function(x,
                                                               folder,
                                                               annotation_format = c("auto", "gff", "gtf"),
                                                               recursive = FALSE) {
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
})

#' @rdname add_individuals_from_folder
setMethod("add_individuals_from_folder", "ANY", function(x,
                                                         folder,
                                                         annotation_format = c("auto", "gff", "gtf"),
                                                         recursive = FALSE) {
  stop("`add_individuals_from_folder()` expects a SynSpecies object.", call. = FALSE)
})

#' Add a pairwise alignment to a SynSpecies object
#'
#' @param x A `SynSpecies` object.
#' @param alignment A `SynPairAlignment` object.
#'
#' @return An updated `SynSpecies` object.
#' @export
setGeneric("add_pairwise_alignment", function(x, alignment) {
  standardGeneric("add_pairwise_alignment")
}, signature = c("x", "alignment"))

#' @rdname add_pairwise_alignment
setMethod("add_pairwise_alignment", c("SynSpecies", "SynPairAlignment"), function(x, alignment) {

  missing_species <- setdiff(alignment_individuals(alignment), names(individuals(x)))
  if (length(missing_species) > 0L) {
    cli::cli_warn(c(
      "Pairwise alignment {.val {alignment_name(alignment)}} references individuals not attached to this {.cls SynSpecies}.",
      "i" = "Missing individuals: {.val {missing_species}}.",
      "i" = "ggexon will keep blank annotation panels for them when possible."
    ))
  }

  entries <- x@pairwise_alignments
  entries[[alignment_name(alignment)]] <- alignment
  x@pairwise_alignments <- entries
  validObject(x)
  x
})

#' @rdname add_pairwise_alignment
setMethod("add_pairwise_alignment", c("SynSpecies", "ANY"), function(x, alignment) {
  stop("`alignment` must be a SynPairAlignment object.", call. = FALSE)
})

#' @rdname add_pairwise_alignment
setMethod("add_pairwise_alignment", c("ANY", "SynPairAlignment"), function(x, alignment) {
  stop("`add_pairwise_alignment()` expects a SynSpecies object.", call. = FALSE)
})

#' @rdname add_pairwise_alignment
setMethod("add_pairwise_alignment", c("ANY", "ANY"), function(x, alignment) {
  stop("`add_pairwise_alignment()` expects a SynSpecies object.", call. = FALSE)
})

#' Add a multiple alignment to a SynSpecies object
#'
#' @param x A `SynSpecies` object.
#' @param alignment A `SynMultiAlignment` object.
#'
#' @return An updated `SynSpecies` object.
#' @export
setGeneric("add_multiple_alignment", function(x, alignment) {
  standardGeneric("add_multiple_alignment")
}, signature = c("x", "alignment"))

#' @rdname add_multiple_alignment
setMethod("add_multiple_alignment", c("SynSpecies", "SynMultiAlignment"), function(x, alignment) {
  entries <- x@multiple_alignments
  entries[[alignment_name(alignment)]] <- alignment
  x@multiple_alignments <- entries
  validObject(x)
  x
})

#' @rdname add_multiple_alignment
setMethod("add_multiple_alignment", c("SynSpecies", "ANY"), function(x, alignment) {
  stop("`alignment` must be a SynMultiAlignment object.", call. = FALSE)
})

#' @rdname add_multiple_alignment
setMethod("add_multiple_alignment", c("ANY", "SynMultiAlignment"), function(x, alignment) {
  stop("`add_multiple_alignment()` expects a SynSpecies object.", call. = FALSE)
})

#' @rdname add_multiple_alignment
setMethod("add_multiple_alignment", c("ANY", "ANY"), function(x, alignment) {
  stop("`add_multiple_alignment()` expects a SynSpecies object.", call. = FALSE)
})

#' Add a locus set to a SynSpecies object
#'
#' @param x A `SynSpecies` object.
#' @param locus_set A `SynLocusSet` object.
#'
#' @return An updated `SynSpecies` object.
#' @export
setGeneric("add_locus_set", function(x, locus_set) {
  standardGeneric("add_locus_set")
}, signature = c("x", "locus_set"))

#' @rdname add_locus_set
setMethod("add_locus_set", c("SynSpecies", "SynLocusSet"), function(x, locus_set) {
  x@locus_sets[[annotation_name(locus_set)]] <- locus_set
  validObject(x)
  x
})

#' @rdname add_locus_set
setMethod("add_locus_set", c("SynSpecies", "ANY"), function(x, locus_set) {
  stop("`locus_set` must be a SynLocusSet object.", call. = FALSE)
})

#' @rdname add_locus_set
setMethod("add_locus_set", c("ANY", "SynLocusSet"), function(x, locus_set) {
  stop("`add_locus_set()` expects a SynSpecies object.", call. = FALSE)
})

#' @rdname add_locus_set
setMethod("add_locus_set", c("ANY", "ANY"), function(x, locus_set) {
  stop("`add_locus_set()` expects a SynSpecies object.", call. = FALSE)
})

#' Retrieve a locus set from a SynSpecies
#'
#' @param x A `SynSpecies` object.
#' @param name Optional locus-set name. If omitted and exactly one locus set is
#'   attached, that set is returned.
#'
#' @return A `SynLocusSet` object, or `NULL` when `name` is supplied and absent.
#' @export
get_locus_set <- function(x, name = NULL) {
  if (!methods::is(x, "SynSpecies")) {
    stop("`get_locus_set()` expects a SynSpecies object.", call. = FALSE)
  }
  sets <- locus_sets(x)
  if (!is.null(name)) {
    if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
      stop("`name` must be a single non-empty character value.", call. = FALSE)
    }
    return(sets[[name]])
  }
  if (length(sets) == 1L) {
    return(sets[[1L]])
  }
  if (length(sets) == 0L) {
    return(NULL)
  }
  stop("Provide `name` when multiple SynLocusSet objects are attached.", call. = FALSE)
}

.validate_synspecies_tree_file <- function(tree_file) {
  if (!is.character(tree_file) || length(tree_file) != 1L || is.na(tree_file) || !nzchar(tree_file)) {
    stop("`tree_file` must be a single non-empty string.", call. = FALSE)
  }
  if (!file.exists(tree_file)) {
    stop("`tree_file` does not exist: ", tree_file, call. = FALSE)
  }
  normalizePath(tree_file, mustWork = TRUE)
}

.read_synspecies_tree_file <- function(tree_file) {
  tree_file <- .validate_synspecies_tree_file(tree_file)
  if (!requireNamespace("ape", quietly = TRUE)) {
    stop("Package `ape` is required to read `tree_file`.", call. = FALSE)
  }

  ext <- to_lower_ascii(tools::file_ext(tree_file))
  tree <- if (ext %in% c("nex", "nexus")) {
    ape::read.nexus(tree_file)
  } else {
    ape::read.tree(file = tree_file)
  }

  if (inherits(tree, "multiPhylo")) {
    stop("`tree_file` must contain a single tree, not a multi-tree object.", call. = FALSE)
  }
  tree
}

.is_synspecies_tree_object <- function(tree) {
  if (is.null(tree)) {
    return(FALSE)
  }
  if (inherits(tree, c("phylo", "tbl_tree", "treedata"))) {
    return(TRUE)
  }
  if (isS4(tree) && any(methods::is(tree) %in% c("phylo", "tbl_tree", "treedata"))) {
    return(TRUE)
  }
  FALSE
}

.validate_synspecies_tree_plot <- function(tree_plot) {
  if (!inherits(tree_plot, "ggtree")) {
    stop("`tree_plot` must be a ggtree object.", call. = FALSE)
  }
  .ggtree_rectangular_plot_data(tree_plot = tree_plot)
  invisible(tree_plot)
}

#' Add a tree or tree plot to a `SynSpecies` object
#'
#' `add_tree()` stores one tree representation on a `SynSpecies`. The input can
#' be a single tree file path, a tree object from `ape`, `tidytree`, or `treeio`,
#' or a rectangular `ggtree` plot. If a new tree object is stored, any previous
#' stored tree plot is cleared; if a new tree plot is stored, any previous raw
#' tree object is cleared.
#'
#' @param x A `SynSpecies` object.
#' @param tree Optional tree object. Supported inputs include `ape::phylo`,
#'   `tidytree::tbl_tree`, `treeio::treedata`, or a `ggtree` plot. A single
#'   character value is treated as `tree_file`.
#' @param tree_file Optional single tree-file path. Newick files are read with
#'   `ape::read.tree()` and Nexus files with `ape::read.nexus()`.
#' @param tree_plot Optional rectangular `ggtree` plot.
#' @param ... Reserved for future tree reader options.
#'
#' @return The updated `SynSpecies` object.
#' @export
setGeneric("add_tree", function(x, tree = NULL, tree_file = NULL, tree_plot = NULL, ...) {
  standardGeneric("add_tree")
})

#' @rdname add_tree
setMethod("add_tree", "SynSpecies", function(x, tree = NULL, tree_file = NULL, tree_plot = NULL, ...) {
  dots <- list(...)
  if (length(dots) > 0L) {
    stop("Unused arguments: ", paste(names(dots), collapse = ", "), call. = FALSE)
  }

  tree_was_supplied <- !is.null(tree)
  inputs_supplied <- sum(tree_was_supplied, !is.null(tree_file), !is.null(tree_plot))
  if (inputs_supplied != 1L) {
    stop("Supply exactly one of `tree`, `tree_file`, or `tree_plot`.", call. = FALSE)
  }

  if (!is.null(tree_file)) {
    x@tree <- .read_synspecies_tree_file(tree_file)
    x@tree_plot <- NULL
    x@metadata$tree_file <- .validate_synspecies_tree_file(tree_file)
    validObject(x)
    return(x)
  }

  if (!is.null(tree_plot)) {
    .validate_synspecies_tree_plot(tree_plot)
    x@tree <- NULL
    x@tree_plot <- tree_plot
    x@metadata$tree_file <- NULL
    validObject(x)
    return(x)
  }

  if (is.character(tree)) {
    x@tree <- .read_synspecies_tree_file(tree)
    x@tree_plot <- NULL
    x@metadata$tree_file <- .validate_synspecies_tree_file(tree)
    validObject(x)
    return(x)
  }

  if (inherits(tree, "ggtree")) {
    .validate_synspecies_tree_plot(tree)
    x@tree <- NULL
    x@tree_plot <- tree
    x@metadata$tree_file <- NULL
    validObject(x)
    return(x)
  }

  if (.is_synspecies_tree_object(tree)) {
    x@tree <- tree
    x@tree_plot <- NULL
    x@metadata$tree_file <- NULL
    validObject(x)
    return(x)
  }

  stop(
    "`tree` must be a tree file path, an ape/tidytree/treeio tree object, or a ggtree plot.",
    call. = FALSE
  )
})

#' @rdname add_tree
setMethod("add_tree", "ANY", function(x, tree = NULL, tree_file = NULL, tree_plot = NULL, ...) {
  stop("`add_tree()` expects a SynSpecies object.", call. = FALSE)
})

#' Store a ggexon panel layout on a `SynSpecies` object
#'
#' @param x A `SynSpecies` object.
#' @param value A `SynLayout`, layout `data.frame`, or `NULL`.
#'
#' @return The updated `SynSpecies` object.
#' @export
setGeneric("species_layout<-", function(x, value) standardGeneric("species_layout<-"))
#' @rdname species_layout-set
setReplaceMethod("species_layout", "SynSpecies", function(x, value) {
  x@layout <- as_syn_layout(value)
  validObject(x)
  x
})

#' Store tree objects on a `SynSpecies`
#'
#' @param x A `SynSpecies` object.
#' @param value A tree object, a rectangular `ggtree` plot, or `NULL`.
#'
#' @return The updated `SynSpecies` object.
#' @export
setGeneric("species_tree<-", function(x, value) standardGeneric("species_tree<-"))
#' @rdname species_tree-set
setReplaceMethod("species_tree", "SynSpecies", function(x, value) {
  x@tree <- value
  validObject(x)
  x
})

#' @rdname species_tree-set
#' @export
setGeneric("species_tree_plot<-", function(x, value) standardGeneric("species_tree_plot<-"))
#' @rdname species_tree-set
setReplaceMethod("species_tree_plot", "SynSpecies", function(x, value) {
  x@tree_plot <- value
  validObject(x)
  x
})

#' @rdname load_annotation
setMethod("load_annotation", "SynSpecies", function(x, annotation = NULL, individual = NULL) {
  if (!is.null(individual)) {
    individual_obj <- resolve_syn_individual(x, species = individual)
    updated <- load_annotation(individual_obj, annotation = annotation)
    x@individuals[[syn_id(updated)]] <- updated
    validObject(x)
    return(x)
  }

  inds <- individuals(x)
  if (length(inds) == 0L) {
    return(x)
  }

  inds <- lapply(inds, load_annotation, annotation = annotation)
  x@individuals <- inds
  validObject(x)
  x
})

#' @rdname subset_feature_annotation
setMethod("subset_feature_annotation", "SynSpecies", function(x,
                                                              annotation = NULL,
                                                              individual = NULL,
                                                              chr = NULL,
                                                              start = NULL,
                                                              end = NULL,
                                                              coords = NULL,
                                                              gene = NULL,
                                                              transcript = NULL) {
  .subset_feature_annotation_impl(x, annotation, individual, chr, start, end, coords, gene, transcript)
})

#' @rdname subset_individual
setMethod("subset_individual", "SynSpecies", function(x,
                                                      individual = NULL,
                                                      chr = NULL,
                                                      start = NULL,
                                                      end = NULL,
                                                      coords = NULL,
                                                      annotations = c("all_feature", "active")) {
  .subset_individual_impl(x, individual, chr, start, end, coords, annotations)
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
#' @details
#' This is an S4 generic that dispatches on the class of `x`.
#'
#' @return A `SynSpecies` object.
#'
#' @examples
#' ann_path <- system.file(
#'   "extdata",
#'   "caenorhabditis_XZ1516.gff3",
#'   package = "ggexon"
#' )
#' ind <- SynIndividual(
#'   annotation_file = ann_path,
#'   genome_file = genome_waiver(),
#'   id = "XZ1516"
#' ) |>
#'   load_annotation()
#' gr <- annotation_data(ind)
#' coords <- paste0(
#'   "XZ1516#",
#'   as.character(GenomeInfoDb::seqnames(gr))[[1L]],
#'   ":",
#'   IRanges::start(gr)[[1L]],
#'   "-",
#'   IRanges::end(gr)[[1L]]
#' )
#'
#' sp <- SynSpecies(name = "worms") |> add_individual(ind)
#' sp_window <- subset_species(sp, coords = coords)
#'
#' @export
setGeneric("subset_species", function(x,
                                      coords,
                                      annotations = c("all_feature", "active")) {
  standardGeneric("subset_species")
})

#' @rdname subset_species
setMethod("subset_species", "SynSpecies", function(x,
                                                   coords,
                                                   annotations = c("all_feature", "active")) {
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
})

#' Subset a pairwise alignment by query/target regions
#'
#' @param x A `SynSpecies` or `SynPairAlignment` object.
#' @param subset Named character vector/list with one region per species for
#'   one or both alignment partners, e.g.
#'   `c(XZ1516 = "RagTag_V:21550000-21680000", N2 = "V:20450000-20451000")`,
#'   `c(XZ1516 = "RagTag_V:21550000-21680000")`, or
#'   `c(XZ1516 = "RagTag_V")`.
#' @param alignment Optional alignment name when `x` is a `SynSpecies`.
#'
#' @details
#' This is an S4 generic that dispatches on the class of `x`.
#'
#' @return An updated `SynPairAlignment` or `SynSpecies` object.
#'
#' @examples
#' paf_path <- system.file("extdata", "V_alginment.paf", package = "ggexon")
#' pair <- SynPairAlignment(
#'   name = "XZ1516_vs_N2",
#'   query_individual = "XZ1516",
#'   target_individual = "N2",
#'   file = paf_path
#' )
#' pair <- subset_pairwise_alignment(pair, subset = c(XZ1516 = "RagTag_V"))
#'
#' @export
setGeneric("subset_pairwise_alignment", function(x, subset, alignment = NULL) {
  standardGeneric("subset_pairwise_alignment")
})

#' @rdname subset_pairwise_alignment
setMethod("subset_pairwise_alignment", "SynPairAlignment", function(x, subset, alignment = NULL) {
  x@data <- pairwise_alignment_data(x, subset = subset)
  x@loaded <- TRUE
  x@lazy <- FALSE
  x
})

#' @rdname subset_pairwise_alignment
setMethod("subset_pairwise_alignment", "SynSpecies", function(x, subset, alignment = NULL) {
  pair <- .resolve_pairwise_alignment_arg(x = x, alignment = alignment)
  updated <- subset_pairwise_alignment(pair, subset = subset)
  x@pairwise_alignments[[alignment_name(updated)]] <- updated
  validObject(x)
  x
})

#' Filter a pairwise alignment by minimum PAF alignment length
#'
#' @param x A `SynSpecies` or `SynPairAlignment` object.
#' @param filter Minimum `alen` to keep.
#' @param alignment Optional alignment name when `x` is a `SynSpecies`.
#'
#' @details
#' This is an S4 generic that dispatches on the class of `x`.
#'
#' @return An updated `SynPairAlignment` or `SynSpecies` object.
#'
#' @examples
#' paf_path <- system.file("extdata", "V_alginment.paf", package = "ggexon")
#' pair <- SynPairAlignment(
#'   name = "XZ1516_vs_N2",
#'   query_individual = "XZ1516",
#'   target_individual = "N2",
#'   file = paf_path
#' )
#' pair <- filter_pairwise_alignment(pair, filter = 200)
#'
#' @export
setGeneric("filter_pairwise_alignment", function(x, filter = 200, alignment = NULL) {
  standardGeneric("filter_pairwise_alignment")
})

#' @rdname filter_pairwise_alignment
setMethod("filter_pairwise_alignment", "SynPairAlignment", function(x, filter = 200, alignment = NULL) {
  x@data <- pairwise_alignment_data(x, filter = filter)
  x@loaded <- TRUE
  x@lazy <- FALSE
  x
})

#' @rdname filter_pairwise_alignment
setMethod("filter_pairwise_alignment", "SynSpecies", function(x, filter = 200, alignment = NULL) {
  pair <- .resolve_pairwise_alignment_arg(x = x, alignment = alignment)
  updated <- filter_pairwise_alignment(pair, filter = filter)
  x@pairwise_alignments[[alignment_name(updated)]] <- updated
  validObject(x)
  x
})

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
#' @param selected_species Optional character vector giving the plotted species
#'   order to retain when `alignment` points to an ODGI multiple alignment.
#'   When `reference_species` is supplied for an ODGI multiple alignment,
#'   ggexon reorders this set greedily from the reference by choosing the next
#'   species with the largest shared-node count against the most recently chosen
#'   species. Adjacent species in the resulting order are linked pairwise.
#' @param filter_by_len Optional ODGI node-length filter such as `"> 10"` or
#'   `"<= 3"`. Applied only when `alignment` resolves to an ODGI multiple
#'   alignment.
#' @param max_target_gap Optional maximum gap used when chaining nearby PAF hits
#'   on the partner genome. Defaults to `max(50000, 2 * window_width)`.
#'
#' @return A list with `windows`, `annotations`, and `links`.
#' @export
setGeneric("subset_synspecies_window", function(x,
                                                reference_species,
                                                chr,
                                                start,
                                                end,
                                                alignment = NULL,
                                                selected_species = NULL,
                                                filter_by_len = NULL,
                                                max_target_gap = NULL) {
  standardGeneric("subset_synspecies_window")
})

#' @rdname subset_synspecies_window
setMethod("subset_synspecies_window", "SynSpecies", function(x,
                                                             reference_species,
                                                             chr,
                                                             start,
                                                             end,
                                                             alignment = NULL,
                                                             selected_species = NULL,
                                                             filter_by_len = NULL,
                                                             max_target_gap = NULL) {
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
  alignment_obj <- .resolve_subset_alignment_arg(
    x = x,
    reference_species = reference_species,
    alignment = alignment,
    selected_species = selected_species
  )

  if (methods::is(alignment_obj, "SynMultiAlignment")) {
    return(
      .subset_odgi_synspecies_window(
        x = x,
        multi = alignment_obj,
        reference_species = reference_species,
        chr = chr,
        start = ref_start,
        end = ref_end,
        selected_species = selected_species,
        filter_by_len = filter_by_len
      )
    )
  }

  pair <- alignment_obj

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
  paf <- .pairwise_alignment_table(pair)

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
      "No pairwise alignment records overlap ", reference_species, ":", ref_chr, ":",
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
    stop("Selected pairwise alignment cluster maps to multiple partner chromosomes.", call. = FALSE)
  }
  partner_chr <- if (methods::is(partner_individual, "SynIndividual")) {
    resolve_syn_seqname(partner_individual, partner_chr[[1L]])
  } else {
    partner_chr[[1L]]
  }
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
})

.subset_odgi_synspecies_window <- function(x,
                                          multi,
                                          reference_species,
                                          chr,
                                          start,
                                          end,
                                          selected_species = NULL,
                                          filter_by_len = NULL) {
  odgi_subset <- .odgi_alignment_windows_from_reference(
    msa = multi,
    reference_species = reference_species,
    chr = chr,
    start = start,
    end = end,
    selected_species = selected_species,
    filter_by_len = filter_by_len
  )

  species_order <- odgi_subset$species_order
  if (length(species_order) < 2L) {
    stop(
      "The selected ODGI alignment window does not overlap at least two plotted species.",
      call. = FALSE
    )
  }

  windows <- odgi_subset$windows
  annotations <- lapply(names(windows), function(species_name) {
    individual <- individuals(x)[[species_name]]
    window <- windows[[species_name]]
    .subset_annotation_window(
      individual,
      chr = window$chr[[1L]],
      start = window$start[[1L]],
      end = window$end[[1L]]
    )
  })
  names(annotations) <- names(windows)

  pair_list <- .odgi_pairwise_alignments_from_multi(
    msa = multi,
    species_order = species_order,
    reference_species = reference_species,
    filter_by_len = filter_by_len
  )
  links <- lapply(pair_list, function(pair) {
    pair_species <- alignment_individuals(pair)
    subset_regions <- vapply(pair_species, function(species_name) {
      window_to_region_string(windows[[species_name]])
    }, character(1))
    pairwise_alignment_data(pair, subset = subset_regions)
  })
  links <- dplyr::bind_rows(links)
  rownames(links) <- NULL

  list(
    windows = windows,
    annotations = annotations,
    links = links
  )
}

.resolve_subset_alignment_arg <- function(x,
                                          reference_species,
                                          alignment = NULL,
                                          selected_species = NULL) {
  pair_list <- pairwise_alignments(x)
  if (!is.null(alignment) && alignment %in% names(pair_list)) {
    return(pair_list[[alignment]])
  }

  if (is.null(alignment) && length(pair_list) == 1L) {
    return(pair_list[[1L]])
  }

  multi_list <- multiple_alignments(x)
  if (!is.null(alignment) && alignment %in% names(multi_list)) {
    multi <- multi_list[[alignment]]
  } else if (is.null(alignment) && length(pair_list) == 0L && length(multi_list) == 1L) {
    multi <- multi_list[[1L]]
  } else {
    multi <- NULL
  }

  if (!is.null(multi)) {
    if (!identical(alignment_format(multi), "odgi")) {
      stop(
        "Only ODGI multiple alignments currently support reference-led comparative window dispatch.",
        call. = FALSE
      )
    }
    selected_species <- unique(as.character(selected_species %||% character()))
    selected_species <- selected_species[selected_species %in% alignment_individuals(multi)]
    if (length(selected_species) == 0L) {
      selected_species <- alignment_individuals(multi)
    }
    if (!reference_species %in% selected_species) {
      selected_species <- c(reference_species, selected_species)
    }
    if (length(selected_species) < 2L) {
      stop(
        "Need at least two selected species to derive ODGI comparative windows.",
        call. = FALSE
      )
    }
    return(multi)
  }

  if (length(pair_list) == 0L) {
    stop(
      "The SynSpecies object does not contain any pairwise alignments or ODGI multiple alignments.",
      call. = FALSE
    )
  }

  if (is.null(alignment)) {
    synspecies_chain_species_order(x)
    stop(
      "For SynSpecies chains with multiple pairwise alignments, supply `alignment` to choose the pair for subsetting.",
      call. = FALSE
    )
  }

  stop(
    "Unknown alignment: ", alignment,
    ". Available pairwise alignments: ", paste(names(pair_list), collapse = ", "),
    call. = FALSE
  )
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

.paf_cigar_from_optional <- function(fields) {
  if (length(fields) <= 12L) {
    return(rep(NA_character_, nrow(fields)))
  }

  optional <- fields[, seq.int(13L, ncol(fields)), drop = FALSE]
  apply(optional, 1L, function(x) {
    hit <- grep("^cg:Z:", x)
    if (length(hit) == 0L) {
      return(NA_character_)
    }
    sub("^cg:Z:", "", x[[hit[[1L]]]])
  })
}

.parse_paf_cigar <- function(cigar) {
  if (length(cigar) != 1L || is.na(cigar) || !nzchar(cigar)) {
    return(data.frame(len = integer(), op = character(), stringsAsFactors = FALSE))
  }

  pieces <- regmatches(cigar, gregexpr("[0-9]+[A-Z=]", cigar, perl = TRUE))[[1L]]
  if (length(pieces) == 0L || identical(pieces, character(0))) {
    return(data.frame(len = integer(), op = character(), stringsAsFactors = FALSE))
  }

  data.frame(
    len = as.integer(sub("[A-Z=]$", "", pieces, perl = TRUE)),
    op = sub("^[0-9]+", "", pieces, perl = TRUE),
    stringsAsFactors = FALSE
  )
}

.paf_query_interval <- function(cursor, width, strand = "+") {
  cursor <- as.integer(cursor)
  width <- as.integer(width)

  if (anyNA(c(cursor, width))) {
    return(c(NA_integer_, NA_integer_))
  }

  if (identical(strand, "-")) {
    return(c(cursor - width, cursor))
  }

  c(cursor, cursor + width)
}

.paf_query_consumes <- function(op) {
  op %in% c("M", "=", "X", "I", "S")
}

.paf_target_consumes <- function(op) {
  op %in% c("M", "=", "X", "D", "N")
}

.empty_pairwise_alignment_detail_table <- function() {
  out <- .empty_pairwise_alignment_table()
  out$paf_row <- integer()
  out$block_index <- integer()
  out$block_size <- integer()
  out$cigar_op <- character()
  out$qstrand <- character()
  out$tstrand <- character()
  out$qstart_raw <- integer()
  out$qend_raw <- integer()
  out$tstart_raw <- integer()
  out$tend_raw <- integer()
  rownames(out) <- NULL
  out
}

.expand_paf_matches <- function(paf) {
  if (nrow(paf) == 0L) {
    return(.empty_pairwise_alignment_detail_table())
  }

  block_rows <- vector("list", nrow(paf))

  for (i in seq_len(nrow(paf))) {
    ops <- .parse_paf_cigar(paf$cigar[[i]])
    if (nrow(ops) == 0L) {
      next
    }

    qstrand <- if (isTRUE(paf$strand[[i]] == "-")) "-" else "+"
    tstrand <- "+"
    qcursor <- if (identical(qstrand, "-")) as.integer(paf$qend[[i]]) else as.integer(paf$qstart[[i]])
    tcursor <- as.integer(paf$tstart[[i]])
    row_index <- 0L
    rows <- vector("list", nrow(ops))

    for (j in seq_len(nrow(ops))) {
      op <- ops$op[[j]]
      len <- as.integer(ops$len[[j]])

      if (is.na(len) || len < 0L) {
        next
      }

      if (op %in% c("M", "=")) {
        row_index <- row_index + 1L
        q_interval <- .paf_query_interval(qcursor, len, strand = qstrand)
        t_interval <- c(tcursor, tcursor + len)

        rows[[row_index]] <- data.frame(
          qchr = as.character(paf$qchr[[i]]),
          qlen = as.integer(paf$qlen[[i]]),
          qstart = as.integer(q_interval[[1L]]),
          qend = as.integer(q_interval[[2L]]),
          strand = as.character(paf$strand[[i]]),
          tchr = as.character(paf$tchr[[i]]),
          tlen = as.integer(paf$tlen[[i]]),
          tstart = as.integer(t_interval[[1L]]),
          tend = as.integer(t_interval[[2L]]),
          nmatch = len,
          alen = len,
          mapq = as.integer(paf$mapq[[i]]),
          paf_row = as.integer(i),
          block_index = as.integer(row_index),
          block_size = len,
          cigar_op = op,
          qstrand = qstrand,
          tstrand = tstrand,
          qstart_raw = as.integer(q_interval[[1L]]),
          qend_raw = as.integer(q_interval[[2L]]),
          tstart_raw = as.integer(t_interval[[1L]]),
          tend_raw = as.integer(t_interval[[2L]]),
          stringsAsFactors = FALSE
        )
      }

      if (.paf_query_consumes(op)) {
        if (identical(qstrand, "-")) {
          qcursor <- qcursor - len
        } else {
          qcursor <- qcursor + len
        }
      }
      if (.paf_target_consumes(op)) {
        tcursor <- tcursor + len
      }
    }

    rows <- Filter(Negate(is.null), rows)
    if (length(rows) > 0L) {
      block_rows[[i]] <- dplyr::bind_rows(rows)
    }
  }

  out <- dplyr::bind_rows(Filter(Negate(is.null), block_rows))
  if (nrow(out) == 0L) {
    return(.empty_pairwise_alignment_detail_table())
  }

  rownames(out) <- NULL
  out
}

.read_pairwise_paf <- function(path, cigar = FALSE) {
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
  paf$cigar <- .paf_cigar_from_optional(paf)
  if (isTRUE(cigar)) {
    return(.expand_paf_matches(paf))
  }
  paf$cigar <- NULL
  paf
}

.empty_pairwise_alignment_table <- function() {
  out <- data.frame(
    qchr = character(),
    qlen = integer(),
    qstart = integer(),
    qend = integer(),
    strand = character(),
    tchr = character(),
    tlen = integer(),
    tstart = integer(),
    tend = integer(),
    nmatch = integer(),
    alen = integer(),
    mapq = integer(),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

.psl_is_integer_field <- function(x) {
  length(x) == 1L &&
    !is.na(x) &&
    grepl("^[+-]?[0-9]+$", trimws(as.character(x)))
}

.psl_seqname_from_field <- function(x, species = NULL) {
  if (length(x) != 1L || is.na(x) || !nzchar(x)) {
    return(NA_character_)
  }

  value <- as.character(x)
  if (!is.null(species) && nzchar(species)) {
    prefix <- paste0(species, "_")
    if (startsWith(value, prefix)) {
      value <- substring(value, nchar(prefix) + 1L)
    }
  }

  value <- sub("_(\\d+)_(\\d+)$", "", value, perl = TRUE)
  value
}

.psl_relative_strand <- function(strand) {
  strand <- as.character(strand)
  ifelse(
    nchar(strand) <= 1L,
    strand,
    ifelse(
      substring(strand, 1L, 1L) == substring(strand, nchar(strand), nchar(strand)),
      "+",
      "-"
    )
  )
}

.parse_psl_block_sizes <- function(x) {
  if (is.na(x) || !nzchar(as.character(x))) {
    return(integer())
  }
  values <- strsplit(as.character(x), ",", fixed = TRUE)[[1L]]
  values <- trimws(values)
  values <- values[nzchar(values)]
  as.integer(values)
}

.parse_psl_block_starts <- function(x) {
  .parse_psl_block_sizes(x)
}

.psl_query_strand <- function(strand) {
  strand <- as.character(strand)
  out <- ifelse(nchar(strand) >= 1L, substring(strand, 1L, 1L), "+")
  out[is.na(out) | !out %in% c("+", "-")] <- "+"
  out
}

.psl_target_strand <- function(strand) {
  strand <- as.character(strand)
  out <- ifelse(nchar(strand) >= 2L, substring(strand, nchar(strand), nchar(strand)), "+")
  out[is.na(out) | !out %in% c("+", "-")] <- "+"
  out
}

.psl_oriented_interval <- function(start, width, seq_length, strand = "+") {
  start <- as.integer(start)
  width <- as.integer(width)
  seq_length <- as.integer(seq_length)

  if (anyNA(c(start, width, seq_length))) {
    return(c(NA_integer_, NA_integer_))
  }

  if (identical(strand, "-")) {
    return(c(seq_length - (start + width), seq_length - start))
  }

  c(start, start + width)
}

.expand_psl_blocks <- function(psl,
                               query_individual = NULL,
                               target_individual = NULL) {
  block_rows <- vector("list", nrow(psl))

  for (i in seq_len(nrow(psl))) {
    block_sizes <- .parse_psl_block_sizes(psl$blockSizes[[i]])
    q_starts <- .parse_psl_block_starts(psl$qStarts[[i]])
    t_starts <- .parse_psl_block_starts(psl$tStarts[[i]])
    block_n <- min(length(block_sizes), length(q_starts), length(t_starts))

    if (block_n == 0L) {
      next
    }

    block_sizes <- as.integer(block_sizes[seq_len(block_n)])
    q_starts <- as.integer(q_starts[seq_len(block_n)])
    t_starts <- as.integer(t_starts[seq_len(block_n)])
    q_intervals <- t(vapply(
      seq_len(block_n),
      function(j) .psl_oriented_interval(q_starts[[j]], block_sizes[[j]], psl$qSize[[i]], .psl_query_strand(psl$strand_raw[[i]])),
      integer(2)
    ))
    t_intervals <- t(vapply(
      seq_len(block_n),
      function(j) .psl_oriented_interval(t_starts[[j]], block_sizes[[j]], psl$tSize[[i]], .psl_target_strand(psl$strand_raw[[i]])),
      integer(2)
    ))

    block_rows[[i]] <- data.frame(
      qchr = rep(.psl_seqname_from_field(psl$qChrom[[i]], species = query_individual), block_n),
      qlen = rep(as.integer(psl$qSize[[i]]), block_n),
      qstart = as.integer(q_intervals[, 1L]),
      qend = as.integer(q_intervals[, 2L]),
      strand = rep(.psl_relative_strand(psl$strand_raw[[i]]), block_n),
      tchr = rep(.psl_seqname_from_field(psl$tName[[i]], species = target_individual), block_n),
      tlen = rep(as.integer(psl$tSize[[i]]), block_n),
      tstart = as.integer(t_intervals[, 1L]),
      tend = as.integer(t_intervals[, 2L]),
      nmatch = block_sizes,
      alen = block_sizes,
      mapq = rep(NA_integer_, block_n),
      psl_row = rep(as.integer(i), block_n),
      block_index = seq_len(block_n),
      block_size = block_sizes,
      qstrand = rep(.psl_query_strand(psl$strand_raw[[i]]), block_n),
      tstrand = rep(.psl_target_strand(psl$strand_raw[[i]]), block_n),
      qstart_raw = q_starts,
      qend_raw = q_starts + block_sizes,
      tstart_raw = t_starts,
      tend_raw = t_starts + block_sizes,
      stringsAsFactors = FALSE
    )
  }

  out <- dplyr::bind_rows(Filter(Negate(is.null), block_rows))
  if (nrow(out) == 0L) {
    return(.empty_pairwise_alignment_table())
  }

  rownames(out) <- NULL
  out
}

.normalize_psl_fields <- function(fields) {
  fields <- trimws(as.character(fields))

  if (length(fields) >= 22L &&
      !.psl_is_integer_field(fields[[1L]]) &&
      .psl_is_integer_field(fields[[2L]])) {
    out <- list(
      matches = fields[[2L]],
      misMatches = fields[[3L]],
      repMatches = fields[[4L]],
      nCount = fields[[5L]],
      qNumInsert = fields[[6L]],
      qBaseInsert = fields[[7L]],
      tNumInsert = fields[[8L]],
      tBaseInsert = fields[[9L]],
      strand_raw = fields[[10L]],
      qName = fields[[1L]],
      qChrom = fields[[11L]],
      qSize = fields[[12L]],
      qStart = fields[[13L]],
      qEnd = fields[[14L]],
      tName = fields[[15L]],
      tSize = fields[[16L]],
      tStart = fields[[17L]],
      tEnd = fields[[18L]],
      blockCount = fields[[19L]],
      blockSizes = fields[[20L]],
      qStarts = fields[[21L]],
      tStarts = fields[[22L]]
    )
  } else if (length(fields) >= 21L && .psl_is_integer_field(fields[[1L]])) {
    out <- list(
      matches = fields[[1L]],
      misMatches = fields[[2L]],
      repMatches = fields[[3L]],
      nCount = fields[[4L]],
      qNumInsert = fields[[5L]],
      qBaseInsert = fields[[6L]],
      tNumInsert = fields[[7L]],
      tBaseInsert = fields[[8L]],
      strand_raw = fields[[9L]],
      qName = fields[[10L]],
      qChrom = fields[[10L]],
      qSize = fields[[11L]],
      qStart = fields[[12L]],
      qEnd = fields[[13L]],
      tName = fields[[14L]],
      tSize = fields[[15L]],
      tStart = fields[[16L]],
      tEnd = fields[[17L]],
      blockCount = fields[[18L]],
      blockSizes = fields[[19L]],
      qStarts = fields[[20L]],
      tStarts = fields[[21L]]
    )
  } else {
    return(NULL)
  }

  required_numeric <- c(
    "matches", "misMatches", "repMatches", "nCount",
    "qNumInsert", "qBaseInsert", "tNumInsert", "tBaseInsert",
    "qSize", "qStart", "qEnd", "tSize", "tStart", "tEnd", "blockCount"
  )
  numeric_values <- suppressWarnings(as.integer(unlist(out[required_numeric], use.names = FALSE)))
  if (anyNA(numeric_values)) {
    return(NULL)
  }

  out
}

#' Read a PSL pairwise alignment into ggexon's internal link table
#'
#' Parses a UCSC PSL file and returns the PAF-like table used internally by
#' `ggexon` for pairwise link dispatch. By default the parser keeps one row per
#' PSL record and normalizes the output columns to `qchr`, `qstart`, `qend`,
#' `tchr`, `tstart`, `tend`, `strand`, `nmatch`, `alen`, and related fields
#' expected by [`pairwise_alignment_data()`] and [`geom_nuclink()`]. When
#' `more = TRUE`, each PSL record is expanded into one row per ungapped block.
#'
#' @param path Path to a PSL file.
#' @param query_individual Optional query-side individual identifier used to
#'   strip a species prefix from `qName` when inferring `qchr`.
#' @param target_individual Optional target-side individual identifier used to
#'   strip a species prefix from `tName` when inferring `tchr`.
#' @param more Logical; when `TRUE`, expand each PSL record into one row per
#'   ungapped alignment block using `blockSizes`, `qStarts`, `tStarts`, the
#'   query/target sequence lengths, and the PSL strand field to compute
#'   detailed block coordinates.
#'
#' @return A PAF-like `data.frame`. When `more = TRUE`, the returned table
#'   includes additional block-level columns such as `psl_row`, `block_index`,
#'   `block_size`, `qstrand`, `tstrand`, and raw block starts.
#' @export
read_pairwise_psl <- function(path,
                              query_individual = NULL,
                              target_individual = NULL,
                              more = FALSE) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`path` must be a single non-empty character value.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("PSL file not found: ", path, call. = FALSE)
  }

  lines <- readLines(path, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0L) {
    return(.empty_pairwise_alignment_table())
  }

  fields <- lapply(strsplit(lines, "\t", fixed = TRUE), .normalize_psl_fields)
  fields <- Filter(Negate(is.null), fields)
  if (length(fields) == 0L) {
    return(.empty_pairwise_alignment_table())
  }

  chr_field <- function(name) {
    vapply(fields, function(x) x[[name]], character(1), USE.NAMES = FALSE)
  }
  int_field <- function(name) {
    suppressWarnings(as.integer(chr_field(name)))
  }

  psl <- data.frame(
    matches = int_field("matches"),
    misMatches = int_field("misMatches"),
    repMatches = int_field("repMatches"),
    nCount = int_field("nCount"),
    qNumInsert = int_field("qNumInsert"),
    qBaseInsert = int_field("qBaseInsert"),
    tNumInsert = int_field("tNumInsert"),
    tBaseInsert = int_field("tBaseInsert"),
    strand_raw = chr_field("strand_raw"),
    qName = chr_field("qName"),
    qChrom = chr_field("qChrom"),
    qSize = int_field("qSize"),
    qStart = int_field("qStart"),
    qEnd = int_field("qEnd"),
    tName = chr_field("tName"),
    tSize = int_field("tSize"),
    tStart = int_field("tStart"),
    tEnd = int_field("tEnd"),
    blockCount = int_field("blockCount"),
    blockSizes = chr_field("blockSizes"),
    qStarts = chr_field("qStarts"),
    tStarts = chr_field("tStarts"),
    stringsAsFactors = FALSE
  )

  block_totals <- vapply(psl$blockSizes, function(x) {
    sum(.parse_psl_block_sizes(x), na.rm = TRUE)
  }, integer(1))

  if (isTRUE(more)) {
    out <- .expand_psl_blocks(
      psl,
      query_individual = query_individual,
      target_individual = target_individual
    )
    rownames(out) <- NULL
    return(out)
  }

  out <- data.frame(
    qchr = vapply(psl$qChrom, .psl_seqname_from_field, character(1), species = query_individual),
    qlen = as.integer(psl$qSize),
    qstart = as.integer(psl$qStart),
    qend = as.integer(psl$qEnd),
    strand = .psl_relative_strand(psl$strand_raw),
    tchr = vapply(psl$tName, .psl_seqname_from_field, character(1), species = target_individual),
    tlen = as.integer(psl$tSize),
    tstart = as.integer(psl$tStart),
    tend = as.integer(psl$tEnd),
    nmatch = as.integer(psl$matches + psl$repMatches),
    alen = as.integer(block_totals),
    mapq = NA_integer_,
    stringsAsFactors = FALSE
  )

  rownames(out) <- NULL
  out
}

.pairwise_alignment_table <- function(x, odgi = NULL, python = NULL, more = NULL, cigar = NULL) {
  use_more <- if (is.null(more)) FALSE else isTRUE(more)
  use_cigar <- if (is.null(cigar)) FALSE else isTRUE(cigar)
  use_cached <- !is.null(x@data)
  if (use_cached && identical(alignment_format(x), "psl") && !is.null(more)) {
    use_cached <- identical(isTRUE(x@metadata$psl_more), use_more)
  }
  if (use_cached && identical(alignment_format(x), "paf") && !is.null(cigar)) {
    use_cached <- identical(isTRUE(x@metadata$paf_detail), use_cigar)
  }

  paf <- if (use_cached) {
    x@data
  } else if (identical(alignment_format(x), "paf")) {
    .read_pairwise_paf(alignment_file(x), cigar = use_cigar)
  } else if (identical(alignment_format(x), "psl")) {
    read_pairwise_psl(
      alignment_file(x),
      query_individual = query_individual(x),
      target_individual = target_individual(x),
      more = use_more
    )
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
                                          python = NULL,
                                          more = NULL,
                                          cigar = NULL) {
  paf <- .pairwise_alignment_table(
    x,
    odgi = odgi,
    python = python,
    more = more,
    cigar = cigar
  )

  if (!is.null(subset)) {
    subset_specs <- .parse_pairwise_subset(
      subset = subset,
      pair = x,
      species_obj = species_obj,
      paf = paf
    )

    qspec <- subset_specs[[query_individual(x)]]
    tspec <- subset_specs[[target_individual(x)]]

    keep <- rep(TRUE, nrow(paf))
    if (!is.null(qspec)) {
      keep <- keep &
        as.character(paf$qchr) == qspec$chr &
        paf$qstart < qspec$end &
        paf$qend > qspec$start
    }
    if (!is.null(tspec)) {
      keep <- keep &
        as.character(paf$tchr) == tspec$chr &
        paf$tstart < tspec$end &
        paf$tend > tspec$start
    }

    paf <- paf[keep, , drop = FALSE]
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
#' objects. Pairwise alignments can currently be loaded from PAF, PSL, and ODGI
#' sources, and multiple alignments can be loaded when `format = "odgi"` points
#' to an ODGI node-table TSV or raw `.og` graph file. When `x` is a
#' [`SynSpecies`], every stored pairwise and multiple alignment is loaded and
#' the updated `SynSpecies` object is returned.
#'
#' Unloaded `SynMultiAlignment` objects with `format = "maf"` are not yet
#' supported because the package does not currently provide a MAF parser. For
#' PSL-backed [`SynPairAlignment`] objects, repeated calls to
#' `load_alignment()` with different explicit `more` values will replace the
#' cached data so the in-memory representation matches the requested detail
#' level, while `more = NULL` preserves any existing cached PSL mode.
#'
#' @param x A [`SynPairAlignment`], [`SynMultiAlignment`], or [`SynSpecies`]
#'   object.
#' @param odgi Optional path to the `odgi` executable. Used when loading ODGI
#'   multiple alignments from raw `.og` graph files.
#' @param python Optional path to the Python interpreter. Used when loading
#'   ODGI multiple alignments from raw `.og` graph files.
#' @param more Logical or `NULL`; when `TRUE` and `x` is a PSL-backed
#'   [`SynPairAlignment`], expand each PSL record into one row per ungapped
#'   block before caching the parsed data. When `NULL`, preserve any existing
#'   cached PSL detail level and default unloaded PSL alignments to the coarse
#'   one-row-per-record representation.
#' @param cigar Logical or `NULL`; when `TRUE` and `x` is a PAF-backed
#'   [`SynPairAlignment`], expand each `cg:Z:` CIGAR string into one row per
#'   match block before caching the parsed data. Only match operations are
#'   emitted; gap and mismatch operations are used only to advance coordinates.
#'   When `NULL`, preserve any existing cached PAF detail level and default
#'   unloaded PAF alignments to the coarse one-row-per-record representation.
#' @param alignment Optional stored alignment name when `x` is a `SynSpecies`.
#'   When omitted, all stored pairwise and multiple alignments are loaded.
#'
#' @details
#' This is an S4 generic that dispatches on the class of `x`.
#'
#' @return An updated object of the same class as `x`.
#'
#' @examples
#' paf_path <- system.file("extdata", "V_alginment.paf", package = "ggexon")
#' pair <- SynPairAlignment(
#'   name = "XZ1516_vs_N2",
#'   query_individual = "XZ1516",
#'   target_individual = "N2",
#'   file = paf_path
#' )
#' pair <- load_alignment(pair)
#'
#' @export
setGeneric("load_alignment", function(x, odgi = NULL, python = NULL, more = NULL, cigar = NULL, alignment = NULL) {
  standardGeneric("load_alignment")
})

#' @rdname load_alignment
setMethod("load_alignment", "SynPairAlignment", function(x, odgi = NULL, python = NULL, more = NULL, cigar = NULL, alignment = NULL) {
  if (identical(alignment_format(x), "psl")) {
    psl_more <- more
    if (is.null(psl_more)) {
      psl_more <- if (is.null(x@data)) FALSE else isTRUE(x@metadata$psl_more)
    }
    reload_psl <- is.null(x@data) ||
      !identical(isTRUE(x@metadata$psl_more), isTRUE(psl_more))
  } else {
    psl_more <- more
    reload_psl <- FALSE
  }

  if (identical(alignment_format(x), "paf")) {
    paf_detail <- cigar
    if (is.null(paf_detail)) {
      paf_detail <- if (is.null(x@data)) FALSE else isTRUE(x@metadata$paf_detail)
    }
    reload_paf <- is.null(x@data) ||
      !identical(isTRUE(x@metadata$paf_detail), isTRUE(paf_detail))
  } else {
    paf_detail <- cigar
    reload_paf <- FALSE
  }

  if (is.null(x@data) || reload_psl || reload_paf) {
    x@data <- .pairwise_alignment_table(
      x,
      odgi = odgi,
      python = python,
      more = psl_more,
      cigar = paf_detail
    )
    if (identical(alignment_format(x), "psl")) {
      x@metadata$psl_more <- isTRUE(psl_more)
    }
    if (identical(alignment_format(x), "paf")) {
      x@metadata$paf_detail <- isTRUE(paf_detail)
    }
  }
  x@loaded <- TRUE
  x@lazy <- FALSE
  x
})

#' @rdname load_alignment
setMethod("load_alignment", "SynMultiAlignment", function(x, odgi = NULL, python = NULL, more = NULL, cigar = NULL, alignment = NULL) {
  if (is.null(x@data)) {
    if (!identical(alignment_format(x), "odgi")) {
      stop(
        "`load_alignment()` currently supports unloaded SynMultiAlignment objects only when `format = 'odgi'`.",
        call. = FALSE
      )
    }
    x@data <- multiple_alignment_data(x, odgi = odgi, python = python)
  }
  if (identical(alignment_format(x), "odgi") && is.null(x@metadata$odgi_labels)) {
    x@metadata$odgi_labels <- .infer_odgi_label_mapping(
      tbl = x@data,
      individuals = x@individuals
    )
  }
  x@loaded <- TRUE
  x@lazy <- FALSE
  x
})

#' @rdname load_alignment
setMethod("load_alignment", "SynSpecies", function(x, odgi = NULL, python = NULL, more = NULL, cigar = NULL, alignment = NULL) {
  if (!is.null(alignment)) {
    resolved <- .resolve_stored_alignment_arg(x, alignment = alignment)
    if (identical(resolved$type, "pairwise")) {
      x@pairwise_alignments[[alignment]] <- load_alignment(
        resolved$object,
        odgi = odgi,
        python = python,
        more = more,
        cigar = cigar
      )
    } else {
      x@multiple_alignments[[alignment]] <- load_alignment(
        resolved$object,
        odgi = odgi,
        python = python
      )
    }
    validObject(x)
    return(x)
  }

  pairs <- pairwise_alignments(x)
  if (length(pairs) > 0L) {
    x@pairwise_alignments <- lapply(
      pairs,
      load_alignment,
      odgi = odgi,
      python = python,
      more = more,
      cigar = cigar
    )
  }

  multis <- multiple_alignments(x)
  if (length(multis) > 0L) {
    x@multiple_alignments <- lapply(multis, load_alignment, odgi = odgi, python = python)
  }

  validObject(x)
  x
})

.resolve_stored_alignment_arg <- function(x, alignment) {
  if (!methods::is(x, "SynSpecies")) {
    stop("`.resolve_stored_alignment_arg()` expects a SynSpecies object.", call. = FALSE)
  }
  if (!is.character(alignment) || length(alignment) != 1L || is.na(alignment) || !nzchar(alignment)) {
    stop("`alignment` must be a single non-empty character value.", call. = FALSE)
  }

  pair_list <- pairwise_alignments(x)
  if (alignment %in% names(pair_list)) {
    return(list(type = "pairwise", object = pair_list[[alignment]]))
  }

  multi_list <- multiple_alignments(x)
  if (alignment %in% names(multi_list)) {
    return(list(type = "multiple", object = multi_list[[alignment]]))
  }

  stop(
    "`alignment` must match a stored pairwise or multiple alignment: ",
    alignment,
    call. = FALSE
  )
}

.parse_pairwise_subset <- function(subset, pair, species_obj = NULL, paf) {
  if (is.list(subset) && !is.atomic(subset)) {
    subset <- unlist(subset, use.names = TRUE)
  }
  if (!is.character(subset) || length(subset) < 1L || is.null(names(subset))) {
    stop(
      "`subset` must be a named character vector/list with one region for one or both species in the pairwise alignment.",
      call. = FALSE
    )
  }

  subset <- subset[!is.na(names(subset)) & nzchar(names(subset))]
  subset <- subset[names(subset) %in% alignment_individuals(pair)]
  if (length(subset) == 0L) {
    stop(
      "`subset` must be named with one or both species in the pairwise alignment: ",
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
      if (methods::is(individual, "SynIndividual")) {
        spec$chr <- resolve_syn_seqname(individual, spec$chr)
      }
    }
    spec$paf_chr <- paf_chr
    spec$chr <- paf_chr
    spec <- .complete_pairwise_subset_window(
      spec = spec,
      species_name = species_name,
      pair = pair,
      species_obj = species_obj,
      paf = paf
    )
    spec
  })
  names(out) <- names(subset)

  missing_species <- setdiff(alignment_individuals(pair), names(out))
  for (species_name in missing_species) {
    out[[species_name]] <- NULL
  }

  out
}

.parse_region_string <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop("Each `subset` entry must be a single region string.", call. = FALSE)
  }

  region <- gsub("\\s+", "", x)
  region <- chartr("\uFF1A", ":", region)
  region <- gsub(",", "", region, fixed = TRUE)

  if (!grepl(":", region, fixed = TRUE)) {
    return(list(chr = region, start = NULL, end = NULL))
  }

  m <- regexec("^([^:]+):(\\d+)-(\\d+)$", region)
  hits <- regmatches(region, m)[[1L]]
  if (length(hits) != 4L) {
    stop("Region must look like `chr` or `chr:start-end`: ", x, call. = FALSE)
  }

  start <- as.integer(hits[[3L]])
  end <- as.integer(hits[[4L]])
  list(
    chr = hits[[2L]],
    start = min(start, end),
    end = max(start, end)
  )
}

.complete_pairwise_subset_window <- function(spec,
                                             species_name,
                                             pair,
                                             species_obj = NULL,
                                             paf) {
  if (!is.null(spec$start) && !is.null(spec$end)) {
    return(spec)
  }

  chr_col <- if (identical(species_name, query_individual(pair))) "qchr" else "tchr"
  start_col <- if (identical(species_name, query_individual(pair))) "qstart" else "tstart"
  end_col <- if (identical(species_name, query_individual(pair))) "qend" else "tend"

  if (!is.null(species_obj) && methods::is(species_obj, "SynSpecies")) {
    individual <- individuals(species_obj)[[species_name]]
    if (methods::is(individual, "SynIndividual")) {
      seqinfo_obj <- seqinfo(individual)
      if (!is.null(seqinfo_obj)) {
        seq_lengths <- GenomeInfoDb::seqlengths(seqinfo_obj)
        if (spec$chr %in% names(seq_lengths)) {
          chr_length <- seq_lengths[[spec$chr]]
          if (!is.na(chr_length) && chr_length > 0) {
            spec$start <- 1L
            spec$end <- as.integer(chr_length)
            return(spec)
          }
        }
      }
    }
  }

  chr_rows <- as.character(paf[[chr_col]]) == spec$chr
  chr_starts <- paf[[start_col]][chr_rows]
  chr_ends <- paf[[end_col]][chr_rows]
  if (length(chr_starts) == 0L || length(chr_ends) == 0L) {
    stop("No pairwise alignment rows found on chromosome ", spec$chr, ".", call. = FALSE)
  }

  spec$start <- as.integer(min(chr_starts, na.rm = TRUE))
  spec$end <- as.integer(max(chr_ends, na.rm = TRUE))
  spec
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

    part_matches <- lower_available %in% base::tolower(chr_parts)
    if (sum(part_matches) == 1L) {
      return(available[part_matches])
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
  if (!methods::is(individual, "SynIndividual")) {
    return(GenomicRanges::GRanges())
  }

  query_features(
    individual,
    chr = chr,
    start = start,
    end = end,
    feature_type = NULL
  )
}

# ── HomologyAnnotation accessors on SynSpecies ──────────────────────────

#' List homology annotations attached to a SynSpecies
#'
#' @param x A `SynSpecies` object.
#' @return A named list of `HomologyAnnotation` objects.
#' @export
setGeneric("homology_annotations", function(x) standardGeneric("homology_annotations"))
#' @rdname homology_annotations
setMethod("homology_annotations", "SynSpecies", function(x) x@homology_annotations)

#' Add or replace a HomologyAnnotation on a SynSpecies
#'
#' @param x A `SynSpecies` object.
#' @param homology A `HomologyAnnotation` object.
#' @return An updated `SynSpecies` object.
#' @export
setGeneric("add_homology_annotation", function(x, homology) {
  standardGeneric("add_homology_annotation")
}, signature = c("x", "homology"))
#' @rdname add_homology_annotation
setMethod("add_homology_annotation", c("SynSpecies", "HomologyAnnotation"), function(x, homology) {
  x@homology_annotations[[annotation_name(homology)]] <- homology
  validObject(x)
  x
})
#' @rdname add_homology_annotation
setMethod("add_homology_annotation", c("SynSpecies", "ANY"), function(x, homology) {
  stop("`homology` must be a HomologyAnnotation object.", call. = FALSE)
})
#' @rdname add_homology_annotation
setMethod("add_homology_annotation", c("ANY", "HomologyAnnotation"), function(x, homology) {
  stop("`add_homology_annotation()` expects a SynSpecies object.", call. = FALSE)
})
#' @rdname add_homology_annotation
setMethod("add_homology_annotation", c("ANY", "ANY"), function(x, homology) {
  stop("`add_homology_annotation()` expects a SynSpecies object.", call. = FALSE)
})

#' Retrieve a HomologyAnnotation from a SynSpecies by query species
#'
#' When `query_species` is supplied, the first `HomologyAnnotation` whose
#' `query_species` matches is returned. When `name` is supplied, the annotation
#' with that exact name is returned. Provide one or the other.
#'
#' @param x A `SynSpecies` object.
#' @param query_species Optional name of the query species.
#' @param name Optional name of the homology annotation layer.
#' @return A `HomologyAnnotation` object, or `NULL` when not found.
#' @export
get_homology_annotation <- function(x, query_species = NULL, name = NULL) {
  if (!methods::is(x, "SynSpecies")) {
    stop("`get_homology_annotation()` expects a SynSpecies object.", call. = FALSE)
  }
  if (!is.null(name)) {
    if (!is.null(query_species)) {
      stop("Provide either `query_species` or `name`, not both.", call. = FALSE)
    }
    hit <- x@homology_annotations[[name]]
    if (is.null(hit)) {
      return(NULL)
    }
    return(hit)
  }

  if (!is.null(query_species)) {
    if (!is.character(query_species) || length(query_species) != 1L ||
        is.na(query_species) || !nzchar(query_species)) {
      stop("`query_species` must be a single non-empty character value.", call. = FALSE)
    }
    for (ha in x@homology_annotations) {
      if (.homology_same_species(methods::slot(ha, "query_species"), query_species)) {
        return(ha)
      }
    }
    return(NULL)
  }

  if (length(x@homology_annotations) == 1L) {
    return(x@homology_annotations[[1L]])
  }

  stop("Provide `query_species` or `name` when multiple homology annotations are attached.", call. = FALSE)
}

.resolve_homology_annotation_for_edit <- function(x, name = NULL, query_species = NULL) {
  if (!methods::is(x, "SynSpecies")) {
    stop("Homology edits on containers require a SynSpecies object.", call. = FALSE)
  }
  if (!is.null(name) && !is.null(query_species)) {
    stop("Provide either `name` or `query_species`, not both.", call. = FALSE)
  }
  if (length(x@homology_annotations) == 0L) {
    stop("No HomologyAnnotation objects are attached to this SynSpecies.", call. = FALSE)
  }

  if (!is.null(name)) {
    if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
      stop("`name` must be a single non-empty character value.", call. = FALSE)
    }
    hit <- match(name, names(x@homology_annotations))
    if (is.na(hit)) {
      stop("No HomologyAnnotation named `", name, "` is attached.", call. = FALSE)
    }
    return(list(index = hit, homology = x@homology_annotations[[hit]]))
  }

  if (!is.null(query_species)) {
    if (!is.character(query_species) || length(query_species) != 1L ||
        is.na(query_species) || !nzchar(query_species)) {
      stop("`query_species` must be a single non-empty character value.", call. = FALSE)
    }
    hits <- which(vapply(
      x@homology_annotations,
      function(ha) .homology_same_species(methods::slot(ha, "query_species"), query_species),
      logical(1)
    ))
    if (length(hits) == 0L) {
      stop(
        "No HomologyAnnotation with `query_species` ",
        query_species,
        " is attached.",
        call. = FALSE
      )
    }
    if (length(hits) > 1L) {
      stop(
        "Multiple HomologyAnnotation objects match `query_species` ",
        query_species,
        "; use `name` to select one.",
        call. = FALSE
      )
    }
    return(list(index = hits[[1L]], homology = x@homology_annotations[[hits[[1L]]]]))
  }

  if (length(x@homology_annotations) == 1L) {
    return(list(index = 1L, homology = x@homology_annotations[[1L]]))
  }

  stop("Provide `query_species` or `name` when multiple homology annotations are attached.", call. = FALSE)
}

.replace_homology_annotation_for_edit <- function(x, target, homology) {
  x@homology_annotations[[target$index]] <- homology
  validObject(x)
  x
}

#' @rdname homology-crud
setMethod("add_homology", "SynSpecies", function(x,
                                                  data = NULL,
                                                  query_gene = NULL,
                                                  reference_gene = NULL,
                                                  ...,
                                                  name = NULL,
                                                  query_species = NULL,
                                                  overwrite = FALSE) {
  target <- .resolve_homology_annotation_for_edit(
    x,
    name = name,
    query_species = query_species
  )
  homology <- add_homology(
    target$homology,
    data = data,
    query_gene = query_gene,
    reference_gene = reference_gene,
    ...,
    overwrite = overwrite
  )
  .replace_homology_annotation_for_edit(x, target, homology)
})

#' @rdname homology-crud
setMethod("replace_homology", "SynSpecies", function(x,
                                                      data = NULL,
                                                      query_gene = NULL,
                                                      reference_gene = NULL,
                                                      ...,
                                                      name = NULL,
                                                      query_species = NULL,
                                                      add_missing = FALSE) {
  target <- .resolve_homology_annotation_for_edit(
    x,
    name = name,
    query_species = query_species
  )
  homology <- replace_homology(
    target$homology,
    data = data,
    query_gene = query_gene,
    reference_gene = reference_gene,
    ...,
    add_missing = add_missing
  )
  .replace_homology_annotation_for_edit(x, target, homology)
})

#' @rdname homology-crud
setMethod("delete_homology", "SynSpecies", function(x,
                                                     data = NULL,
                                                     query_gene = NULL,
                                                     reference_gene = NULL,
                                                     ...,
                                                     name = NULL,
                                                     query_species = NULL,
                                                     missing = c("error", "warn", "ignore")) {
  target <- .resolve_homology_annotation_for_edit(
    x,
    name = name,
    query_species = query_species
  )
  homology <- delete_homology(
    target$homology,
    data = data,
    query_gene = query_gene,
    reference_gene = reference_gene,
    ...,
    missing = missing
  )
  .replace_homology_annotation_for_edit(x, target, homology)
})
