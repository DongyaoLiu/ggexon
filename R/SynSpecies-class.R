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
    metadata = "list"
  ),
  prototype = list(
    name = NA_character_,
    individuals = list(),
    pairwise_alignments = list(),
    multiple_alignments = list(),
    metadata = list()
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
