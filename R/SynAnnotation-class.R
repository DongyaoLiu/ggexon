#' @include SynBioc-unions.R
NULL

#' SynAnnotation class hierarchy
#'
#' `SynAnnotation` is the abstract base class for annotation layers attached to
#' `SynIndividual` or `SynSpecies` objects. Individual-level and species-level
#' annotations are represented by dedicated abstract subclasses, with
#' genome-coordinate and protein-coordinate annotations further specializing the
#' individual-level branch.
#'
#' @name SynAnnotation-class-hierarchy
#' @section Class hierarchy:
#' * `SynAnnotation`: abstract base class
#' * `SynIndAnnotation`: abstract individual-level annotation
#' * `SynSpeAnnotation`: abstract species-level annotation
#' * `SynGenomeAnnotation`: abstract genome-coordinate individual annotation
#' * `SynProteinAnnotation`: abstract protein-coordinate individual annotation
#' * `SynFeatureAnnotation`: GFF/GTF structural annotation
#' * `SynAnnotationPatch`: gene-model patch record
#' * `SynVCFAnnotation`: VCF/BCF variant annotation
#' * `SynBigWigAnnotation`: BigWig signal annotation
#' * `SynProteinDomainAnnotation`: protein-domain annotation
#' * `SynProteinMutationAnnotation`: protein-coordinate mutation annotation
#'
#' @keywords internal
NULL

#' SynAnnotation class
#'
#' `SynAnnotation` is the abstract base class for all annotation layers attached
#' to `SynIndividual` or `SynSpecies` objects. Subclasses specialize the owner
#' level, coordinate system, and payload they carry, but all annotations share
#' the same naming, source-file, lazy loading, metadata, and plot-cache
#' semantics.
#'
#' @slot name Short unique label used to retrieve the annotation layer.
#' @slot source_file Path or paths to the on-disk file(s) backing the
#'   annotation.
#' @slot annotation_scope Scalar coordinate scope such as `"nucleotide"` or
#'   `"protein"`.
#' @slot lazy Logical; whether data loading should normally be deferred.
#' @slot loaded Logical; whether the payload has been materialized in memory.
#' @slot metadata Optional user or import metadata.
#' @slot plot_cache List used to store derived plotting tables or other
#'   reusable render-time state.
#'
#' @section Prototype defaults:
#' * `annotation_scope = "unknown"`
#' * `lazy = TRUE`
#' * `loaded = FALSE`
#' * `metadata = list()`
#' * `plot_cache = list()`
#'
#' @section Validity rules:
#' * `name` and `annotation_scope` must each be one non-empty character value.
#' * `source_file` must be a non-empty character vector with no empty entries.
#' * `lazy` and `loaded` must each be scalar logical values.
#'
#' @exportClass SynAnnotation
setClass(
  "SynAnnotation",
  contains = "VIRTUAL",
  slots = c(
    name = "character",
    source_file = "character",
    annotation_scope = "character",
    lazy = "logical",
    loaded = "logical",
    metadata = "list",
    plot_cache = "list"
  ),
  prototype = list(
    name = NA_character_,
    source_file = NA_character_,
    annotation_scope = "unknown",
    lazy = TRUE,
    loaded = FALSE,
    metadata = list(),
    plot_cache = list()
  ),
  validity = function(object) {
    problems <- character()

    if (length(object@name) != 1L || is.na(object@name) || !nzchar(object@name)) {
      problems <- c(problems, "`name` must be a single non-empty character value.")
    }
    if (length(object@source_file) == 0L ||
        any(is.na(object@source_file)) ||
        any(!nzchar(object@source_file))) {
      problems <- c(
        problems,
        "`source_file` must be a non-empty character vector with no empty entries."
      )
    }
    if (length(object@annotation_scope) != 1L ||
        is.na(object@annotation_scope) ||
        !nzchar(object@annotation_scope)) {
      problems <- c(
        problems,
        "`annotation_scope` must be a single non-empty character value."
      )
    }
    if (length(object@lazy) != 1L || is.na(object@lazy)) {
      problems <- c(problems, "`lazy` must be a single logical value.")
    }
    if (length(object@loaded) != 1L || is.na(object@loaded)) {
      problems <- c(problems, "`loaded` must be a single logical value.")
    }

    if (length(problems) == 0L) TRUE else problems
  }
)

#' SynIndAnnotation class
#'
#' `SynIndAnnotation` is an abstract subtype of `SynAnnotation` for annotation
#' layers attached to a `SynIndividual`.
#'
#' @exportClass SynIndAnnotation
setClass("SynIndAnnotation", contains = "SynAnnotation")

#' SynSpeAnnotation class
#'
#' `SynSpeAnnotation` is an abstract subtype of `SynAnnotation` for annotation
#' layers attached to a `SynSpecies`.
#'
#' @exportClass SynSpeAnnotation
setClass(
  "SynSpeAnnotation",
  contains = "SynAnnotation",
  prototype = list(annotation_scope = "species")
)

#' SynGenomeAnnotation class
#'
#' `SynGenomeAnnotation` is an abstract subtype of `SynIndAnnotation` for
#' annotations indexed on genomic coordinates.
#'
#' @exportClass SynGenomeAnnotation
setClass("SynGenomeAnnotation", contains = "SynIndAnnotation")

#' SynProteinAnnotation class
#'
#' `SynProteinAnnotation` is an abstract subtype of `SynIndAnnotation` for
#' annotations indexed on protein coordinates.
#'
#' @exportClass SynProteinAnnotation
setClass("SynProteinAnnotation", contains = "SynIndAnnotation")

#' SynFeatureAnnotation class
#'
#' `SynFeatureAnnotation` stores structural genome annotation such as genes,
#' transcripts, exons, CDS, and derived label or sequence caches. It is the
#' default feature-layer type attached to `SynIndividual`.
#'
#' In addition to the slots listed below, the class inherits `name`,
#' `source_file`, `annotation_scope`, `lazy`, `loaded`, `metadata`, and
#' `plot_cache` from `SynAnnotation`.
#'
#' @slot annotation_format Source annotation format. One of `"auto"`, `"gff"`,
#'   or `"gtf"`.
#' @slot base_annotation Optional immutable base `GRanges` before any patches
#'   are applied.
#' @slot annotation Optional active `GRanges` after patching or label updates.
#' @slot patches List of `SynAnnotationPatch` objects applied to this feature
#'   layer.
#' @slot feature_index Optional cached lookup structure for features.
#' @slot label_map Optional cached feature-id to display-label mapping.
#' @slot nucleotide_seq Optional cached nucleotide sequences extracted from the
#'   annotated features.
#' @slot protein_seq Optional cached translated protein sequences.
#'
#' @section Prototype defaults:
#' * `annotation_format = "auto"`
#' * `annotation_scope = "nucleotide"`
#' * `base_annotation = NULL`
#' * `annotation = NULL`
#' * `patches = list()`
#' * `feature_index = NULL`
#' * `label_map = NULL`
#' * `nucleotide_seq = NULL`
#' * `protein_seq = NULL`
#'
#' @section Validity rules:
#' * `annotation_format` must be one of `"auto"`, `"gff"`, or `"gtf"`, either
#'   length one or the same length as `source_file`.
#'
#' @exportClass SynFeatureAnnotation
setClass(
  "SynFeatureAnnotation",
  contains = "SynGenomeAnnotation",
  slots = c(
    annotation_format = "character",
    base_annotation = "NULLOrGRanges",
    annotation = "NULLOrGRanges",
    patches = "list",
    feature_index = "ANY",
    label_map = "ANY",
    nucleotide_seq = "NULLOrDNAStringSet",
    protein_seq = "NULLOrAAStringSet"
  ),
  prototype = list(
    annotation_format = "auto",
    annotation_scope = "nucleotide",
    base_annotation = NULL,
    annotation = NULL,
    patches = list(),
    feature_index = NULL,
    label_map = NULL,
    nucleotide_seq = NULL,
    protein_seq = NULL
  ),
  validity = function(object) {
    if (!(length(object@annotation_format) %in% c(1L, length(object@source_file))) ||
        any(!(object@annotation_format %in% c("auto", "gff", "gtf")))) {
      "`annotation_format` must be one of 'auto', 'gff', or 'gtf', with length 1 or the same length as `source_file`."
    } else {
      TRUE
    }
  }
)

#' SynAnnotationPatch class
#'
#' `SynAnnotationPatch` records a patch that replaces, adds, or drops a subset
#' of features from a `SynFeatureAnnotation`. As a genome-coordinate annotation,
#' it inherits the shared `SynGenomeAnnotation` metadata used across
#' nucleotide-level annotation layers.
#'
#' In addition to the slots listed below, the class inherits `name`,
#' `source_file`, `annotation_scope`, `lazy`, `loaded`, `metadata`, and
#' `plot_cache` from `SynAnnotation`.
#'
#' @slot name Patch label.
#' @slot source_file Optional source identifier describing where the patch came
#'   from. In-memory patches default to `"<patch>"`.
#' @slot patch_data Optional patch payload as `GRanges`. Required for `"replace"`
#'   and `"add"` patches.
#' @slot target_ids Character vector of feature identifiers targeted by the
#'   patch.
#' @slot mode Patch mode. One of `"replace"`, `"add"`, or `"drop"`.
#' @slot metadata Optional patch metadata.
#'
#' @section Prototype defaults:
#' * `source_file = "<patch>"`
#' * `annotation_scope = "nucleotide"`
#' * `lazy = FALSE`
#' * `loaded = TRUE`
#' * `patch_data = NULL`
#' * `target_ids = character()`
#' * `mode = "replace"`
#'
#' @section Validity rules:
#' * `mode` must be one of `"replace"`, `"add"`, or `"drop"`.
#' * `patch_data` is required when `mode` is `"replace"` or `"add"`.
#'
#' @exportClass SynAnnotationPatch
setClass(
  "SynAnnotationPatch",
  contains = "SynGenomeAnnotation",
  slots = c(
    patch_data = "NULLOrGRanges",
    target_ids = "character",
    mode = "character"
  ),
  prototype = list(
    source_file = "<patch>",
    annotation_scope = "nucleotide",
    lazy = FALSE,
    loaded = TRUE,
    patch_data = NULL,
    target_ids = character(),
    mode = "replace"
  ),
  validity = function(object) {
    problems <- character()
    if (length(object@mode) != 1L || !(object@mode %in% c("replace", "add", "drop"))) {
      problems <- c(problems, "`mode` must be one of 'replace', 'add', or 'drop'.")
    }
    if (object@mode != "drop" && is.null(object@patch_data)) {
      problems <- c(problems, "`patch_data` is required for 'replace' and 'add' patches.")
    }
    if (length(problems) == 0L) TRUE else problems
  }
)

#' SynVCFAnnotation class
#'
#' `SynVCFAnnotation` stores a region-queryable variant layer backed by a VCF or
#' BCF file.
#'
#' In addition to the slots listed below, the class inherits `name`,
#' `source_file`, `annotation_scope`, `lazy`, `loaded`, `metadata`, and
#' `plot_cache` from `SynAnnotation`.
#'
#' @slot data_format Variant file format label. Currently `"vcf"`.
#' @slot variants Optional cached parsed variant data.
#' @slot index_file Optional index file path for random access.
#' @slot genome_build Optional genome-build label.
#' @slot region_cache Cache of previously queried genomic windows.
#'
#' @section Prototype defaults:
#' * `data_format = "vcf"`
#' * `annotation_scope = "nucleotide"`
#' * `variants = NULL`
#' * `index_file = NA_character_`
#' * `genome_build = NA_character_`
#' * `region_cache = list()`
#'
#' @section Validity rules:
#' No additional custom validity checks are defined beyond the inherited slot
#' classes and parent-class rules.
#'
#' @exportClass SynVCFAnnotation
setClass(
  "SynVCFAnnotation",
  contains = "SynGenomeAnnotation",
  slots = c(
    data_format = "character",
    variants = "ANY",
    index_file = "character",
    genome_build = "character",
    region_cache = "list"
  ),
  prototype = list(
    data_format = "vcf",
    annotation_scope = "nucleotide",
    variants = NULL,
    index_file = NA_character_,
    genome_build = NA_character_,
    region_cache = list()
  )
)

#' SynBigWigAnnotation class
#'
#' `SynBigWigAnnotation` stores a region-queryable signal layer backed by a
#' BigWig file.
#'
#' In addition to the slots listed below, the class inherits `name`,
#' `source_file`, `annotation_scope`, `lazy`, `loaded`, `metadata`, and
#' `plot_cache` from `SynAnnotation`.
#'
#' @slot data_format Signal file format label. Currently `"bigwig"`.
#' @slot signal Optional cached parsed signal data.
#' @slot seqinfo Optional cached `Seqinfo` describing available sequences.
#' @slot window_cache Cache of previously queried genomic windows.
#'
#' @section Prototype defaults:
#' * `data_format = "bigwig"`
#' * `annotation_scope = "nucleotide"`
#' * `signal = NULL`
#' * `seqinfo = NULL`
#' * `window_cache = list()`
#'
#' @section Validity rules:
#' No additional custom validity checks are defined beyond the inherited slot
#' classes and parent-class rules.
#'
#' @exportClass SynBigWigAnnotation
setClass(
  "SynBigWigAnnotation",
  contains = "SynGenomeAnnotation",
  slots = c(
    data_format = "character",
    signal = "ANY",
    seqinfo = "ANY",
    window_cache = "list"
  ),
  prototype = list(
    data_format = "bigwig",
    annotation_scope = "nucleotide",
    signal = NULL,
    seqinfo = NULL,
    window_cache = list()
  ),
  validity = function(object) {
    if (!is.null(object@seqinfo) && !methods::is(object@seqinfo, "Seqinfo")) {
      "`seqinfo` must be a Seqinfo object or NULL."
    } else {
      TRUE
    }
  }
)

#' SynProteinDomainAnnotation class
#'
#' `SynProteinDomainAnnotation` stores protein-domain calls keyed by a protein-,
#' transcript-, or gene-level identifier.
#'
#' In addition to the slots listed below, the class inherits `name`,
#' `source_file`, `annotation_scope`, `lazy`, `loaded`, `metadata`, and
#' `plot_cache` from `SynAnnotation`.
#'
#' @slot data_format Domain file format label. Currently `"domain"`.
#' @slot domain_data Optional cached parsed domain table.
#' @slot keytype Identifier type used to join domain rows to package objects.
#' @slot source_db Optional source database label such as `"Pfam"` or
#'   `"InterPro"`.
#'
#' @section Prototype defaults:
#' * `data_format = "domain"`
#' * `annotation_scope = "protein"`
#' * `domain_data = NULL`
#' * `keytype = "protein_id"`
#' * `source_db = NA_character_`
#'
#' @section Validity rules:
#' No additional custom validity checks are defined beyond the inherited slot
#' classes and parent-class rules.
#'
#' @exportClass SynProteinDomainAnnotation
setClass(
  "SynProteinDomainAnnotation",
  contains = "SynProteinAnnotation",
  slots = c(
    data_format = "character",
    domain_data = "ANY",
    keytype = "character",
    source_db = "character"
  ),
  prototype = list(
    data_format = "domain",
    annotation_scope = "protein",
    domain_data = NULL,
    keytype = "protein_id",
    source_db = NA_character_
  )
)

#' SynProteinMutationAnnotation class
#'
#' `SynProteinMutationAnnotation` stores protein-coordinate mutation records,
#' such as amino-acid substitutions summarized across strains or individuals.
#' It is a protein-space annotation parallel to `SynProteinDomainAnnotation`.
#'
#' In addition to the slots listed below, the class inherits `name`,
#' `source_file`, `annotation_scope`, `lazy`, `loaded`, `metadata`, and
#' `plot_cache` from `SynAnnotation`.
#'
#' @slot data_format Mutation file format label. Currently
#'   `"protein_mutation"`.
#' @slot mutation_data Optional cached normalized mutation table.
#' @slot individual_index Optional long index mapping source mutation rows to
#'   individual/strain identifiers.
#' @slot keytype Identifier column used to join mutations to proteins,
#'   transcripts, or genes.
#'
#' @section Prototype defaults:
#' * `data_format = "protein_mutation"`
#' * `annotation_scope = "protein"`
#' * `mutation_data = NULL`
#' * `individual_index = NULL`
#' * `keytype = "gene_id"`
#'
#' @exportClass SynProteinMutationAnnotation
setClass(
  "SynProteinMutationAnnotation",
  contains = "SynProteinAnnotation",
  slots = c(
    data_format = "character",
    mutation_data = "ANY",
    individual_index = "ANY",
    keytype = "character"
  ),
  prototype = list(
    data_format = "protein_mutation",
    annotation_scope = "protein",
    mutation_data = NULL,
    individual_index = NULL,
    keytype = "gene_id"
  ),
  validity = function(object) {
    problems <- character()
    if (length(object@data_format) != 1L ||
        is.na(object@data_format) ||
        !nzchar(object@data_format)) {
      problems <- c(problems, "`data_format` must be a single non-empty character value.")
    }
    if (length(object@keytype) != 1L ||
        is.na(object@keytype) ||
        !nzchar(object@keytype)) {
      problems <- c(problems, "`keytype` must be a single non-empty character value.")
    }
    if (length(problems) == 0L) TRUE else problems
  }
)

#' Constructor for SynFeatureAnnotation
#'
#' @param name Short unique label for the annotation layer.
#' @param annotation_file Path or paths to the GFF or GTF file(s).
#' @param annotation_format One of `"auto"`, `"gff"`, or `"gtf"`, or a vector
#'   of the same length as `annotation_file`.
#' @param metadata Optional metadata list.
#' @param lazy Logical; whether downstream loading should default to lazy mode.
#'
#' @return A `SynFeatureAnnotation` object with deferred `GRanges`, patch, and
#'   cache slots.
#' @export
SynFeatureAnnotation <- function(name,
                                 annotation_file,
                                 annotation_format = "auto",
                                 metadata = list(),
                                 lazy = TRUE) {
  annotation_format <- .normalize_annotation_format_input(annotation_format, annotation_file)

  new(
    "SynFeatureAnnotation",
    name = name,
    source_file = annotation_file,
    annotation_format = annotation_format,
    lazy = lazy,
    metadata = metadata
  )
}

#' Constructor for SynAnnotationPatch
#'
#' @param name Patch label.
#' @param patch_data Optional patched gene model as `GRanges`.
#' @param target_ids Target gene IDs to replace, add, or drop.
#' @param mode One of `"replace"`, `"add"`, or `"drop"`.
#' @param source_file Optional source identifier for the patch. Defaults to
#'   `"<patch>"` for in-memory patch objects.
#' @param metadata Optional metadata list.
#'
#' @return A `SynAnnotationPatch` object.
#' @export
SynAnnotationPatch <- function(name,
                               patch_data = NULL,
                               target_ids = character(),
                               mode = c("replace", "add", "drop"),
                               source_file = "<patch>",
                               metadata = list()) {
  mode <- match.arg(mode)
  new(
    "SynAnnotationPatch",
    name = name,
    source_file = source_file,
    patch_data = patch_data,
    target_ids = as.character(target_ids),
    mode = mode,
    metadata = metadata
  )
}

#' Constructor for SynVCFAnnotation
#'
#' @param name Short unique label for the annotation layer.
#' @param vcf_file Path to the VCF or BCF file.
#' @param index_file Optional index path for large VCF/BCF access.
#' @param genome_build Optional genome build string.
#' @param metadata Optional metadata list.
#' @param lazy Logical; defaults to `TRUE` for region-based querying.
#'
#' @return A `SynVCFAnnotation` object.
#' @export
SynVCFAnnotation <- function(name,
                             vcf_file,
                             index_file = NA_character_,
                             genome_build = NA_character_,
                             metadata = list(),
                             lazy = TRUE) {
  new(
    "SynVCFAnnotation",
    name = name,
    source_file = vcf_file,
    lazy = lazy,
    metadata = metadata,
    index_file = index_file,
    genome_build = genome_build
  )
}

#' Constructor for SynBigWigAnnotation
#'
#' @param name Short unique label for the annotation layer.
#' @param bigwig_file Path to the BigWig file.
#' @param metadata Optional metadata list.
#' @param lazy Logical; defaults to `TRUE` for windowed loading.
#'
#' @return A `SynBigWigAnnotation` object.
#' @export
SynBigWigAnnotation <- function(name,
                                bigwig_file,
                                metadata = list(),
                                lazy = TRUE) {
  new(
    "SynBigWigAnnotation",
    name = name,
    source_file = bigwig_file,
    lazy = lazy,
    metadata = metadata
  )
}

#' Constructor for SynProteinDomainAnnotation
#'
#' @param name Short unique label for the annotation layer.
#' @param domain_file Path to the protein-domain annotation file.
#' @param keytype Key used to map domains to proteins or transcripts.
#' @param source_db Domain database source, such as `"Pfam"` or `"InterPro"`.
#' @param metadata Optional metadata list.
#' @param lazy Logical; whether to defer loading until requested.
#'
#' @return A `SynProteinDomainAnnotation` object.
#' @export
SynProteinDomainAnnotation <- function(name,
                                       domain_file,
                                       keytype = c("protein_id", "transcript_id", "gene_id"),
                                       source_db = NA_character_,
                                       metadata = list(),
                                       lazy = TRUE) {
  keytype <- match.arg(keytype)

  new(
    "SynProteinDomainAnnotation",
    name = name,
    source_file = domain_file,
    lazy = lazy,
    metadata = metadata,
    keytype = keytype,
    source_db = source_db
  )
}

#' Constructor for SynProteinMutationAnnotation
#'
#' @param name Short unique label for the annotation layer.
#' @param mutation_file Path to the protein-mutation annotation file.
#' @param keytype Identifier column used to match mutation rows to features.
#' @param mutation_data Optional normalized mutation table.
#' @param individual_index Optional long table mapping mutation rows to
#'   individual identifiers.
#' @param metadata Optional metadata list.
#' @param lazy Logical; whether to defer loading until requested.
#'
#' @return A `SynProteinMutationAnnotation` object.
#' @export
SynProteinMutationAnnotation <- function(name,
                                         mutation_file,
                                         keytype = "gene_id",
                                         mutation_data = NULL,
                                         individual_index = NULL,
                                         metadata = list(),
                                         lazy = TRUE) {
  if (!is.character(keytype) || length(keytype) != 1L ||
      is.na(keytype) || !nzchar(keytype)) {
    stop("`keytype` must be a single non-empty character value.", call. = FALSE)
  }

  new(
    "SynProteinMutationAnnotation",
    name = name,
    source_file = mutation_file,
    lazy = lazy,
    loaded = !is.null(mutation_data),
    metadata = metadata,
    keytype = keytype,
    mutation_data = mutation_data,
    individual_index = individual_index
  )
}

#' @export
#' @rdname ggexon-show
setMethod("show", "SynAnnotation", function(object) {
  cat("An object of class \"", class(object), "\"\n", sep = "")
  cat("  name:", object@name, "\n")
  cat("  source_file:", object@source_file, "\n")
  cat("  annotation_scope:", object@annotation_scope, "\n")
  cat("  lazy:", object@lazy, "\n")
  cat("  loaded:", object@loaded, "\n")
})

setGeneric("annotation_name", function(x) standardGeneric("annotation_name"))
setMethod("annotation_name", "SynAnnotation", function(x) x@name)

setGeneric("annotation_kind", function(x) standardGeneric("annotation_kind"))
setMethod("annotation_kind", "SynAnnotation", function(x) class(x)[[1L]])

setGeneric("source_file", function(x) standardGeneric("source_file"))
setMethod("source_file", "SynAnnotation", function(x) x@source_file)

setGeneric("annotation_scope", function(x) standardGeneric("annotation_scope"))
setMethod("annotation_scope", "SynAnnotation", function(x) x@annotation_scope)

setGeneric("is_lazy", function(x) standardGeneric("is_lazy"))
setMethod("is_lazy", "SynAnnotation", function(x) x@lazy)

setGeneric("is_loaded", function(x) standardGeneric("is_loaded"))
setMethod("is_loaded", "SynAnnotation", function(x) x@loaded)

setGeneric("annotation_metadata", function(x) {
  standardGeneric("annotation_metadata")
})
setMethod("annotation_metadata", "SynAnnotation", function(x) x@metadata)

setGeneric("annotation_metadata<-", function(x, value) {
  standardGeneric("annotation_metadata<-")
})
setReplaceMethod("annotation_metadata", "SynAnnotation", function(x, value) {
  if (!is.list(value)) {
    stop("`annotation_metadata<-` expects a list.", call. = FALSE)
  }
  x@metadata <- value
  validObject(x)
  x
})

setGeneric("base_annotation", function(x) standardGeneric("base_annotation"))
setMethod("base_annotation", "SynFeatureAnnotation", function(x) x@base_annotation)

setGeneric("patches", function(x) standardGeneric("patches"))
setMethod("patches", "SynFeatureAnnotation", function(x) x@patches)

setGeneric("patch_name", function(x) standardGeneric("patch_name"))
setMethod("patch_name", "SynAnnotationPatch", function(x) x@name)

setGeneric("patch_mode", function(x) standardGeneric("patch_mode"))
setMethod("patch_mode", "SynAnnotationPatch", function(x) x@mode)

setGeneric("patch_target_ids", function(x) standardGeneric("patch_target_ids"))
setMethod("patch_target_ids", "SynAnnotationPatch", function(x) x@target_ids)

setGeneric("patch_data", function(x) standardGeneric("patch_data"))
setMethod("patch_data", "SynAnnotationPatch", function(x) x@patch_data)

setGeneric("label_map", function(x) standardGeneric("label_map"))
setMethod("label_map", "SynFeatureAnnotation", function(x) x@label_map)

setGeneric("label_map<-", function(x, value) standardGeneric("label_map<-"))
setReplaceMethod("label_map", "SynFeatureAnnotation", function(x, value) {
  x@label_map <- value
  validObject(x)
  x
})

#' Set human-readable gene labels on a feature annotation layer
#'
#' This keeps stable gene IDs for internal logic and adds a `plot_label`
#' metadata column for plotting.
#'
#' @param x A `SynFeatureAnnotation` or `SynIndividual` object.
#' @param mapping Either a named character vector (`feature_id -> label`) or a
#'   two-column data frame with ID and label columns.
#' @param annotation Optional feature-annotation layer name when `x` is a
#'   `SynIndividual`.
#'
#' @return The updated object.
#' @export
set_gene_labels <- function(x, mapping, annotation = NULL) {
  if (methods::is(x, "SynIndividual")) {
    ann_name <- if (is.null(annotation)) active_feature_annotation(x) else annotation
    ann <- get_annotation(x, ann_name)
    if (!methods::is(ann, "SynFeatureAnnotation")) {
      stop("`set_gene_labels()` requires a SynFeatureAnnotation layer.", call. = FALSE)
    }
    ann <- set_gene_labels(ann, mapping = mapping)
    x <- add_annotation(x, ann, set_active = identical(ann_name, active_feature_annotation(x)))
    return(x)
  }

  if (!methods::is(x, "SynFeatureAnnotation")) {
    stop(
      "`set_gene_labels()` expects a SynFeatureAnnotation or SynIndividual object.",
      call. = FALSE
    )
  }

  map_df <- .normalize_label_mapping(mapping)
  label_map(x) <- map_df

  if (!is.null(annotation_data(x))) {
    annotation_data(x) <- .apply_label_mapping(annotation_data(x), map_df)
  }

  x
}

#' Apply a gene-model patch to a feature annotation
#'
#' @param x A `SynFeatureAnnotation` or `SynIndividual` object.
#' @param patch A `SynAnnotationPatch`, `GRanges`, or patch-like data.
#' @param annotation Optional feature-annotation layer name when `x` is a
#'   `SynIndividual`.
#' @param target_ids Optional target gene IDs when `patch` is not already a
#'   `SynAnnotationPatch`.
#' @param mode One of `"replace"`, `"add"`, or `"drop"`.
#' @param name Optional patch label.
#'
#' @return The updated object.
#' @export
patch_annotation <- function(x,
                             patch,
                             annotation = NULL,
                             target_ids = NULL,
                             mode = c("replace", "add", "drop"),
                             name = NULL) {
  mode <- match.arg(mode)

  if (methods::is(x, "SynIndividual")) {
    ann_name <- if (is.null(annotation)) active_feature_annotation(x) else annotation
    ann <- get_annotation(x, ann_name)
    if (!methods::is(ann, "SynFeatureAnnotation")) {
      stop("`patch_annotation()` requires a SynFeatureAnnotation layer.", call. = FALSE)
    }
    ann <- patch_annotation(
      ann,
      patch = patch,
      target_ids = target_ids,
      mode = mode,
      name = name
    )
    x <- add_annotation(x, ann, set_active = identical(ann_name, active_feature_annotation(x)))
    return(x)
  }

  if (!methods::is(x, "SynFeatureAnnotation")) {
    stop(
      "`patch_annotation()` expects a SynFeatureAnnotation or SynIndividual object.",
      call. = FALSE
    )
  }

  patch_obj <- .as_annotation_patch(
    patch = patch,
    target_ids = target_ids,
    mode = mode,
    name = name
  )

  if (is.null(base_annotation(x))) {
    x@base_annotation <- annotation_data(x)
  }
  if (is.null(base_annotation(x))) {
    stop("Load the base annotation before applying patches.", call. = FALSE)
  }

  x@patches[[patch_name(patch_obj)]] <- patch_obj
  x@annotation <- .apply_annotation_patches(base_annotation(x), x@patches)
  if (!is.null(label_map(x))) {
    x@annotation <- .apply_label_mapping(x@annotation, label_map(x))
  }

  x@feature_index <- NULL
  x@nucleotide_seq <- NULL
  x@protein_seq <- NULL
  x@plot_cache <- list()
  x@loaded <- !is.null(x@annotation)
  validObject(x)
  x
}

#' List annotation patches
#'
#' @param x A `SynFeatureAnnotation` or `SynIndividual` object.
#' @param annotation Optional feature-annotation layer name when `x` is a
#'   `SynIndividual`.
#'
#' @return A named list of `SynAnnotationPatch` objects.
#' @export
list_patches <- function(x, annotation = NULL) {
  if (methods::is(x, "SynIndividual")) {
    ann_name <- if (is.null(annotation)) active_feature_annotation(x) else annotation
    return(list_patches(get_annotation(x, ann_name)))
  }
  if (!methods::is(x, "SynFeatureAnnotation")) {
    stop("`list_patches()` expects a SynFeatureAnnotation or SynIndividual object.", call. = FALSE)
  }
  patches(x)
}

#' Clear annotation patches
#'
#' @param x A `SynFeatureAnnotation` or `SynIndividual` object.
#' @param annotation Optional feature-annotation layer name when `x` is a
#'   `SynIndividual`.
#'
#' @return The updated object.
#' @export
clear_patches <- function(x, annotation = NULL) {
  if (methods::is(x, "SynIndividual")) {
    ann_name <- if (is.null(annotation)) active_feature_annotation(x) else annotation
    ann <- clear_patches(get_annotation(x, ann_name))
    x <- add_annotation(x, ann, set_active = identical(ann_name, active_feature_annotation(x)))
    return(x)
  }
  if (!methods::is(x, "SynFeatureAnnotation")) {
    stop("`clear_patches()` expects a SynFeatureAnnotation or SynIndividual object.", call. = FALSE)
  }
  x@patches <- list()
  x@annotation <- x@base_annotation
  if (!is.null(label_map(x)) && !is.null(x@annotation)) {
    x@annotation <- .apply_label_mapping(x@annotation, label_map(x))
  }
  x@feature_index <- NULL
  x@nucleotide_seq <- NULL
  x@protein_seq <- NULL
  x@plot_cache <- list()
  x
}

#' Read a small GFF/GTF patch file
#'
#' Imports a patch fragment and normalizes it to the same metadata schema used
#' by feature annotations in `ggexon`.
#'
#' @param path Path to a small GFF or GTF patch file.
#' @param format One of `"auto"`, `"gff"`, or `"gtf"`. Currently used for
#'   validation and future extension.
#'
#' @return A normalized `GRanges` object suitable for `patch_annotation()`.
#' @export
read_patch_gff <- function(path, format = c("auto", "gff", "gtf")) {
  format <- match.arg(format)
  if (!file.exists(path)) {
    stop("Patch file does not exist: ", path, call. = FALSE)
  }

  gr <- rtracklayer::import(path)
  gr <- .normalize_annotation(gr)
  attr(gr, "patch_format") <- format
  gr
}

#' Apply a patch directly from a small GFF/GTF file
#'
#' @param x A `SynFeatureAnnotation` or `SynIndividual` object.
#' @param patch_file Path to the patch GFF/GTF file.
#' @param annotation Optional feature-annotation layer name when `x` is a
#'   `SynIndividual`.
#' @param target_ids Optional target gene IDs. When omitted, target IDs are
#'   inferred from the patch file.
#' @param mode One of `"replace"`, `"add"`, or `"drop"`.
#' @param name Optional patch label.
#' @param format One of `"auto"`, `"gff"`, or `"gtf"`.
#'
#' @return The updated object.
#' @export
patch_annotation_from_gff <- function(x,
                                      patch_file,
                                      annotation = NULL,
                                      target_ids = NULL,
                                      mode = c("replace", "add", "drop"),
                                      name = NULL,
                                      format = c("auto", "gff", "gtf")) {
  mode <- match.arg(mode)
  format <- match.arg(format)

  patch_gr <- if (mode == "drop") NULL else read_patch_gff(patch_file, format = format)
  if (is.null(target_ids) && !is.null(patch_gr)) {
    target_ids <- unique(.extract_patch_target_ids(patch_gr))
  }

  patch_name_value <- if (is.null(name)) {
    paste0(tools::file_path_sans_ext(basename(patch_file)), "-", mode)
  } else {
    name
  }

  patch_obj <- SynAnnotationPatch(
    name = patch_name_value,
    patch_data = patch_gr,
    target_ids = target_ids %||% character(),
    mode = mode,
    metadata = list(source_file = patch_file, format = format)
  )

  patch_annotation(
    x,
    patch = patch_obj,
    annotation = annotation
  )
}

.normalize_label_mapping <- function(mapping) {
  if (is.character(mapping) && !is.null(names(mapping))) {
    return(.label_mapping_df(names(mapping), unname(as.character(mapping))))
  }

  if (is.data.frame(mapping) || methods::is(mapping, "DataFrame")) {
    if (ncol(mapping) < 2L) {
      stop("`mapping` data frame must contain at least two columns.", call. = FALSE)
    }
    mapping <- as.data.frame(mapping)
    id_col <- .label_mapping_column(
      mapping,
      candidates = c("feature_id", "gene_id", "ID", "id", "feature", "gene"),
      fallback = 1L
    )
    label_col <- .label_mapping_column(
      mapping,
      candidates = c("label", "plot_label", "gene_label", "gene_name", "name"),
      fallback = seq_len(ncol(mapping)),
      exclude = id_col
    )
    return(.label_mapping_df(mapping[[id_col]], mapping[[label_col]]))
  }

  stop(
    "`mapping` must be a named character vector or a two-column data frame.",
    call. = FALSE
  )
}

.label_mapping_column <- function(mapping,
                                  candidates,
                                  fallback,
                                  exclude = integer()) {
  nms <- names(mapping)
  if (!is.null(nms)) {
    candidate_idx <- match(to_lower_ascii(candidates), to_lower_ascii(nms))
    candidate_idx <- candidate_idx[!is.na(candidate_idx)]
    candidate_idx <- setdiff(candidate_idx, exclude)
    if (length(candidate_idx) > 0L) {
      return(candidate_idx[[1L]])
    }
  }
  fallback <- fallback[fallback %in% seq_len(ncol(mapping))]
  fallback <- setdiff(fallback, exclude)
  if (length(fallback) > 0L) {
    return(fallback[[1L]])
  }
  stop("`mapping` data frame must contain distinct ID and label columns.", call. = FALSE)
}

.label_mapping_df <- function(feature_id, label) {
  feature_id <- as.character(feature_id)
  label <- as.character(label)
  if (length(feature_id) != length(label)) {
    stop("`mapping` feature IDs and labels must have the same length.", call. = FALSE)
  }
  invalid_id <- is.na(feature_id) | !nzchar(feature_id)
  if (any(invalid_id)) {
    stop("`mapping` feature IDs must be non-empty and non-missing.", call. = FALSE)
  }
  invalid_label <- is.na(label) | !nzchar(label)
  if (any(invalid_label)) {
    stop("`mapping` labels must be non-empty and non-missing.", call. = FALSE)
  }
  duplicated_id <- unique(feature_id[duplicated(feature_id)])
  if (length(duplicated_id) > 0L) {
    stop(
      "`mapping` feature IDs must be unique: ",
      paste(utils::head(duplicated_id, 5L), collapse = ", "),
      call. = FALSE
    )
  }
  S4Vectors::DataFrame(feature_id = feature_id, label = label)
}

.apply_label_mapping <- function(gr, mapping) {
  map_ids <- as.character(mapping$feature_id)
  map_labels <- as.character(mapping$label)
  valid <- !is.na(map_ids) & nzchar(map_ids) & !is.na(map_labels) & nzchar(map_labels)
  map_ids <- map_ids[valid]
  map_labels <- map_labels[valid]

  meta <- S4Vectors::mcols(gr)
  plot_label <- if ("plot_label" %in% colnames(meta)) {
    as.character(meta$plot_label)
  } else {
    rep(NA_character_, length(gr))
  }

  id_candidates <- c("gene_id", "gene_name", "ID", "Parent", "transcript_id")
  candidate_values <- lapply(
    id_candidates[id_candidates %in% colnames(meta)],
    function(col) as.character(meta[[col]])
  )

  for (i in seq_along(map_ids)) {
    hit <- rep(FALSE, length(gr))
    for (values in candidate_values) {
      hit <- hit | (!is.na(values) & values == map_ids[[i]])
    }
    plot_label[hit] <- map_labels[[i]]
  }

  gene_name <- if ("gene_name" %in% colnames(meta)) as.character(meta$gene_name) else rep(NA_character_, length(gr))
  gene_id <- if ("gene_id" %in% colnames(meta)) as.character(meta$gene_id) else rep(NA_character_, length(gr))
  id_col <- if ("ID" %in% colnames(meta)) as.character(meta$ID) else rep(NA_character_, length(gr))
  fallback <- gene_name
  fallback[is.na(fallback) | !nzchar(fallback)] <- gene_id[is.na(fallback) | !nzchar(fallback)]
  fallback[is.na(fallback) | !nzchar(fallback)] <- id_col[is.na(fallback) | !nzchar(fallback)]
  plot_label[is.na(plot_label) | !nzchar(plot_label)] <- fallback[is.na(plot_label) | !nzchar(plot_label)]

  meta$plot_label <- plot_label
  S4Vectors::mcols(gr) <- meta
  gr
}

.as_annotation_patch <- function(patch, target_ids = NULL, mode = "replace", name = NULL) {
  if (methods::is(patch, "SynAnnotationPatch")) {
    return(patch)
  }
  patch_name_value <- if (is.null(name)) paste0("patch_", format(Sys.time(), "%Y%m%d%H%M%S")) else name
  patch_data_value <- if (methods::is(patch, "GRanges")) patch else NULL
  if (!is.null(patch_data_value) && is.null(target_ids)) {
    target_ids <- unique(.extract_patch_target_ids(patch_data_value))
  }
  SynAnnotationPatch(
    name = patch_name_value,
    patch_data = patch_data_value,
    target_ids = target_ids %||% character(),
    mode = mode
  )
}

.apply_annotation_patches <- function(base_gr, patch_list) {
  out <- base_gr
  if (length(patch_list) == 0L) {
    return(out)
  }
  for (patch in patch_list) {
    targets <- unique(as.character(patch_target_ids(patch)))
    targets <- targets[!is.na(targets) & nzchar(targets)]
    patch_mode_value <- patch_mode(patch)
    patch_gr <- patch_data(patch)

    if (patch_mode_value %in% c("replace", "drop")) {
      drop_idx <- rep(FALSE, length(out))
      if (length(targets) > 0L) {
        drop_idx <- drop_idx | .match_gene_targets(out, targets)
      }
      if (patch_mode_value == "replace" && !is.null(patch_gr) && length(patch_gr) > 0L) {
        drop_idx <- drop_idx | .match_patch_overlaps(out, patch_gr)
      }
      out <- out[!drop_idx]
    }
    if (patch_mode_value %in% c("replace", "add")) {
      if (!is.null(patch_gr) && length(patch_gr) > 0L) {
        out <- c(out, patch_gr)
      }
    }
  }
  .sort_annotation_ranges(out)
}

.match_gene_targets <- function(gr, target_ids) {
  meta <- S4Vectors::mcols(gr)
  candidates <- c("gene_id", "gene_name", "ID", "Parent", "transcript_id")
  hit <- rep(FALSE, length(gr))
  for (col in candidates[candidates %in% colnames(meta)]) {
    values <- as.character(meta[[col]])
    hit <- hit | (!is.na(values) & values %in% target_ids)
  }
  hit
}

.match_patch_overlaps <- function(gr, patch_gr) {
  if (length(gr) == 0L || is.null(patch_gr) || length(patch_gr) == 0L) {
    return(rep(FALSE, length(gr)))
  }
  query_seqnames <- as.character(GenomeInfoDb::seqnames(gr))
  subject_seqnames <- unique(as.character(GenomeInfoDb::seqnames(patch_gr)))
  same_seqname <- query_seqnames %in% subject_seqnames
  hit <- rep(FALSE, length(gr))
  if (!any(same_seqname)) {
    return(hit)
  }

  hit[same_seqname] <- IRanges::overlapsAny(
    gr[same_seqname],
    patch_gr,
    ignore.strand = TRUE
  )
  hit
}

.extract_patch_target_ids <- function(gr) {
  meta <- S4Vectors::mcols(gr)
  type <- if ("type" %in% colnames(meta)) as.character(meta$type) else rep(NA_character_, nrow(meta))
  gene_rows <- !is.na(type) & type == "gene"
  if (any(gene_rows)) {
    candidates <- c("gene_id", "gene_name", "ID")
    for (col in candidates[candidates %in% colnames(meta)]) {
      vals <- unique(as.character(meta[[col]][gene_rows]))
      vals <- vals[!is.na(vals) & nzchar(vals)]
      if (length(vals) > 0L) {
        return(vals)
      }
    }
  }
  candidates <- c("gene_id", "gene_name", "ID")
  for (col in candidates[candidates %in% colnames(meta)]) {
    vals <- unique(as.character(meta[[col]]))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) > 0L) {
      return(vals)
    }
  }
  character()
}

.sort_annotation_ranges <- function(gr) {
  if (length(gr) == 0L) {
    return(gr)
  }
  gr[order(as.character(GenomeInfoDb::seqnames(gr)), IRanges::start(gr), IRanges::end(gr))]
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Query variants from a SynVCFAnnotation
#'
#' For large tabix-backed VCF files, this function uses region-based access when
#' possible. Plain-text VCF files are read and filtered on demand.
#'
#' @param x A `SynVCFAnnotation` object.
#' @param chr Chromosome name.
#' @param start Start coordinate.
#' @param end End coordinate.
#'
#' @return A `S4Vectors::DataFrame` with the matching variant records.
#' @export
query_variants <- function(x, chr, start, end) {
  if (!methods::is(x, "SynVCFAnnotation")) {
    stop("`query_variants()` expects a SynVCFAnnotation object.", call. = FALSE)
  }

  cache_key <- paste(chr, start, end, sep = ":")
  if (cache_key %in% names(x@region_cache)) {
    return(x@region_cache[[cache_key]])
  }

  if (grepl("\\.gz$", source_file(x), ignore.case = TRUE) &&
      !is.na(x@index_file) && nzchar(x@index_file) &&
      file.exists(source_file(x)) && file.exists(x@index_file)) {
    param <- sprintf("%s:%s-%s", chr, start, end)
    vcf_lines <- Rsamtools::scanTabix(source_file(x), param = param)[[1L]]
    lines <- vcf_lines[!grepl("^#", vcf_lines)]
  } else {
    lines <- .read_delimited_annotation_lines(source_file(x), skip_pattern = "^##")
    lines <- lines[!grepl("^#", lines)]
    if (length(lines) > 0L) {
      fields <- strsplit(lines, "\t", fixed = TRUE)
      keep <- vapply(
        fields,
        function(rec) {
          length(rec) >= 2L &&
            identical(rec[[1L]], chr) &&
            as.integer(rec[[2L]]) >= start &&
            as.integer(rec[[2L]]) <= end
        },
        logical(1)
      )
      lines <- lines[keep]
    }
  }

  result <- .vcf_lines_to_dataframe(lines)
  x@region_cache[[cache_key]] <- result
  x@loaded <- TRUE
  result
}

#' Query signal from a SynBigWigAnnotation
#'
#' @param x A `SynBigWigAnnotation` object.
#' @param chr Chromosome name.
#' @param start Start coordinate.
#' @param end End coordinate.
#'
#' @return A `GRanges` object with the overlapping signal records.
#' @export
query_signal <- function(x, chr, start, end) {
  if (!methods::is(x, "SynBigWigAnnotation")) {
    stop("`query_signal()` expects a SynBigWigAnnotation object.", call. = FALSE)
  }

  cache_key <- paste(chr, start, end, sep = ":")
  if (cache_key %in% names(x@window_cache)) {
    return(x@window_cache[[cache_key]])
  }

  region <- GenomicRanges::GRanges(
    seqnames = chr,
    ranges = IRanges::IRanges(start = start, end = end)
  )
  signal <- rtracklayer::import.bw(source_file(x), which = region, as = "GRanges")

  x@window_cache[[cache_key]] <- signal
  x@signal <- signal
  x@loaded <- TRUE
  if (is.null(x@seqinfo) && length(signal) > 0L) {
    x@seqinfo <- GenomeInfoDb::seqinfo(signal)
  }
  signal
}

#' Query protein-domain annotations
#'
#' @param x A `SynProteinDomainAnnotation` object.
#' @param ids Optional identifiers to match against `keytype`.
#' @param domains Optional domain names to filter.
#'
#' @return A `S4Vectors::DataFrame` with matching domain records.
#' @export
query_domains <- function(x, ids = NULL, domains = NULL) {
  if (!methods::is(x, "SynProteinDomainAnnotation")) {
    stop(
      "`query_domains()` expects a SynProteinDomainAnnotation object.",
      call. = FALSE
    )
  }

  domain_data <- x@domain_data
  if (is.null(domain_data)) {
    domain_data <- .load_domain_table(source_file(x))
    x@domain_data <- domain_data
    x@loaded <- TRUE
  }

  out <- domain_data
  if (!is.null(ids)) {
    key_col <- x@keytype
    if (!key_col %in% colnames(out)) {
      stop(
        "The domain table does not contain the key column: ",
        key_col,
        call. = FALSE
      )
    }
    out <- out[as.character(out[[key_col]]) %in% as.character(ids), , drop = FALSE]
  }
  if (!is.null(domains)) {
    domain_col <- .pick_domain_column(out)
    out <- out[
      as.character(out[[domain_col]]) %in% as.character(domains),
      ,
      drop = FALSE
    ]
  }

  out
}

#' Rename protein-domain identifiers using an explicit mapping
#'
#' @param x A `SynProteinDomainAnnotation` or `SynIndividual` object.
#' @param mapping Either a named character vector (`old_id -> new_id`) or a
#'   two-column data frame with source and target identifier columns.
#' @param annotation Optional annotation-layer name when `x` is a
#'   `SynIndividual`.
#' @param from Source identifier column. Defaults to the domain annotation
#'   `keytype`.
#' @param to Target identifier column to populate. Defaults to
#'   `"transcript_id"`.
#' @param drop_unmapped Logical; when `TRUE`, drop domain rows that do not map
#'   to a target identifier.
#'
#' @return The updated object.
#' @export
rename_domain_annotation_ids <- function(x,
                                         mapping,
                                         annotation = NULL,
                                         from = NULL,
                                         to = "transcript_id",
                                         drop_unmapped = FALSE) {
  if (methods::is(x, "SynIndividual")) {
    ann_name <- annotation %||% NULL
    ann <- if (is.null(ann_name)) {
      ann_names <- annotation_names(x)
      hit <- ann_names[vapply(ann_names, function(name) {
        methods::is(get_annotation(x, name), "SynProteinDomainAnnotation")
      }, logical(1))]
      if (length(hit) == 0L) {
        stop(
          "No SynProteinDomainAnnotation layer is attached to this SynIndividual.",
          call. = FALSE
        )
      }
      hit[[1L]]
    } else {
      ann_name
    }

    updated_ann <- rename_domain_annotation_ids(
      get_annotation(x, ann),
      mapping = mapping,
      from = from,
      to = to,
      drop_unmapped = drop_unmapped
    )
    return(add_annotation(x, updated_ann))
  }

  if (!methods::is(x, "SynProteinDomainAnnotation")) {
    stop(
      "`rename_domain_annotation_ids()` expects a SynProteinDomainAnnotation or SynIndividual object.",
      call. = FALSE
    )
  }

  map_df <- .normalize_domain_id_mapping(mapping)
  from <- from %||% x@keytype

  if (!is.character(from) || length(from) != 1L || is.na(from) || !nzchar(from)) {
    stop("`from` must be a single non-empty character value.", call. = FALSE)
  }
  if (!is.character(to) || length(to) != 1L || is.na(to) || !nzchar(to)) {
    stop("`to` must be a single non-empty character value.", call. = FALSE)
  }

  domain_df <- query_domains(x)
  if (!from %in% colnames(domain_df)) {
    stop(
      "The domain table does not contain the source column: ",
      from,
      call. = FALSE
    )
  }

  source_vals <- as.character(domain_df[[from]])
  mapped_vals <- map_df$target_id[match(source_vals, map_df$source_id)]

  if (identical(from, to)) {
    domain_df[[to]] <- ifelse(
      !is.na(mapped_vals) & nzchar(mapped_vals),
      mapped_vals,
      source_vals
    )
  } else {
    domain_df[[to]] <- ifelse(
      !is.na(mapped_vals) & nzchar(mapped_vals),
      mapped_vals,
      NA_character_
    )
  }

  if (isTRUE(drop_unmapped)) {
    keep <- !is.na(domain_df[[to]]) & nzchar(as.character(domain_df[[to]]))
    domain_df <- domain_df[keep, , drop = FALSE]
  }

  x@domain_data <- domain_df
  x@keytype <- to
  x@loaded <- TRUE
  x@metadata$domain_id_mapping <- list(
    from = from,
    to = to,
    n_mapping = nrow(map_df),
    drop_unmapped = isTRUE(drop_unmapped)
  )

  validObject(x)
  x
}

.normalize_domain_id_mapping <- function(mapping) {
  if (is.character(mapping) && !is.null(names(mapping))) {
    return(
      S4Vectors::DataFrame(
        source_id = names(mapping),
        target_id = unname(as.character(mapping))
      )
    )
  }

  if (is.data.frame(mapping) || methods::is(mapping, "DataFrame")) {
    mapping <- as.data.frame(mapping, stringsAsFactors = FALSE)
    if (ncol(mapping) < 2L) {
      stop("Mapping data frame must have at least two columns.", call. = FALSE)
    }
    return(
      S4Vectors::DataFrame(
        source_id = as.character(mapping[[1L]]),
        target_id = as.character(mapping[[2L]])
      )
    )
  }

  stop(
    "`mapping` must be a named character vector or a two-column data frame.",
    call. = FALSE
  )
}

.read_delimited_annotation_lines <- function(path, skip_pattern = NULL) {
  con <- if (grepl("\\.gz$", path, ignore.case = TRUE)) gzfile(path, open = "rt") else file(path, open = "rt")
  on.exit(close(con), add = TRUE)
  lines <- readLines(con, warn = FALSE)
  if (!is.null(skip_pattern)) {
    lines <- lines[!grepl(skip_pattern, lines)]
  }
  lines[nzchar(lines)]
}

.vcf_lines_to_dataframe <- function(lines) {
  if (length(lines) == 0L) {
    return(S4Vectors::DataFrame(
      CHROM = character(),
      POS = integer(),
      ID = character(),
      REF = character(),
      ALT = character(),
      QUAL = character(),
      FILTER = character(),
      INFO = character()
    ))
  }

  fields <- strsplit(lines, "\t", fixed = TRUE)
  pad_record <- function(rec) {
    length(rec) <- max(length(rec), 8L)
    rec
  }
  fields <- lapply(fields, pad_record)

  S4Vectors::DataFrame(
    CHROM = vapply(fields, `[[`, character(1), 1L),
    POS = as.integer(vapply(fields, `[[`, character(1), 2L)),
    ID = vapply(fields, `[[`, character(1), 3L),
    REF = vapply(fields, `[[`, character(1), 4L),
    ALT = vapply(fields, `[[`, character(1), 5L),
    QUAL = vapply(fields, `[[`, character(1), 6L),
    FILTER = vapply(fields, `[[`, character(1), 7L),
    INFO = vapply(fields, `[[`, character(1), 8L)
  )
}

.load_domain_table <- function(path) {
  ext <- tools::file_ext(path)
  delim <- if (base::tolower(ext) %in% c("csv")) "," else "\t"

  if (.looks_like_interproscan_tsv(path, delim = delim)) {
    tbl <- readr::read_delim(
      file = path,
      delim = delim,
      col_names = FALSE,
      show_col_types = FALSE,
      progress = FALSE
    )
    tbl <- .normalize_interproscan_table(tbl)
  } else {
    tbl <- readr::read_delim(
      file = path,
      delim = delim,
      show_col_types = FALSE,
      progress = FALSE
    )
  }

  S4Vectors::DataFrame(tbl)
}

.pick_domain_column <- function(x) {
  candidates <- c(
    "domain",
    "interpro_accession",
    "signature_accession",
    "domain_name",
    "interpro_description",
    "signature_description",
    "pfam",
    "interpro",
    "name"
  )
  hit <- candidates[candidates %in% colnames(x)]
  if (length(hit) == 0L) {
    stop(
      "Could not identify a domain name column in the domain table.",
      call. = FALSE
    )
  }
  hit[[1L]]
}

.looks_like_interproscan_tsv <- function(path, delim = "\t") {
  first_line <- .read_delimited_annotation_lines(path)[1L]
  if (is.na(first_line) || !nzchar(first_line)) {
    return(FALSE)
  }

  fields <- strsplit(first_line, delim, fixed = TRUE)[[1L]]
  if (length(fields) != 15L) {
    return(FALSE)
  }

  has_known_header <- base::tolower(fields[[1L]]) %in% c("protein_id", "sequence_id", "accession")
  if (has_known_header) {
    return(FALSE)
  }

  grepl("interproscan", basename(path), ignore.case = TRUE) ||
    grepl("^[A-Za-z0-9_.:-]+$", fields[[1L]]) ||
    grepl("^[0-9a-f]{32}$", fields[[2L]], ignore.case = TRUE)
}

.normalize_interproscan_table <- function(tbl) {
  interpro_cols <- c(
    "protein_id",
    "sequence_md5",
    "protein_length",
    "analysis",
    "signature_accession",
    "signature_description",
    "start",
    "end",
    "score",
    "status",
    "analysis_date",
    "interpro_accession",
    "interpro_description",
    "go_terms",
    "pathways"
  )

  if (ncol(tbl) < length(interpro_cols)) {
    stop(
      "InterProScan table must contain at least 15 columns.",
      call. = FALSE
    )
  }

  colnames(tbl)[seq_along(interpro_cols)] <- interpro_cols
  tbl <- tibble::as_tibble(tbl)

  blankish <- function(x) {
    is.na(x) | x %in% c("", "-")
  }

  tbl$domain <- dplyr::if_else(
    !blankish(tbl$interpro_accession),
    tbl$interpro_accession,
    tbl$signature_accession,
    missing = tbl$signature_accession
  )
  tbl$domain_name <- dplyr::if_else(
    !blankish(tbl$interpro_description),
    tbl$interpro_description,
    tbl$signature_description,
    missing = tbl$signature_description
  )

  tbl
}
