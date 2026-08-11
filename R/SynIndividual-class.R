#' @include SynBioc-unions.R
NULL

#' SynIndividual class
#'
#' `SynIndividual` stores the per-individual data needed to build synteny plots.
#' Only `id` is required at the class level; genome and annotation file paths
#' can be attached later. Parsed annotations, nucleotide/protein sequences, and
#' plotting caches can also be attached through accessor methods.
#'
#' @slot id Scalar identifier for the species, genome, or plotting track.
#' @slot genome_file Path to the genome FASTA file, or `NA_character_` when the
#'   genome was waived during construction.
#' @slot annotation_file Path or paths to the corresponding GFF or GTF file(s).
#' @slot annotation_format One of `"gff"`, `"gtf"`, or `"auto"`, or a vector
#'   matching `annotation_file`.
#' @slot seqinfo Sequence-level metadata such as chromosome names and lengths
#'   stored as a `GenomeInfoDb::Seqinfo` object.
#' @slot annotations Named list of `SynAnnotation` objects attached to this
#'   genome.
#' @slot active_annotation Name of the default feature annotation layer to use.
#' @slot metadata User or import metadata describing the individual.
#' @slot plot_cache Derived plotting tables cached for reuse.
#' @slot projected_domains Named list of projected protein-domain tables stored
#'   for inspection.
#'
#' @section Prototype defaults:
#' * `annotation_format = "auto"`
#' * `seqinfo = NULL`
#' * `annotations = list()`
#' * `active_annotation = "default"`
#' * `metadata = list()`
#' * `plot_cache = list()`
#' * `projected_domains = list()`
#'
#' The raw class prototype leaves `annotations` empty. The high-level
#' [SynIndividual()] constructor usually populates that slot with a `"default"`
#' [SynFeatureAnnotation()] immediately.
#'
#' @section Validity rules:
#' * `id` and `active_annotation` must be scalar non-empty character values.
#' * `genome_file` must be a length-one character vector or `NA_character_`.
#' * When present, `annotation_file` must be a non-empty character vector with
#'   no empty entries.
#' * `annotation_format` must be one of `"auto"`, `"gff"`, or `"gtf"`. When
#'   `annotation_file` is present, it must be length one or the same length as
#'   `annotation_file`.
#' * When `annotations` is non-empty, every entry must inherit from
#'   `SynAnnotation`. When feature annotations are present,
#'   `active_annotation` must name one of those feature layers.
#' * `projected_domains` must be a list of data-frame-like objects.
#'
#' @exportClass SynIndividual
setClass(
  "SynIndividual",
  slots = c(
    id = "character",
    genome_file = "character",
    annotation_file = "character",
    annotation_format = "character",
    seqinfo = "ANY",
    annotations = "list",
    active_annotation = "character",
    metadata = "list",
    plot_cache = "list",
    projected_domains = "list"
  ),
  prototype = list(
    id = NA_character_,
    genome_file = NA_character_,
    annotation_file = NA_character_,
    annotation_format = "auto",
    seqinfo = NULL,
    annotations = list(),
    active_annotation = "default",
    metadata = list(),
    plot_cache = list(),
    projected_domains = list()
  ),
  validity = function(object) {
    problems <- character()

    if (length(object@id) != 1L || is.na(object@id) || !nzchar(object@id)) {
      problems <- c(problems, "`id` must be a single non-empty character value.")
    }
    if (length(object@genome_file) != 1L) {
      problems <- c(
        problems,
        "`genome_file` must be a single character value or `NA_character_`."
      )
    }
    has_annotation_file <- !(
      length(object@annotation_file) == 1L &&
        is.na(object@annotation_file)
    )
    if (has_annotation_file &&
        (length(object@annotation_file) == 0L ||
         any(is.na(object@annotation_file)) ||
         any(!nzchar(object@annotation_file)))) {
      problems <- c(
        problems,
        "`annotation_file` must be a non-empty character vector with no empty entries when supplied."
      )
    }
    if (any(!(object@annotation_format %in% c("auto", "gff", "gtf")))) {
      problems <- c(
        problems,
        "`annotation_format` must contain only 'auto', 'gff', or 'gtf'."
      )
    } else if (has_annotation_file &&
               !(length(object@annotation_format) %in% c(1L, length(object@annotation_file)))) {
      problems <- c(
        problems,
        "`annotation_format` must have length 1 or the same length as `annotation_file` when annotation files are supplied."
      )
    }
    if (length(object@active_annotation) != 1L ||
        is.na(object@active_annotation) ||
        !nzchar(object@active_annotation)) {
      problems <- c(
        problems,
        "`active_annotation` must be a single non-empty character value."
      )
    }
    if (!is.null(object@seqinfo) && !methods::is(object@seqinfo, "Seqinfo")) {
      problems <- c(problems, "`seqinfo` must be a Seqinfo object or NULL.")
    }
    if (length(object@annotations) > 0L) {
      bad_annotations <- !vapply(
        object@annotations,
        methods::is,
        logical(1),
        class2 = "SynAnnotation"
      )
      if (any(bad_annotations)) {
        problems <- c(
          problems,
          "`annotations` must be a list of SynAnnotation objects."
        )
      }
    }
    if (length(object@projected_domains) > 0L) {
      bad_projected <- !vapply(
        object@projected_domains,
        function(x) is.data.frame(x) || methods::is(x, "DataFrame"),
        logical(1)
      )
      if (any(bad_projected)) {
        problems <- c(
          problems,
          "`projected_domains` must be a list of data.frame-like tables."
        )
      }
    }

    if (length(problems) == 0L) TRUE else problems
  }
)

#' Constructor for SynIndividual
#'
#' @param genome_file Path to the genome FASTA file. Use `genome_waiver()` to
#'   initialize a `SynIndividual` without a genome FASTA.
#' @param annotation_file Optional path or paths to the corresponding GFF or
#'   GTF file(s).
#' @param id Optional scalar identifier. Defaults to the FASTA stem, or to the
#'   first annotation-file stem when `genome_file` is waived. Required when
#'   neither file input is supplied.
#' @param annotation_format One of `"auto"`, `"gff"`, or `"gtf"`, or a vector
#'   of the same length as `annotation_file`.
#' @param metadata Optional metadata list.
#'
#' @return A `SynIndividual` object with deferred slots left empty.
#' @export
SynIndividual <- function(genome_file = genome_waiver(),
                          annotation_file = NA_character_,
                          id = NULL,
                          annotation_format = "auto",
                          metadata = list()) {
  genome_file <- .normalize_genome_file_input(genome_file)
  has_annotation_file <- !missing(annotation_file) &&
    !(length(annotation_file) == 1L && is.character(annotation_file) && is.na(annotation_file))

  if (has_annotation_file) {
    annotation_file <- .normalize_annotation_file_input(annotation_file)
    annotation_format <- .normalize_annotation_format_input(annotation_format, annotation_file)
  } else {
    annotation_file <- NA_character_
    annotation_format <- .normalize_annotation_format_absent(annotation_format)
  }

  if (.has_genome_file(genome_file) && has_annotation_file) {
    check_syn_files(
      genome_file = genome_file,
      annotation_file = annotation_file
    )
  } else if (has_annotation_file) {
    .check_annotation_file(annotation_file)
  }

  if (is.null(id)) {
    if (.has_genome_file(genome_file)) {
      id <- .path_stem(genome_file)
    } else if (has_annotation_file) {
      id <- .path_stem(annotation_file[[1L]])
    } else {
      stop(
        "`id` must be supplied when neither `genome_file` nor `annotation_file` is provided.",
        call. = FALSE
      )
    }
  }

  annotations <- list()
  if (has_annotation_file) {
    default_annotation <- SynFeatureAnnotation(
      name = "default",
      annotation_file = annotation_file,
      annotation_format = annotation_format
    )
    annotations <- list(default = default_annotation)
  }

  new(
    "SynIndividual",
    id = id,
    genome_file = genome_file,
    annotation_file = annotation_file,
    annotation_format = annotation_format,
    annotations = annotations,
    active_annotation = "default",
    metadata = metadata
  )
}

.path_stem <- function(path) {
  stem <- basename(path)
  stem <- sub("\\.gz$", "", stem, ignore.case = TRUE)
  tools::file_path_sans_ext(stem)
}

.normalize_annotation_file_input <- function(annotation_file) {
  if (!is.character(annotation_file) || length(annotation_file) == 0L) {
    stop(
      "`annotation_file` must be a non-empty character vector.",
      call. = FALSE
    )
  }
  annotation_file <- as.character(annotation_file)
  if (any(is.na(annotation_file)) || any(!nzchar(annotation_file))) {
    stop(
      "`annotation_file` must not contain missing or empty paths.",
      call. = FALSE
    )
  }
  unname(annotation_file)
}

.normalize_annotation_format_input <- function(annotation_format, annotation_file) {
  allowed <- c("auto", "gff", "gtf")
  if (missing(annotation_format) || is.null(annotation_format)) {
    return(rep("auto", length(annotation_file)))
  }

  annotation_format <- as.character(annotation_format)
  if (!(length(annotation_format) %in% c(1L, length(annotation_file)))) {
    stop(
      "`annotation_format` must have length 1 or the same length as `annotation_file`.",
      call. = FALSE
    )
  }
  if (any(is.na(annotation_format)) || any(!(annotation_format %in% allowed))) {
    stop(
      "`annotation_format` must contain only 'auto', 'gff', or 'gtf'.",
      call. = FALSE
    )
  }
  if (length(annotation_format) == 1L) {
    rep(annotation_format, length(annotation_file))
  } else {
    annotation_format
  }
}

.normalize_annotation_format_absent <- function(annotation_format) {
  allowed <- c("auto", "gff", "gtf")
  if (missing(annotation_format) || is.null(annotation_format)) {
    return("auto")
  }

  annotation_format <- as.character(annotation_format)
  if (length(annotation_format) != 1L) {
    stop(
      "`annotation_format` must have length 1 when `annotation_file` is not supplied.",
      call. = FALSE
    )
  }
  if (is.na(annotation_format) || !(annotation_format %in% allowed)) {
    stop(
      "`annotation_format` must be one of 'auto', 'gff', or 'gtf'.",
      call. = FALSE
    )
  }

  annotation_format
}

#' Genome-file waiver for `SynIndividual()`
#'
#' Use this helper when you want to initialize a `SynIndividual` from
#' annotations only, without an available genome FASTA. Sequence-dependent
#' operations such as `extract_cds_seq()` and `translate_protein()` will then
#' stop with a clear error message.
#'
#' @return A sentinel value understood by `SynIndividual()`.
#' @export
genome_waiver <- function() {
  waiver()
}

#' Check whether genome and annotation files match
#'
#' Validates that every sequence name in the first column of the annotation
#' file is present among the FASTA headers. FASTA names are compared using the
#' first token after `>`, which matches standard GFF/GTF `seqname` usage.
#'
#' @param genome_file Path to the genome FASTA file.
#' @param annotation_file Path to the corresponding GFF or GTF file.
#'
#' @return Invisibly returns `TRUE` when the files match.
#' @export
check_syn_files <- function(genome_file, annotation_file) {
  if (!is.character(genome_file) || length(genome_file) != 1L ||
      is.na(genome_file) || !nzchar(genome_file)) {
    stop("`genome_file` must be a single non-empty character value.", call. = FALSE)
  }
  annotation_file <- .normalize_annotation_file_input(annotation_file)
  if (!file.exists(genome_file)) {
    stop("Genome FASTA file does not exist: ", genome_file, call. = FALSE)
  }
  missing_files <- annotation_file[!file.exists(annotation_file)]
  if (length(missing_files) > 0L) {
    stop("Annotation file does not exist: ", missing_files[[1L]], call. = FALSE)
  }
  non_files <- annotation_file[!file_test("-f", annotation_file)]
  if (length(non_files) > 0L) {
    stop("Annotation file is not a regular file: ", non_files[[1L]], call. = FALSE)
  }

  fasta_headers <- .read_fasta_headers(genome_file)
  annotation_chr <- .read_annotation_seqnames(annotation_file)

  if (length(fasta_headers) == 0L) {
    stop("No FASTA headers were found in: ", genome_file, call. = FALSE)
  }
  if (length(annotation_chr) == 0L) {
    stop(
      "No sequence names were found in the first column of: ",
      annotation_file,
      call. = FALSE
    )
  }

  missing_chr <- setdiff(annotation_chr, fasta_headers)
  if (length(missing_chr) > 0L) {
    stop(
      paste0(
        "Annotation and genome files do not match. The following annotation ",
        "sequence names are missing from the FASTA headers: ",
        paste(utils::head(missing_chr, 10L), collapse = ", "),
        if (length(missing_chr) > 10L) " ..." else ""
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.normalize_genome_file_input <- function(genome_file) {
  if (missing(genome_file) || is.null(genome_file) || is.waive(genome_file)) {
    return(NA_character_)
  }
  if (!is.character(genome_file) || length(genome_file) != 1L) {
    stop(
      "`genome_file` must be a single file path or `genome_waiver()`.",
      call. = FALSE
    )
  }
  if (is.na(genome_file) || !nzchar(genome_file)) {
    return(NA_character_)
  }
  genome_file
}

.has_genome_file <- function(genome_file) {
  is.character(genome_file) &&
    length(genome_file) == 1L &&
    !is.na(genome_file) &&
    nzchar(genome_file)
}

.check_annotation_file <- function(annotation_file) {
  annotation_file <- .normalize_annotation_file_input(annotation_file)
  missing_files <- annotation_file[!file.exists(annotation_file)]
  if (length(missing_files) > 0L) {
    stop("Annotation file does not exist: ", missing_files[[1L]], call. = FALSE)
  }
  non_files <- annotation_file[!file_test("-f", annotation_file)]
  if (length(non_files) > 0L) {
    stop("Annotation file is not a regular file: ", non_files[[1L]], call. = FALSE)
  }
  invisible(TRUE)
}

.require_genome_file <- function(x, call = "This operation") {
  path <- if (methods::is(x, "SynIndividual")) genome_file(x) else x
  if (.has_genome_file(path)) {
    return(path)
  }
  stop(
    call,
    " requires a genome FASTA, but `genome_file` was waived when the SynIndividual was created.",
    call. = FALSE
  )
}

#' Load annotations into Syn-aware objects
#'
#' Imports the annotation file as a `GRanges` object and stores it in the
#' active feature-annotation slots. The imported ranges are lightly normalized
#' so downstream query and translation methods can use consistent metadata
#' columns.
#'
#' When `x` is a [`SynSpecies`], the helper loads the active feature annotation
#' for every stored [`SynIndividual`] and returns an updated `SynSpecies`
#' object. Pairwise alignments, multiple alignments, metadata, and layout are
#' left unchanged.
#'
#' @param x A `SynIndividual`, `SynFeatureAnnotation`, or `SynSpecies` object.
#' @param annotation Optional annotation-layer name when `x` is a
#'   `SynIndividual` or `SynSpecies`. Defaults to the active feature annotation
#'   for the selected individual.
#' @param individual Optional individual name when `x` is a `SynSpecies`. When
#'   omitted, all stored individuals are updated.
#'
#' @details
#' This is an S4 generic that dispatches on the class of `x`.
#'
#' @return An updated object of the same class as `x`.
#'
#' @examples
#' ann_path <- system.file(
#'   "extdata",
#'   "caenorhabditis_XZ1516.gff3",
#'   package = "ggexon"
#' )
#'
#' ann <- SynFeatureAnnotation(name = "default", annotation_file = ann_path)
#' ann <- load_annotation(ann)
#'
#' ind <- SynIndividual(
#'   annotation_file = ann_path,
#'   genome_file = genome_waiver(),
#'   id = "XZ1516"
#' )
#' ind <- load_annotation(ind)
#'
#' sp <- SynSpecies(name = "worms") |> add_individual(ind)
#' sp <- load_annotation(sp)
#'
#' @export
setGeneric("load_annotation", function(x, annotation = NULL, individual = NULL) {
  standardGeneric("load_annotation")
})

#' @rdname load_annotation
setMethod("load_annotation", "SynFeatureAnnotation", function(x, annotation = NULL, individual = NULL) {
  if (!is.null(annotation_data(x))) {
    return(x)
  }
  gr <- .import_all_annotation_files(x)
  annotation_data(x) <- .normalize_annotation(gr)
  x@base_annotation <- annotation_data(x)
  x@loaded <- TRUE
  x
})

#' @rdname load_annotation
setMethod("load_annotation", "SynAnnotation", function(x, annotation = NULL, individual = NULL) {
  stop(
    "`load_annotation()` currently supports SynFeatureAnnotation objects.",
    call. = FALSE
  )
})

#' @rdname load_annotation
setMethod("load_annotation", "SynIndividual", function(x, annotation = NULL, individual = NULL) {
  ann_name <- annotation %||% active_feature_annotation(x)
  ann <- get_annotation(x, ann_name)
  ann <- load_annotation(ann)
  x <- add_annotation(x, ann, set_active = identical(ann_name, active_feature_annotation(x)))

  active_ann <- get_annotation(x, active_feature_annotation(x))
  seqinfo(x) <- if (!is.null(annotation_data(active_ann))) {
    GenomeInfoDb::seqinfo(annotation_data(active_ann))
  } else {
    NULL
  }
  x
})

.resolve_seqname_value <- function(chr, available, label = "annotation") {
  if (is.null(chr)) {
    return(NULL)
  }
  if (!is.character(chr) || length(chr) != 1L || is.na(chr) || !nzchar(chr)) {
    stop("`chr` must be a single non-empty character value.", call. = FALSE)
  }

  available <- unique(as.character(available))
  available <- available[!is.na(available) & nzchar(available)]
  if (length(available) == 0L) {
    stop("The ", label, " does not contain any sequence names.", call. = FALSE)
  }

  if (chr %in% available) {
    return(chr)
  }

  lower_available <- base::tolower(available)
  lower_chr <- base::tolower(chr)
  if (lower_chr %in% lower_available) {
    return(available[match(lower_chr, lower_available)])
  }

  stripped_chr <- sub("^chr", "", lower_chr)
  stripped_available <- sub("^chr", "", lower_available)
  prefix_matches <- which(stripped_available == stripped_chr)
  if (length(prefix_matches) == 1L) {
    return(available[[prefix_matches]])
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
    "Unknown chromosome ", chr, " for ", label,
    ". Available seqnames include: ",
    paste(utils::head(available, 10L), collapse = ", "),
    call. = FALSE
  )
}

.resolve_annotation_seqname <- function(x, chr) {
  if (is.null(chr)) {
    return(NULL)
  }

  label <- if (methods::is(x, "SynIndividual")) {
    syn_id(x)
  } else if (methods::is(x, "SynAnnotation")) {
    annotation_name(x)
  } else {
    "annotation"
  }

  available <- if (!is.null(annotation_data(x))) {
    unique(as.character(GenomeInfoDb::seqnames(annotation_data(x))))
  } else {
    .read_annotation_seqnames(annotation_file(x))
  }

  .resolve_seqname_value(chr = chr, available = available, label = label)
}

.import_annotation_window <- function(x,
                                      chr,
                                      start = NULL,
                                      end = NULL,
                                      feature_types = NULL,
                                      colnames = NULL) {
  resolved_chr <- .resolve_annotation_seqname(x, chr)
  if (is.null(start)) {
    start <- 1L
  }
  if (is.null(end)) {
    end <- .Machine$integer.max
  }
  if (!is.numeric(start) || length(start) != 1L || is.na(start)) {
    stop("`start` must be a single numeric value or NULL.", call. = FALSE)
  }
  if (!is.numeric(end) || length(end) != 1L || is.na(end)) {
    stop("`end` must be a single numeric value or NULL.", call. = FALSE)
  }

  window_start <- as.integer(min(start, end))
  window_end <- as.integer(max(start, end))
  which <- GenomicRanges::GRanges(
    seqnames = resolved_chr,
    ranges = IRanges::IRanges(start = window_start, end = window_end)
  )

  import_colnames <- if (is.null(colnames)) NULL else unique(colnames)
  annotation_paths <- annotation_file(x)
  annotation_formats <- .annotation_import_format(x)

  imported <- lapply(seq_along(annotation_paths), function(i) {
    annotation_path <- annotation_paths[[i]]
    annotation_format <- annotation_formats[[i]]

    if (.is_indexed_annotation_file(annotation_path)) {
      tabix <- Rsamtools::TabixFile(
        annotation_path,
        index = .annotation_index_path(annotation_path)
      )
      open(tabix)
      on.exit(try(close(tabix), silent = TRUE), add = TRUE)

      .quiet_indexed_annotation_import(
        rtracklayer::import(
          tabix,
          format = annotation_format,
          which = which,
          feature.type = feature_types,
          colnames = import_colnames
        )
      )
    } else {
      rtracklayer::import(
        rtracklayer::GFFFile(annotation_path),
        which = which,
        feature.type = feature_types,
        colnames = import_colnames
      )
    }
  })
  gr <- .combine_annotation_granges(imported)

  list(
    gr = .normalize_annotation(gr),
    chr = resolved_chr,
    start = window_start,
    end = window_end
  )
}

.query_import_feature_types <- function(feature_type = "CDS",
                                        genes = NULL,
                                        transcripts = NULL) {
  if (!is.null(genes)) {
    return(NULL)
  }
  if (is.null(feature_type)) {
    return(NULL)
  }
  unique(as.character(feature_type))
}

.build_row_lookup_index <- function(keys, rows = seq_along(keys)) {
  valid <- !is.na(keys) & nzchar(keys)
  if (!any(valid)) {
    return(list())
  }

  split_rows <- split(as.integer(rows[valid]), keys[valid])
  lapply(split_rows, function(x) sort(unique(as.integer(x))))
}

.build_meta_lookup_index <- function(meta, columns) {
  columns <- intersect(columns, colnames(meta))
  if (length(columns) == 0L) {
    return(list())
  }

  row_ids <- seq_len(nrow(meta))
  key_rows <- lapply(columns, function(col) {
    values <- as.character(meta[[col]])
    valid <- !is.na(values) & nzchar(values)
    if (!any(valid)) {
      return(NULL)
    }
    data.frame(
      key = values[valid],
      row = row_ids[valid],
      stringsAsFactors = FALSE
    )
  })
  key_rows <- Filter(Negate(is.null), key_rows)
  if (length(key_rows) == 0L) {
    return(list())
  }

  key_rows <- do.call(rbind, key_rows)
  split_rows <- split(key_rows$row, key_rows$key)
  lapply(split_rows, function(x) sort(unique(as.integer(x))))
}

.build_annotation_feature_index <- function(gr) {
  meta <- S4Vectors::mcols(gr)
  list(
    seqname = .build_row_lookup_index(as.character(GenomeInfoDb::seqnames(gr))),
    type = .build_row_lookup_index(base::tolower(as.character(meta$type))),
    gene = .build_meta_lookup_index(
      meta,
      c("gene_name", "gene_id", "Name", "gene", "ID")
    ),
    transcript = .build_meta_lookup_index(
      meta,
      c("transcript_id", "Parent", "transcript_name", "ID")
    ),
    parent = .build_meta_lookup_index(meta, c("Parent"))
  )
}

.lookup_feature_index_rows <- function(index, slot, keys) {
  keys <- unique(as.character(keys))
  keys <- keys[!is.na(keys) & nzchar(keys)]
  if (length(keys) == 0L) {
    return(integer())
  }

  lookup <- index[[slot]]
  if (is.null(lookup) || length(lookup) == 0L) {
    return(integer())
  }

  matched_keys <- intersect(keys, names(lookup))
  if (length(matched_keys) == 0L) {
    return(integer())
  }

  sort(unique(as.integer(unlist(lookup[matched_keys], use.names = FALSE))))
}

#' Build a reusable feature lookup index
#'
#' Builds and stores a lookup index over a loaded feature annotation so repeated
#' calls to [query_features()] can resolve seqname, feature-type, gene, and
#' transcript filters without rescanning the full `GRanges` each time.
#'
#' @param x A `SynIndividual` or `SynFeatureAnnotation` object.
#'
#' @return The updated object.
#' @export
build_feature_index <- function(x) {
  if (!(methods::is(x, "SynIndividual") || methods::is(x, "SynFeatureAnnotation"))) {
    stop(
      "`build_feature_index()` expects a SynIndividual or SynFeatureAnnotation object.",
      call. = FALSE
    )
  }

  x <- load_annotation(x)
  if (!is.null(feature_index(x))) {
    return(x)
  }

  feature_index(x) <- .build_annotation_feature_index(annotation_data(x))
  x
}

#' Query annotation features from a SynIndividual object
#'
#' @param x A `SynIndividual` or `SynFeatureAnnotation` object.
#' @param genes Optional character vector of gene names or gene identifiers.
#' @param transcripts Optional character vector of transcript identifiers.
#' @param chr Optional chromosome name.
#' @param start Optional start coordinate.
#' @param end Optional end coordinate.
#' @param feature_type Feature type to return. Defaults to `"CDS"`.
#' @param all Logical; when `TRUE`, return all matching `feature_type` records.
#'
#' @details
#' When `chr` is supplied and the annotation has not yet been fully loaded,
#' `query_features()` attempts a region-restricted import first. This is
#' especially helpful for large indexed `.gff3.gz` or `.gtf.gz` files.
#'
#' If a reusable lookup index has been created with [build_feature_index()],
#' `query_features()` uses it to accelerate repeated gene-, transcript-,
#' seqname-, and type-based filtering on fully loaded annotations.
#'
#' @return A `GRanges` object containing the requested features.
#' @export
query_features <- function(x,
                           genes = NULL,
                           transcripts = NULL,
                           chr = NULL,
                           start = NULL,
                           end = NULL,
                           feature_type = "CDS",
                           all = FALSE) {
  if (!(methods::is(x, "SynIndividual") || methods::is(x, "SynFeatureAnnotation"))) {
    stop(
      "`query_features()` expects a SynIndividual or SynFeatureAnnotation object.",
      call. = FALSE
    )
  }

  use_region_import <- is.null(annotation_data(x)) && !is.null(chr)
  if (use_region_import) {
    imported <- .import_annotation_window(
      x,
      chr = chr,
      start = start,
      end = end,
      feature_types = .query_import_feature_types(
        feature_type = feature_type,
        genes = genes,
        transcripts = transcripts
      )
    )
    all_gr <- imported$gr
    chr <- imported$chr
  } else {
    x <- load_annotation(x)
    all_gr <- annotation_data(x)
    if (!is.null(chr)) {
      chr <- .resolve_annotation_seqname(x, chr)
    }
  }
  index <- if (use_region_import) NULL else feature_index(x)
  gr <- all_gr

  has_selector <- !is.null(genes) || !is.null(transcripts) || !is.null(chr) || all
  if (!has_selector) {
    stop(
      "Provide `genes`, `transcripts`, `chr`, or set `all = TRUE`.",
      call. = FALSE
    )
  }

  type_filtered <- FALSE
  if (!is.null(index)) {
    keep_rows <- seq_along(all_gr)

    if (!is.null(genes)) {
      genes <- unique(as.character(genes))
      gene_rows <- .lookup_feature_index_rows(index, "gene", genes)
      gene_ids <- unique(.annotation_primary_ids(all_gr[gene_rows]))
      gene_ids <- gene_ids[!is.na(gene_ids) & nzchar(gene_ids)]
      transcript_ids <- unique(c(
        .annotation_transcript_ids(
          all_gr[
            .lookup_feature_index_rows(index, "gene", gene_ids)
          ]
        ),
        .annotation_transcript_ids(
          all_gr[.lookup_feature_index_rows(index, "parent", gene_ids)]
        )
      ))
      transcript_ids <- transcript_ids[!is.na(transcript_ids) & nzchar(transcript_ids)]

      keep_gene_rows <- union(
        gene_rows,
        .lookup_feature_index_rows(index, "gene", gene_ids)
      )
      if (length(transcript_ids) > 0L) {
        keep_gene_rows <- union(
          keep_gene_rows,
          .lookup_feature_index_rows(index, "transcript", transcript_ids)
        )
      }
      keep_rows <- intersect(keep_rows, keep_gene_rows)
    }

    if (!is.null(transcripts)) {
      transcripts <- unique(as.character(transcripts))
      transcript_rows <- .lookup_feature_index_rows(index, "transcript", transcripts)
      transcript_gene_ids <- unique(.annotation_primary_ids(all_gr[transcript_rows]))
      transcript_gene_ids <- transcript_gene_ids[
        !is.na(transcript_gene_ids) & nzchar(transcript_gene_ids)
      ]
      parent_gene_rows <- integer()
      if (length(transcript_gene_ids) > 0L) {
        parent_gene_rows <- .lookup_feature_index_rows(index, "gene", transcript_gene_ids)
        if (length(parent_gene_rows) > 0L) {
          parent_gene_rows <- parent_gene_rows[
            .annotation_gene_row_match(all_gr[parent_gene_rows], transcript_gene_ids)
          ]
        }
      }
      keep_rows <- intersect(
        keep_rows,
        union(transcript_rows, parent_gene_rows)
      )
    }

    if (!is.null(chr)) {
      keep_rows <- intersect(
        keep_rows,
        .lookup_feature_index_rows(index, "seqname", chr)
      )
    }

    if (!is.null(feature_type)) {
      keep_rows <- intersect(
        keep_rows,
        .lookup_feature_index_rows(index, "type", base::tolower(feature_type))
      )
      type_filtered <- TRUE
    }

    gr <- all_gr[keep_rows]
  } else {
    if (!is.null(genes)) {
      genes <- unique(as.character(genes))
      gene_match <- .match_annotation_values(
        all_gr,
        c("gene_name", "gene_id", "Name", "gene", "ID"),
        genes
      )
      gene_ids <- unique(.annotation_primary_ids(all_gr[gene_match]))
      gene_ids <- gene_ids[!is.na(gene_ids) & nzchar(gene_ids)]

      transcript_ids <- unique(c(
        .annotation_transcript_ids(
          all_gr[
            .match_annotation_values(
              all_gr,
              c("gene_id"),
              gene_ids
            )
          ]
        ),
        .annotation_transcript_ids(
          all_gr[
            .match_annotation_values(
              all_gr,
              c("Parent"),
              gene_ids
            )
          ]
        )
      ))
      transcript_ids <- transcript_ids[!is.na(transcript_ids) & nzchar(transcript_ids)]

      gene_filter <- .match_annotation_values(
        gr,
        c("gene_name", "gene_id", "Name", "gene", "ID"),
        genes
      )
      if (length(gene_ids) > 0L) {
        gene_filter <- gene_filter | .match_annotation_values(
          gr,
          c("gene_id"),
          gene_ids
        )
      }
      if (length(transcript_ids) > 0L) {
        gene_filter <- gene_filter | .match_annotation_values(
          gr,
          c("transcript_id", "Parent"),
          transcript_ids
        )
      }
      gr <- gr[gene_filter]
    }

    if (!is.null(transcripts)) {
      transcripts <- unique(as.character(transcripts))
      transcript_gene_ids <- unique(.annotation_primary_ids(
        gr[
          .match_annotation_values(
            gr,
            c("transcript_id", "Parent", "transcript_name", "ID"),
            transcripts
          )
        ]
      ))
      transcript_gene_ids <- transcript_gene_ids[
        !is.na(transcript_gene_ids) & nzchar(transcript_gene_ids)
      ]
      transcript_match <- .match_annotation_values(
        gr,
        c("transcript_id", "Parent", "transcript_name", "ID"),
        transcripts
      )
      if (length(transcript_gene_ids) > 0L) {
        transcript_match <- transcript_match | .annotation_gene_row_match(
          gr,
          transcript_gene_ids
        )
      }
      gr <- gr[transcript_match]
    }
  }

  if (!is.null(chr)) {
    chr <- as.character(chr)[1L]
    if (is.null(start)) {
      start <- 1L
    }
    if (is.null(end)) {
      chr_rows <- as.character(GenomeInfoDb::seqnames(gr)) == chr
      if (!any(chr_rows)) {
        gr <- gr[FALSE]
      } else {
        end <- max(IRanges::end(gr[chr_rows]))
      }
    }

    region <- GenomicRanges::GRanges(
      seqnames = chr,
      ranges = IRanges::IRanges(start = start, end = end)
    )
    gr <- gr[IRanges::overlapsAny(gr, region)]
  }

  if (!is.null(feature_type) && !isTRUE(type_filtered)) {
    types <- as.character(S4Vectors::mcols(gr)$type)
    gr <- gr[
      !is.na(types) &
        base::tolower(types) == base::tolower(feature_type)
    ]
  }

  gr
}

.resolve_annotation_window_seqname <- function(gr, chr, label = "annotation") {
  if (!is.character(chr) || length(chr) != 1L || is.na(chr) || !nzchar(chr)) {
    stop("`chr` must be a single non-empty character value.", call. = FALSE)
  }

  available <- unique(as.character(GenomeInfoDb::seqnames(gr)))
  if (length(available) == 0L) {
    stop("The ", label, " does not contain any sequence names.", call. = FALSE)
  }

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
    "Unknown chromosome ", chr, " for ", label,
    ". Available seqnames include: ",
    paste(utils::head(available, 10L), collapse = ", "),
    call. = FALSE
  )
}

.normalize_coords_input <- function(coords, arg = "`coords`") {
  if (is.null(coords)) {
    return(NULL)
  }

  if (is.list(coords)) {
    coords <- unlist(coords, use.names = FALSE)
  }

  if (!is.character(coords) || length(coords) == 0L) {
    stop(
      arg,
      " must be a character string or a list/vector of character strings.",
      call. = FALSE
    )
  }

  coords <- trimws(as.character(coords))
  if (any(is.na(coords)) || any(!nzchar(coords))) {
    stop(arg, " cannot contain missing or empty strings.", call. = FALSE)
  }

  coords
}

.parse_window_coords <- function(coords, arg = "`coords`") {
  coords <- .normalize_coords_input(coords, arg = arg)
  if (length(coords) != 1L) {
    stop(arg, " must contain exactly one coordinate string.", call. = FALSE)
  }

  match <- regmatches(
    coords[[1L]],
    regexec("^([^:]+):(\\d+)-(\\d+)$", coords[[1L]])
  )[[1L]]
  if (length(match) != 4L) {
    stop(
      arg,
      " must have the format 'seqname:start-end', for example 'V_RagTag:21559983-21620009'.",
      call. = FALSE
    )
  }

  chr <- trimws(match[[2L]])
  start <- as.integer(match[[3L]])
  end <- as.integer(match[[4L]])

  list(
    chr = chr,
    start = min(start, end),
    end = max(start, end)
  )
}

.parse_species_window_coords <- function(coords, arg = "`coords`") {
  coords <- .normalize_coords_input(coords, arg = arg)

  out <- lapply(coords, function(entry) {
    match <- regmatches(
      entry,
      regexec("^([^#]+)#(.+)$", entry)
    )[[1L]]
    if (length(match) != 3L) {
      stop(
        arg,
        " entries must have the format 'species#seqname:start-end', for example 'XZ1516#V_RagTag:21559983-21620009'.",
        call. = FALSE
      )
    }

    species <- trimws(match[[2L]])
    if (!nzchar(species)) {
      stop(arg, " entries must include a non-empty species tag.", call. = FALSE)
    }

    window <- .parse_window_coords(match[[3L]], arg = arg)
    c(list(species = species), window)
  })

  species <- vapply(out, `[[`, character(1), "species")
  if (anyDuplicated(species)) {
    duplicated_species <- unique(species[duplicated(species)])
    stop(
      arg,
      " contains duplicate species tags: ",
      paste(duplicated_species, collapse = ", "),
      call. = FALSE
    )
  }

  names(out) <- species
  out
}

.resolve_subset_window_args <- function(chr = NULL,
                                        start = NULL,
                                        end = NULL,
                                        coords = NULL,
                                        require_window = TRUE,
                                        arg = "`coords`") {
  if (!is.null(coords)) {
    if (!is.null(chr) || !is.null(start) || !is.null(end)) {
      stop(
        "Provide either ",
        arg,
        " or `chr`/`start`/`end`, not both.",
        call. = FALSE
      )
    }
    return(.parse_window_coords(coords, arg = arg))
  }

  if (is.null(chr)) {
    if (isTRUE(require_window)) {
      stop("Provide either `coords` or `chr`.", call. = FALSE)
    }
    return(NULL)
  }

  list(chr = chr, start = start, end = end)
}

.normalize_subset_feature_selector <- function(x, arg) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.list(x)) {
    x <- unlist(x, use.names = FALSE)
  }
  if (!is.character(x)) {
    stop("`", arg, "` must be a character vector or list of strings.", call. = FALSE)
  }
  x <- unique(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0L) {
    stop("`", arg, "` must contain at least one non-empty string.", call. = FALSE)
  }
  x
}

.filter_annotation_by_feature_ids <- function(gr,
                                              genes = NULL,
                                              transcripts = NULL) {
  if (length(gr) == 0L || (is.null(genes) && is.null(transcripts))) {
    return(gr)
  }

  out <- gr
  if (!is.null(genes)) {
    gene_match <- .match_annotation_values(
      out,
      c("gene_name", "gene_id", "Name", "gene", "ID"),
      genes
    )
    gene_ids <- unique(.annotation_primary_ids(out[gene_match]))
    gene_ids <- gene_ids[!is.na(gene_ids) & nzchar(gene_ids)]

    transcript_ids <- unique(c(
      .annotation_transcript_ids(
        out[
          .match_annotation_values(
            out,
            c("gene_id"),
            gene_ids
          )
        ]
      ),
      .annotation_transcript_ids(
        out[
          .match_annotation_values(
            out,
            c("Parent"),
            gene_ids
          )
        ]
      )
    ))
    transcript_ids <- transcript_ids[!is.na(transcript_ids) & nzchar(transcript_ids)]

    gene_filter <- .match_annotation_values(
      out,
      c("gene_name", "gene_id", "Name", "gene", "ID"),
      genes
    )
    if (length(gene_ids) > 0L) {
      gene_filter <- gene_filter | .match_annotation_values(
        out,
        c("gene_id"),
        gene_ids
      )
    }
    if (length(transcript_ids) > 0L) {
      gene_filter <- gene_filter | .match_annotation_values(
        out,
        c("transcript_id", "Parent"),
        transcript_ids
      )
    }
    out <- out[gene_filter]
  }

  if (!is.null(transcripts)) {
    transcript_gene_ids <- unique(.annotation_primary_ids(
      out[
        .match_annotation_values(
          out,
          c("transcript_id", "Parent", "transcript_name", "ID"),
          transcripts
        )
      ]
    ))
    transcript_gene_ids <- transcript_gene_ids[
      !is.na(transcript_gene_ids) & nzchar(transcript_gene_ids)
    ]
    transcript_match <- .match_annotation_values(
      out,
      c("transcript_id", "Parent", "transcript_name", "ID"),
      transcripts
    )
    if (length(transcript_gene_ids) > 0L) {
      transcript_match <- transcript_match | .annotation_gene_row_match(
        out,
        transcript_gene_ids
      )
    }
    out <- out[transcript_match]
  }

  out
}

.annotation_gene_row_match <- function(gr, gene_ids) {
  gene_ids <- unique(as.character(gene_ids))
  gene_ids <- gene_ids[!is.na(gene_ids) & nzchar(gene_ids)]
  if (length(gene_ids) == 0L || length(gr) == 0L) {
    return(rep(FALSE, length(gr)))
  }

  meta <- S4Vectors::mcols(gr)
  types <- base::tolower(as.character(meta$type))
  gene_rows <- !is.na(types) & types == "gene"
  gene_rows &
    .match_annotation_values(gr, c("gene_id", "gene_name", "Name", "gene", "ID"), gene_ids)
}

.infer_subset_window_from_gr <- function(gr) {
  if (length(gr) == 0L) {
    return(NULL)
  }
  seqlevels <- unique(as.character(GenomeInfoDb::seqnames(gr)))
  seqlevels <- seqlevels[!is.na(seqlevels) & nzchar(seqlevels)]
  if (length(seqlevels) != 1L) {
    return(NULL)
  }
  list(
    chr = seqlevels[[1L]],
    start = min(IRanges::start(gr), na.rm = TRUE),
    end = max(IRanges::end(gr), na.rm = TRUE)
  )
}

.subset_feature_annotation_counts <- function(gr) {
  if (length(gr) == 0L) {
    return(list(rows = 0L, genes = 0L, transcripts = 0L))
  }

  meta <- S4Vectors::mcols(gr)
  gene_ids <- unique(as.character(meta$gene_id))
  gene_ids <- gene_ids[!is.na(gene_ids) & nzchar(gene_ids)]
  transcript_ids <- unique(as.character(meta$transcript_id))
  transcript_ids <- transcript_ids[!is.na(transcript_ids) & nzchar(transcript_ids)]

  list(
    rows = as.integer(length(gr)),
    genes = as.integer(length(gene_ids)),
    transcripts = as.integer(length(transcript_ids))
  )
}

.inform_subset_feature_annotation <- function(gr) {
  counts <- .subset_feature_annotation_counts(gr)
  cli::cli_inform(
    "subset_feature_annotation() kept {counts$rows} rows, {counts$genes} genes, and {counts$transcripts} transcripts."
  )
}

.normalize_annotation_window <- function(gr,
                                         chr,
                                         start = NULL,
                                         end = NULL,
                                         label = "annotation",
                                         resolve_seqname = TRUE) {
  target_chr <- if (isTRUE(resolve_seqname)) {
    .resolve_annotation_window_seqname(gr, chr = chr, label = label)
  } else {
    chr
  }

  if (!is.null(start) &&
      (!is.numeric(start) || length(start) != 1L || is.na(start))) {
    stop("`start` must be a single numeric value or NULL.", call. = FALSE)
  }
  if (!is.null(end) &&
      (!is.numeric(end) || length(end) != 1L || is.na(end))) {
    stop("`end` must be a single numeric value or NULL.", call. = FALSE)
  }

  chr_rows <- as.character(GenomeInfoDb::seqnames(gr)) == target_chr
  chr_gr <- gr[chr_rows]
  if (length(chr_gr) == 0L) {
    stop("No annotation rows were found on chromosome ", target_chr, ".", call. = FALSE)
  }

  window_start <- if (is.null(start)) 1L else as.integer(start)
  window_end <- if (is.null(end)) max(IRanges::end(chr_gr)) else as.integer(end)

  list(
    chr = target_chr,
    start = min(window_start, window_end),
    end = max(window_start, window_end)
  )
}

.subset_granges_window <- function(gr,
                                   chr,
                                   start = NULL,
                                   end = NULL,
                                   label = "annotation",
                                   resolve_seqname = TRUE) {
  window <- .normalize_annotation_window(
    gr = gr,
    chr = chr,
    start = start,
    end = end,
    label = label,
    resolve_seqname = resolve_seqname
  )

  region <- GenomicRanges::GRanges(
    seqnames = window$chr,
    ranges = IRanges::IRanges(start = window$start, end = window$end)
  )
  keep <- IRanges::overlapsAny(gr, region)
  out <- gr[keep]
  if (length(out) == 0L) {
    return(out)
  }

  IRanges::ranges(out) <- IRanges::pintersect(
    IRanges::ranges(out),
    IRanges::ranges(region)[rep(1L, length(out))]
  )
  out
}

#' Subset a feature annotation layer by genomic window
#'
#' Returns a windowed snapshot of a [`SynFeatureAnnotation`]. The current
#' active annotation and its base annotation are trimmed to the requested
#' genomic interval. Patch history and derived caches are cleared so the
#' returned object behaves as a self-contained view of the selected region.
#'
#' @param x A `SynFeatureAnnotation`, `SynIndividual`, or `SynSpecies` object.
#' @param annotation Optional feature-annotation layer name when `x` is a
#'   `SynIndividual` or `SynSpecies`. Defaults to the active feature annotation.
#' @param individual Optional individual name when `x` is a `SynSpecies`.
#' @param chr Chromosome or sequence name to keep.
#' @param start,end Optional numeric window bounds. If one bound is omitted,
#'   the helper expands to the start or end of the selected sequence present in
#'   the annotation.
#' @param coords Optional coordinate string in the form
#'   `"V_RagTag:21559983-21620009"`. Use either `coords` or `chr`/`start`/`end`.
#' @param gene Optional gene name(s) or identifier(s) to keep.
#' @param transcript Optional transcript identifier(s) to keep.
#'
#' @details
#' This is an S4 generic that dispatches on the class of `x`.
#'
#' @return
#' Returns an object of the same top-level class supplied to `x`: a
#' `SynFeatureAnnotation`, updated `SynIndividual`, or updated `SynSpecies`.
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
#'
#' gr <- annotation_data(ind)
#' chr <- as.character(GenomeInfoDb::seqnames(gr))[[1L]]
#' start <- IRanges::start(gr)[[1L]]
#' end <- IRanges::end(gr)[[1L]]
#'
#' ann_view <- subset_feature_annotation(
#'   get_annotation(ind, "default"),
#'   chr = chr,
#'   start = start,
#'   end = end
#' )
#' ind_view <- subset_feature_annotation(ind, chr = chr, start = start, end = end)
#'
#' @export
setGeneric("subset_feature_annotation", function(x,
                                                 annotation = NULL,
                                                 individual = NULL,
                                                 chr = NULL,
                                                 start = NULL,
                                                 end = NULL,
                                                 coords = NULL,
                                                 gene = NULL,
                                                 transcript = NULL) {
  standardGeneric("subset_feature_annotation")
})

.subset_feature_annotation_impl <- function(x,
                                           annotation = NULL,
                                           individual = NULL,
                                           chr = NULL,
                                           start = NULL,
                                           end = NULL,
                                           coords = NULL,
                                           gene = NULL,
                                           transcript = NULL) {
  owner <- .resolve_subset_feature_annotation_input(
    x = x,
    annotation = annotation,
    individual = individual
  )
  ann <- owner$annotation
  genes <- .normalize_subset_feature_selector(gene, "gene")
  transcripts <- .normalize_subset_feature_selector(transcript, "transcript")
  require_window <- is.null(genes) && is.null(transcripts)

  args <- .resolve_subset_window_args(
    chr = chr,
    start = start,
    end = end,
    coords = coords,
    require_window = require_window
  )

  ann <- load_annotation(ann)
  active_gr <- annotation_data(ann)
  base_gr <- base_annotation(ann)
  if (is.null(base_gr)) {
    base_gr <- active_gr
  }

  active_gr <- .filter_annotation_by_feature_ids(
    active_gr,
    genes = genes,
    transcripts = transcripts
  )
  base_gr <- .filter_annotation_by_feature_ids(
    base_gr,
    genes = genes,
    transcripts = transcripts
  )

  window <- if (is.null(args)) {
    .infer_subset_window_from_gr(active_gr)
  } else {
    .normalize_annotation_window(
      gr = active_gr,
      chr = args$chr,
      start = args$start,
      end = args$end,
      label = paste0("annotation layer ", annotation_name(ann))
    )
  }

  window_metadata <- annotation_metadata(ann)
  window_metadata$subset_window <- window
  annotation_metadata(ann) <- window_metadata

  if (is.null(window)) {
    ann@annotation <- active_gr
    ann@base_annotation <- base_gr
  } else {
    ann@annotation <- .subset_granges_window(
      gr = active_gr,
      chr = window$chr,
      start = window$start,
      end = window$end,
      resolve_seqname = FALSE
    )
    ann@base_annotation <- .subset_granges_window(
      gr = base_gr,
      chr = window$chr,
      start = window$start,
      end = window$end,
      resolve_seqname = FALSE
    )
  }
  ann@patches <- list()
  ann@feature_index <- NULL
  ann@nucleotide_seq <- NULL
  ann@protein_seq <- NULL
  ann@plot_cache <- list()
  ann@loaded <- TRUE
  validObject(ann)
  .inform_subset_feature_annotation(ann@annotation)

  if (identical(owner$kind, "annotation")) {
    return(ann)
  }

  if (identical(owner$kind, "individual")) {
    return(add_annotation(owner$object, ann, set_active = identical(annotation_name(ann), active_feature_annotation(owner$object))))
  }

  if (identical(owner$kind, "species")) {
    updated_individual <- add_annotation(
      owner$individual,
      ann,
      set_active = identical(annotation_name(ann), active_feature_annotation(owner$individual))
    )
    individuals <- owner$object@individuals
    individuals[[syn_id(updated_individual)]] <- updated_individual
    owner$object@individuals <- individuals
    validObject(owner$object)
    return(owner$object)
  }

  ann
}

#' @rdname subset_feature_annotation
setMethod("subset_feature_annotation", "SynFeatureAnnotation", function(x,
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

#' @rdname subset_feature_annotation
setMethod("subset_feature_annotation", "SynIndividual", function(x,
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

.resolve_subset_feature_annotation_input <- function(x,
                                                     annotation = NULL,
                                                     individual = NULL) {
  if (methods::is(x, "SynFeatureAnnotation")) {
    return(list(kind = "annotation", annotation = x))
  }

  if (methods::is(x, "SynSpecies")) {
    individual_obj <- resolve_syn_individual(x, species = individual)
    ann_name <- annotation %||% active_feature_annotation(individual_obj)
    ann <- get_annotation(individual_obj, ann_name)
    if (!methods::is(ann, "SynFeatureAnnotation")) {
      stop(
        "`subset_feature_annotation()` requires a SynFeatureAnnotation layer.",
        call. = FALSE
      )
    }
    return(list(
      kind = "species",
      object = x,
      individual = individual_obj,
      annotation = ann
    ))
  }

  if (methods::is(x, "SynIndividual")) {
    ann_name <- annotation %||% active_feature_annotation(x)
    ann <- get_annotation(x, ann_name)
    if (!methods::is(ann, "SynFeatureAnnotation")) {
      stop(
        "`subset_feature_annotation()` requires a SynFeatureAnnotation layer.",
        call. = FALSE
      )
    }
    return(list(kind = "individual", object = x, annotation = ann))
  }

  stop(
    "`subset_feature_annotation()` expects a SynFeatureAnnotation, SynIndividual, or SynSpecies object.",
    call. = FALSE
  )
}

#' Subset a SynIndividual by genomic window
#'
#' Returns a new [`SynIndividual`] whose feature annotation layers are trimmed
#' to the requested genomic window. By default all attached
#' [`SynFeatureAnnotation`] layers are subsetted, while non-feature annotation
#' layers remain attached unchanged. Object-level derived caches are cleared so
#' the result is ready to use as a clean windowed view.
#'
#' @param x A `SynIndividual` or `SynSpecies` object.
#' @param individual Optional individual name when `x` is a `SynSpecies`.
#' @param chr Chromosome or sequence name to keep.
#' @param start,end Optional numeric window bounds. If one bound is omitted,
#'   the helper expands to the start or end of the selected sequence present in
#'   the active feature annotation.
#' @param coords Optional coordinate string in the form
#'   `"V_RagTag:21559983-21620009"`. Use either `coords` or `chr`/`start`/`end`.
#' @param annotations One of `"all_feature"` or `"active"`. Controls whether
#'   all feature annotation layers are trimmed, or only the active one.
#'
#' @details
#' This is an S4 generic that dispatches on the class of `x`.
#'
#' @return A `SynIndividual` object.
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
#'
#' gr <- annotation_data(ind)
#' chr <- as.character(GenomeInfoDb::seqnames(gr))[[1L]]
#' start <- IRanges::start(gr)[[1L]]
#' end <- IRanges::end(gr)[[1L]]
#'
#' ind_window <- subset_individual(ind, chr = chr, start = start, end = end)
#'
#' @export
setGeneric("subset_individual", function(x,
                                         individual = NULL,
                                         chr = NULL,
                                         start = NULL,
                                         end = NULL,
                                         coords = NULL,
                                         annotations = c("all_feature", "active")) {
  standardGeneric("subset_individual")
})

.subset_individual_impl <- function(x,
                                    individual = NULL,
                                    chr = NULL,
                                    start = NULL,
                                    end = NULL,
                                    coords = NULL,
                                    annotations = c("all_feature", "active")) {
  if (methods::is(x, "SynSpecies")) {
    x <- resolve_syn_individual(x, species = individual)
  }

  args <- .resolve_subset_window_args(
    chr = chr,
    start = start,
    end = end,
    coords = coords
  )
  annotations <- match.arg(annotations)
  feature_names <- names(x@annotations)[vapply(
    x@annotations,
    methods::is,
    logical(1),
    class2 = "SynFeatureAnnotation"
  )]

  if (length(feature_names) == 0L) {
    stop("The SynIndividual does not contain any SynFeatureAnnotation layers.", call. = FALSE)
  }

  target_names <- if (identical(annotations, "active")) {
    active_feature_annotation(x)
  } else {
    feature_names
  }

  out <- x
  out@plot_cache <- list()
  out@projected_domains <- list()

  for (name in target_names) {
    ann <- get_annotation(out, name)
    ann <- subset_feature_annotation(
      ann,
      chr = args$chr,
      start = args$start,
      end = args$end
    )
    out <- add_annotation(out, ann, set_active = FALSE)
  }

  active_ann <- get_annotation(out, active_feature_annotation(out))
  out@annotation_file <- annotation_file(active_ann)
  out@annotation_format <- annotation_format(active_ann)
  out@seqinfo <- if (!is.null(annotation_data(active_ann))) {
    GenomeInfoDb::seqinfo(annotation_data(active_ann))
  } else {
    NULL
  }

  validObject(out)
  out
}

#' @rdname subset_individual
setMethod("subset_individual", "SynIndividual", function(x,
                                                         individual = NULL,
                                                         chr = NULL,
                                                         start = NULL,
                                                         end = NULL,
                                                         coords = NULL,
                                                         annotations = c("all_feature", "active")) {
  .subset_individual_impl(x, individual, chr, start, end, coords, annotations)
})


#' Extract CDS nucleotide sequences
#'
#' @param x A `SynIndividual` object.
#' @param genes Optional character vector of gene names or identifiers.
#' @param transcripts Optional character vector of transcript identifiers.
#' @param chr Optional chromosome name.
#' @param start Optional start coordinate.
#' @param end Optional end coordinate.
#' @param all Logical; when `TRUE`, extract all CDS records.
#' @param store Logical; when `TRUE`, store the extracted sequences in
#'   `nucleotide_seq`.
#' @param append Logical; when `TRUE`, append new sequences to existing cached
#'   values by name.
#'
#' @return An updated `SynIndividual` object when `store = TRUE`, otherwise a
#'   `DNAStringSet`.
#' @export
extract_cds_seq <- function(x,
                            genes = NULL,
                            transcripts = NULL,
                            chr = NULL,
                            start = NULL,
                            end = NULL,
                            all = FALSE,
                            store = TRUE,
                            append = TRUE) {
  if (!methods::is(x, "SynIndividual")) {
    stop("`extract_cds_seq()` expects a SynIndividual object.", call. = FALSE)
  }

  x <- load_annotation(x)
  cds_gr <- query_features(
    x,
    genes = genes,
    transcripts = transcripts,
    chr = chr,
    start = start,
    end = end,
    feature_type = "CDS",
    all = all
  )

  cds_dna <- .extract_cds_sequences_from_gr(
    genome_file = .require_genome_file(x, call = "`extract_cds_seq()`"),
    cds_gr = cds_gr
  )

  if (!store) {
    return(cds_dna)
  }

  active_name <- active_feature_annotation(x)
  ann <- get_annotation(x, active_name)
  existing <- nucleotide_seq(ann)
  nucleotide_seq(ann) <- .merge_string_sets(existing, cds_dna, append = append)
  x <- add_annotation(x, ann, set_active = TRUE)
  nucleotide_seq(x) <- nucleotide_seq(ann)
  x
}

#' Translate CDS sequences to proteins
#'
#' @param x A `SynIndividual` object.
#' @param genes Optional character vector of gene names or identifiers.
#' @param transcripts Optional character vector of transcript identifiers.
#' @param chr Optional chromosome name.
#' @param start Optional start coordinate.
#' @param end Optional end coordinate.
#' @param all Logical; when `TRUE`, translate all CDS records.
#' @param store Logical; when `TRUE`, store translated proteins in
#'   `protein_seq` and the CDS sequences in `nucleotide_seq`.
#' @param append Logical; when `TRUE`, append new sequences to existing cached
#'   values by name.
#' @param if.fuzzy.codon Passed to `Biostrings::translate()`.
#'
#' @return An updated `SynIndividual` object when `store = TRUE`, otherwise an
#'   `AAStringSet`.
#' @export
translate_protein <- function(x,
                              genes = NULL,
                              transcripts = NULL,
                              chr = NULL,
                              start = NULL,
                              end = NULL,
                              all = FALSE,
                              store = TRUE,
                              append = TRUE,
                              if.fuzzy.codon = "error") {
  cds_dna <- extract_cds_seq(
    x,
    genes = genes,
    transcripts = transcripts,
    chr = chr,
    start = start,
    end = end,
    all = all,
    store = FALSE
  )

  aa <- Biostrings::translate(cds_dna, if.fuzzy.codon = if.fuzzy.codon)
  names(aa) <- names(cds_dna)

  if (!store) {
    return(aa)
  }

  updated_x <- extract_cds_seq(
    x,
    genes = genes,
    transcripts = transcripts,
    chr = chr,
    start = start,
    end = end,
    all = all,
    store = TRUE,
    append = append
  )
  active_name <- active_feature_annotation(updated_x)
  ann <- get_annotation(updated_x, active_name)
  protein_seq(ann) <- .merge_string_sets(
    protein_seq(ann),
    aa,
    append = append
  )
  updated_x <- add_annotation(updated_x, ann, set_active = TRUE)
  protein_seq(updated_x) <- protein_seq(ann)
  updated_x
}

#' Coerce and assign annotation data
#'
#' @param x A `SynIndividual` object.
#' @param value A `GenomicRanges::GRanges` object or `NULL`.
#'
#' @return The updated `SynIndividual` object.
#' @export
setGeneric("annotation_data<-", function(x, value) {
  standardGeneric("annotation_data<-")
})

.open_text_connection <- function(path) {
  if (grepl("\\.gz$", path, ignore.case = TRUE)) {
    gzfile(path, open = "rt")
  } else {
    file(path, open = "rt")
  }
}

.quiet_indexed_annotation_import <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      message <- conditionMessage(w)
      if (identical(message, "Use BiocIO::FileForFormat") ||
          grepl("connection is not positioned at the start of the file, rewinding it", message, fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

.annotation_index_path <- function(path) {
  candidates <- c(paste0(path, ".tbi"), paste0(path, ".csi"))
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0L) {
    return(NULL)
  }
  existing[[1L]]
}

.is_indexed_annotation_file <- function(path) {
  grepl("\\.(gff3?|gtf)\\.gz$", path, ignore.case = TRUE) &&
    !is.null(.annotation_index_path(path))
}

.annotation_import_format <- function(x) {
  declared_format <- if (methods::is(x, "SynIndividual") || methods::is(x, "SynFeatureAnnotation")) {
    annotation_format(x)
  } else {
    "auto"
  }

  path <- if (methods::is(x, "SynIndividual") || methods::is(x, "SynFeatureAnnotation")) {
    annotation_file(x)
  } else {
    x
  }
  if (length(declared_format) == 1L) {
    declared_format <- rep(declared_format, length(path))
  }

  vapply(seq_along(path), function(i) {
    if (!identical(declared_format[[i]], "auto")) {
      return(declared_format[[i]])
    }

    ext <- base::tolower(tools::file_ext(sub("\\.gz$", "", path[[i]], ignore.case = TRUE)))
    switch(
      ext,
      gff = "gff",
      gff3 = "gff",
      gtf = "gtf",
      stop("Unsupported annotation file format: ", path[[i]], call. = FALSE)
    )
  }, character(1))
}

.read_fasta_headers <- function(path) {
  con <- .open_text_connection(path)
  on.exit(close(con), add = TRUE)

  lines <- readLines(con, warn = FALSE)
  header_lines <- grep("^>", lines, value = TRUE)
  if (length(header_lines) == 0L) {
    return(character())
  }

  unique(sub("\\s.*$", "", substring(header_lines, 2L)))
}

.read_annotation_seqnames <- function(path) {
  path <- .normalize_annotation_file_input(path)
  seqnames <- unlist(lapply(path, function(one_path) {
    if (.is_indexed_annotation_file(one_path)) {
      tabix <- Rsamtools::TabixFile(one_path, index = .annotation_index_path(one_path))
      open(tabix)
      on.exit(try(close(tabix), silent = TRUE), add = TRUE)

      seqs <- as.character(Rsamtools::seqnamesTabix(tabix))
      seqs <- trimws(seqs)
      return(unique(seqs[nzchar(seqs)]))
    }

    con <- .open_text_connection(one_path)
    on.exit(close(con), add = TRUE)

    lines <- readLines(con, warn = FALSE)
    lines <- lines[!grepl("^\\s*#", lines)]
    lines <- lines[nzchar(lines)]
    if (length(lines) == 0L) {
      return(character())
    }

    fields <- strsplit(lines, "\t", fixed = TRUE)
    seqs <- vapply(
      fields,
      function(x) {
        if (length(x) == 0L) "" else x[[1L]]
      },
      character(1)
    )
    seqs <- trimws(seqs)
    unique(seqs[nzchar(seqs)])
  }), use.names = FALSE)
  unique(seqnames)
}

.combine_annotation_granges <- function(gr_list) {
  gr_list <- Filter(function(x) length(x) > 0L, gr_list)
  if (length(gr_list) == 0L) {
    return(GenomicRanges::GRanges())
  }
  if (length(gr_list) == 1L) {
    return(gr_list[[1L]])
  }
  do.call(c, unname(gr_list))
}

.import_all_annotation_files <- function(x) {
  annotation_paths <- annotation_file(x)
  annotation_formats <- .annotation_import_format(x)
  imported <- lapply(seq_along(annotation_paths), function(i) {
    rtracklayer::import(
      annotation_paths[[i]],
      format = annotation_formats[[i]]
    )
  })
  .combine_annotation_granges(imported)
}

.normalize_annotation <- function(gr) {
  meta <- S4Vectors::mcols(gr)
  type <- as.character(meta$type)
  meta$type <- type

  raw_id <- if ("ID" %in% colnames(meta)) as.character(meta$ID) else rep(NA_character_, nrow(meta))
  raw_parent <- if ("Parent" %in% colnames(meta)) as.character(meta$Parent) else rep(NA_character_, nrow(meta))
  raw_name <- if ("Name" %in% colnames(meta)) as.character(meta$Name) else rep(NA_character_, nrow(meta))
  raw_gene_id <- if ("gene_id" %in% colnames(meta)) as.character(meta$gene_id) else rep(NA_character_, nrow(meta))
  raw_transcript_id <- if ("transcript_id" %in% colnames(meta)) {
    as.character(meta$transcript_id)
  } else {
    rep(NA_character_, nrow(meta))
  }

  gene_id <- raw_gene_id
  is_gene <- type == "gene"
  is_transcript <- type %in% c("mRNA", "transcript")
  is_child_feature <- type %in% c("CDS", "exon", "five_prime_UTR", "three_prime_UTR")

  gene_id[is_gene & (is.na(gene_id) | !nzchar(gene_id))] <- raw_id[is_gene & (is.na(gene_id) | !nzchar(gene_id))]
  gene_id[is_transcript & (is.na(gene_id) | !nzchar(gene_id))] <- raw_parent[is_transcript & (is.na(gene_id) | !nzchar(gene_id))]
  gene_id[is_child_feature & (is.na(gene_id) | !nzchar(gene_id))] <- NA_character_
  gene_id[is.na(gene_id) | !nzchar(gene_id)] <- .coalesce_character_cols(meta, c("gene", "gene_name", "Name"))[is.na(gene_id) | !nzchar(gene_id)]

  gene_name <- if ("gene_name" %in% colnames(meta)) as.character(meta$gene_name) else rep(NA_character_, nrow(meta))
  gene_name[is.na(gene_name) | !nzchar(gene_name)] <- raw_name[is.na(gene_name) | !nzchar(gene_name)]
  gene_name[is.na(gene_name) | !nzchar(gene_name)] <- gene_id[is.na(gene_name) | !nzchar(gene_name)]

  transcript_id <- raw_transcript_id
  transcript_id[is_transcript & (is.na(transcript_id) | !nzchar(transcript_id))] <- raw_id[is_transcript & (is.na(transcript_id) | !nzchar(transcript_id))]
  transcript_id[is_child_feature & (is.na(transcript_id) | !nzchar(transcript_id))] <- raw_parent[is_child_feature & (is.na(transcript_id) | !nzchar(transcript_id))]
  transcript_id[is_gene] <- NA_character_

  meta$gene_id <- gene_id
  meta$gene_name <- gene_name
  meta$transcript_id <- transcript_id
  meta$phase <- .coalesce_character_cols(meta, c("phase", "frame"))

  S4Vectors::mcols(gr) <- meta
  gr
}

.coalesce_character_cols <- function(meta, candidates) {
  values <- rep(NA_character_, nrow(meta))
  for (col in candidates) {
    if (!col %in% colnames(meta)) {
      next
    }
    current <- as.character(meta[[col]])
    use_current <- is.na(values) & !is.na(current) & nzchar(current)
    values[use_current] <- current[use_current]
  }
  values
}

.match_annotation_values <- function(gr, candidates, values) {
  values <- unique(as.character(values))
  if (length(values) == 0L) {
    return(rep(FALSE, length(gr)))
  }

  matches <- rep(FALSE, length(gr))
  meta <- S4Vectors::mcols(gr)
  for (col in candidates) {
    if (!col %in% colnames(meta)) {
      next
    }
    matches <- matches | as.character(meta[[col]]) %in% values
  }
  matches
}

.annotation_primary_ids <- function(gr) {
  .coalesce_character_cols(
    S4Vectors::mcols(gr),
    c("gene_id", "gene_name", "ID", "Name")
  )
}

.annotation_transcript_ids <- function(gr) {
  .coalesce_character_cols(
    S4Vectors::mcols(gr),
    c("transcript_id", "ID", "Parent")
  )
}

.extract_cds_sequences_from_gr <- function(genome_file, cds_gr) {
  if (length(cds_gr) == 0L) {
    return(Biostrings::DNAStringSet())
  }

  genome_file <- .require_genome_file(genome_file, call = "CDS extraction")

  genome <- Biostrings::readDNAStringSet(filepath = genome_file)
  names(genome) <- sub("\\s.*$", "", names(genome))

  transcript_ids <- .coalesce_character_cols(
    S4Vectors::mcols(cds_gr),
    c("transcript_id", "Parent", "gene_id", "gene_name", "ID")
  )
  if (any(is.na(transcript_ids) | !nzchar(transcript_ids))) {
    stop("Every CDS record must have a transcript or gene identifier.", call. = FALSE)
  }

  split_index <- split(seq_along(cds_gr), transcript_ids)
  out <- vector("list", length(split_index))
  out_names <- names(split_index)

  for (i in seq_along(split_index)) {
    idx <- split_index[[i]]
    tx_gr <- cds_gr[idx]
    tx_name <- out_names[[i]]

    tx_seq <- Biostrings::DNAStringSet(.assemble_transcript_cds(tx_gr, genome))
    names(tx_seq) <- tx_name
    out[[i]] <- tx_seq
  }

  do.call(c, out)
}

.assemble_transcript_cds <- function(tx_gr, genome) {
  strand_value <- unique(as.character(BiocGenerics::strand(tx_gr)))
  strand_value <- strand_value[!is.na(strand_value)]
  if (length(strand_value) != 1L) {
    stop("CDS records for one transcript must share a single strand.", call. = FALSE)
  }

  seqname_value <- unique(as.character(GenomeInfoDb::seqnames(tx_gr)))
  if (length(seqname_value) != 1L) {
    stop("CDS records for one transcript must share a single seqname.", call. = FALSE)
  }
  seqname_value <- seqname_value[[1L]]
  if (!seqname_value %in% names(genome)) {
    stop("Sequence '", seqname_value, "' is missing from the genome FASTA.", call. = FALSE)
  }

  # Assemble CDS in transcript 5'->3' order: genomic low->high on '+' and
  # genomic high->low on '-', then reverse-complement each minus-strand segment
  # before concatenation so the final sequence is coding-space oriented.
  if (strand_value == "-") {
    order_idx <- order(IRanges::start(tx_gr), decreasing = TRUE)
  } else {
    order_idx <- order(IRanges::start(tx_gr))
  }
  tx_gr <- tx_gr[order_idx]

  tx_parts <- vector("list", length(tx_gr))
  chr_seq <- genome[[seqname_value]]

  for (i in seq_along(tx_gr)) {
    seg <- Biostrings::subseq(
      chr_seq,
      start = IRanges::start(tx_gr)[i],
      end = IRanges::end(tx_gr)[i]
    )
    if (strand_value == "-") {
      seg <- Biostrings::reverseComplement(seg)
    }
    tx_parts[[i]] <- seg
  }

  do.call(Biostrings::xscat, tx_parts)
}

.merge_string_sets <- function(existing, new_values, append = TRUE) {
  if (is.null(existing) || !append) {
    return(new_values)
  }

  if (!methods::is(existing, class(new_values)[1L])) {
    stop("Existing and new sequence caches must use the same class.", call. = FALSE)
  }

  existing_names <- names(existing)
  new_names <- names(new_values)
  if (is.null(existing_names) || any(!nzchar(existing_names))) {
    return(c(existing, new_values))
  }
  if (is.null(new_names) || any(!nzchar(new_names))) {
    return(c(existing, new_values))
  }

  keep_existing <- !existing_names %in% new_names
  c(existing[keep_existing], new_values)
}

#' @export
#' @rdname ggexon-show
setMethod("show", "SynIndividual", function(object) {
  loaded <- c(
    annotation = !is.null(annotation_data(object)),
    nucleotide_seq = !is.null(nucleotide_seq(object)),
    protein_seq = !is.null(protein_seq(object)),
    seqinfo = !is.null(object@seqinfo),
    feature_index = !is.null(feature_index(object))
  )

  cat("An object of class \"SynIndividual\"\n")
  cat("  id:", object@id, "\n")
  cat(
    "  genome_file:",
    if (.has_genome_file(object@genome_file)) object@genome_file else "<waived>",
    "\n"
  )
  cat("  annotation_file:", object@annotation_file, "\n")
  cat("  annotation_format:", object@annotation_format, "\n")
  cat("  active_feature_annotation:", object@active_annotation, "\n")
  cat("  projected_domains:", length(object@projected_domains), "\n")
  cat("  loaded:", paste(names(loaded)[loaded], collapse = ", "), "\n")
})

setGeneric("syn_id", function(x) standardGeneric("syn_id"))
setMethod("syn_id", "SynIndividual", function(x) x@id)

setGeneric("genome_file", function(x) standardGeneric("genome_file"))
setMethod("genome_file", "SynIndividual", function(x) x@genome_file)

setGeneric("annotation_file", function(x) standardGeneric("annotation_file"))
setMethod("annotation_file", "SynIndividual", function(x) x@annotation_file)
setMethod("annotation_file", "SynAnnotation", function(x) source_file(x))

setGeneric("annotation_format", function(x) standardGeneric("annotation_format"))
setMethod("annotation_format", "SynIndividual", function(x) x@annotation_format)
setMethod("annotation_format", "SynFeatureAnnotation", function(x) x@annotation_format)

#' Access annotation data
#'
#' Return the feature-annotation `GRanges` for the active annotation layer of a
#' `SynIndividual`, or the `GRanges` stored in a `SynFeatureAnnotation`. This is
#' the read counterpart to [`annotation_data<-`].
#'
#' @param x A `SynIndividual` or `SynFeatureAnnotation` object.
#'
#' @return A `GenomicRanges::GRanges` object, or `NULL` when no active
#'   annotation is present.
#' @export
setGeneric("annotation_data", function(x) standardGeneric("annotation_data"))
#' @rdname annotation_data
setMethod("annotation_data", "SynIndividual", function(x) {
  ann_name <- active_feature_annotation(x)
  if (!ann_name %in% names(x@annotations)) return(NULL)
  ann <- get_annotation(x, ann_name)
  annotation_data(ann)
})
#' @rdname annotation_data
setMethod("annotation_data", "SynFeatureAnnotation", function(x) x@annotation)

setGeneric("nucleotide_seq", function(x) standardGeneric("nucleotide_seq"))
setMethod("nucleotide_seq", "SynIndividual", function(x) {
  ann_name <- active_feature_annotation(x)
  if (!ann_name %in% names(x@annotations)) return(NULL)
  ann <- get_annotation(x, ann_name)
  nucleotide_seq(ann)
})
setMethod("nucleotide_seq", "SynFeatureAnnotation", function(x) x@nucleotide_seq)

setGeneric("protein_seq", function(x) standardGeneric("protein_seq"))
setMethod("protein_seq", "SynIndividual", function(x) {
  ann_name <- active_feature_annotation(x)
  if (!ann_name %in% names(x@annotations)) return(NULL)
  ann <- get_annotation(x, ann_name)
  protein_seq(ann)
})
setMethod("protein_seq", "SynFeatureAnnotation", function(x) x@protein_seq)

setGeneric("seqinfo", function(x) standardGeneric("seqinfo"))
setMethod("seqinfo", "SynIndividual", function(x) x@seqinfo)

setGeneric("feature_index", function(x) standardGeneric("feature_index"))
setMethod("feature_index", "SynIndividual", function(x) {
  ann_name <- active_feature_annotation(x)
  if (!ann_name %in% names(x@annotations)) return(NULL)
  ann <- get_annotation(x, ann_name)
  feature_index(ann)
})
setMethod("feature_index", "SynFeatureAnnotation", function(x) x@feature_index)

setGeneric("syn_metadata", function(x) standardGeneric("syn_metadata"))
setMethod("syn_metadata", "SynIndividual", function(x) x@metadata)

setGeneric("plot_cache", function(x) standardGeneric("plot_cache"))
setMethod("plot_cache", "SynIndividual", function(x) x@plot_cache)
setMethod("plot_cache", "SynAnnotation", function(x) x@plot_cache)

setGeneric("projected_domains", function(x) standardGeneric("projected_domains"))
setMethod("projected_domains", "SynIndividual", function(x) x@projected_domains)

setGeneric("annotation_names", function(x) standardGeneric("annotation_names"))
setMethod("annotation_names", "SynIndividual", function(x) names(x@annotations) %||% character())

setGeneric("active_annotation", function(x) {
  standardGeneric("active_annotation")
})
setMethod("active_annotation", "SynIndividual", function(x) x@active_annotation)

setGeneric("active_feature_annotation", function(x) {
  standardGeneric("active_feature_annotation")
})
setMethod("active_feature_annotation", "SynIndividual", function(x) {
  x@active_annotation
})

#' Add or replace an annotation layer on a SynIndividual
#'
#' @param x A `SynIndividual` object.
#' @param annotation A `SynAnnotation` object.
#' @param set_active Logical; when `TRUE`, make this the active annotation.
#'
#' @return An updated `SynIndividual` object.
#' @export
setGeneric("add_annotation", function(x, annotation, set_active = FALSE) {
  standardGeneric("add_annotation")
}, signature = c("x", "annotation"))

#' @rdname add_annotation
setMethod("add_annotation", c("SynIndividual", "SynAnnotation"), function(x, annotation, set_active = FALSE) {
  annotations <- x@annotations
  annotations[[annotation_name(annotation)]] <- annotation
  x@annotations <- annotations

  if (isTRUE(set_active) && !methods::is(annotation, "SynFeatureAnnotation")) {
    stop(
      "`set_active = TRUE` is only supported for SynFeatureAnnotation layers.",
      call. = FALSE
    )
  }

  active_is_feature <- x@active_annotation %in% names(annotations) &&
    methods::is(annotations[[x@active_annotation]], "SynFeatureAnnotation")
  if ((isTRUE(set_active) || !active_is_feature) &&
      methods::is(annotation, "SynFeatureAnnotation")) {
    x@active_annotation <- annotation_name(annotation)
  }

  if (identical(x@active_annotation, annotation_name(annotation)) &&
      methods::is(annotation, "SynFeatureAnnotation")) {
    x@annotation_file <- annotation_file(annotation)
    x@annotation_format <- annotation_format(annotation)
  }

  validObject(x)
  x
})

#' @rdname add_annotation
setMethod("add_annotation", c("SynIndividual", "ANY"), function(x, annotation, set_active = FALSE) {
  stop("`annotation` must be a SynAnnotation object.", call. = FALSE)
})

#' @rdname add_annotation
setMethod("add_annotation", c("ANY", "SynAnnotation"), function(x, annotation, set_active = FALSE) {
  stop("`add_annotation()` expects a SynIndividual object.", call. = FALSE)
})

#' @rdname add_annotation
setMethod("add_annotation", c("ANY", "ANY"), function(x, annotation, set_active = FALSE) {
  stop("`add_annotation()` expects a SynIndividual object.", call. = FALSE)
})

#' Attach an InterProScan protein-domain layer to a SynIndividual
#'
#' @param x A `SynIndividual` object.
#' @param domain_file Path to an InterProScan TSV export. Defaults to the
#'   bundled `InterProScan.tsv` example when available.
#' @param name Annotation-layer name used to store the imported domains.
#' @param keytype Identifier column used to match domain rows to proteins or
#'   transcripts.
#' @param source_db Domain database label recorded in the annotation metadata.
#'
#' @return An updated `SynIndividual` object with a
#'   `SynProteinDomainAnnotation` layer attached.
#' @export
setGeneric("add_interproscan_annotation", function(x,
                                                   domain_file = system.file(
                                                     "extdata",
                                                     "InterProScan.tsv",
                                                     package = "ggexon"
                                                   ),
                                                   name = "interpro",
                                                   keytype = c("protein_id", "transcript_id", "gene_id"),
                                                   source_db = "InterPro") {
  standardGeneric("add_interproscan_annotation")
}, signature = "x")

#' @rdname add_interproscan_annotation
setMethod("add_interproscan_annotation", "SynIndividual", function(x,
                                                                  domain_file = system.file(
                                                                    "extdata",
                                                                    "InterProScan.tsv",
                                                                    package = "ggexon"
                                                                  ),
                                                                  name = "interpro",
                                                                  keytype = c("protein_id", "transcript_id", "gene_id"),
                                                                  source_db = "InterPro") {
  keytype <- match.arg(keytype)

  if (!is.character(domain_file) || length(domain_file) != 1L ||
      is.na(domain_file) || !nzchar(domain_file)) {
    stop(
      "`domain_file` must be a single non-empty character value.",
      call. = FALSE
    )
  }
  if (!file.exists(domain_file)) {
    stop("InterProScan file does not exist: ", domain_file, call. = FALSE)
  }

  add_annotation(
    x,
    SynProteinDomainAnnotation(
      name = name,
      domain_file = domain_file,
      keytype = keytype,
      source_db = source_db
    )
  )
})

#' @rdname add_interproscan_annotation
setMethod("add_interproscan_annotation", "ANY", function(x,
                                                        domain_file = system.file(
                                                          "extdata",
                                                          "InterProScan.tsv",
                                                          package = "ggexon"
                                                        ),
                                                        name = "interpro",
                                                        keytype = c("protein_id", "transcript_id", "gene_id"),
                                                        source_db = "InterPro") {
  stop(
    "`add_interproscan_annotation()` expects a SynIndividual object.",
    call. = FALSE
  )
})

#' Retrieve an annotation layer from a SynIndividual
#'
#' @param x A `SynIndividual` object.
#' @param name Annotation layer name. Defaults to the active annotation.
#'
#' @return A `SynAnnotation` object.
#' @export
get_annotation <- function(x, name = NULL) {
  if (!methods::is(x, "SynIndividual")) {
    stop("`get_annotation()` expects a SynIndividual object.", call. = FALSE)
  }
  if (is.null(name)) {
    name <- active_annotation(x)
  }
  if (!name %in% names(x@annotations)) {
    stop("Unknown annotation layer: ", name, call. = FALSE)
  }
  x@annotations[[name]]
}

#' Set the active annotation layer on a SynIndividual
#'
#' @param x A `SynIndividual` object.
#' @param name Annotation layer name.
#'
#' @return An updated `SynIndividual` object.
#' @export
set_active_annotation <- function(x, name) {
  ann <- get_annotation(x, name)
  if (!methods::is(ann, "SynFeatureAnnotation")) {
    stop(
      "`set_active_annotation()` expects a SynFeatureAnnotation layer name.",
      call. = FALSE
    )
  }
  x@active_annotation <- name
  x@annotation_file <- annotation_file(ann)
  x@annotation_format <- annotation_format(ann)
  validObject(x)
  x
}

#' Set the active feature annotation layer on a SynIndividual
#'
#' @param x A `SynIndividual` object.
#' @param name Feature annotation layer name.
#'
#' @return An updated `SynIndividual` object.
#' @export
set_active_feature_annotation <- function(x, name) {
  set_active_annotation(x, name)
}

#' @rdname annotation_data-set
setReplaceMethod("annotation_data", "SynIndividual", function(x, value) {
  if (!is.null(value) && !methods::is(value, "GRanges")) {
    stop("`annotation_data<-` expects a GRanges object or NULL.", call. = FALSE)
  }
  if (length(x@annotations) > 0L && active_annotation(x) %in% names(x@annotations)) {
    ann <- x@annotations[[active_annotation(x)]]
    ann@annotation <- value
    x@annotations[[active_annotation(x)]] <- ann
  }
  validObject(x)
  x
})
#' @rdname annotation_data-set
setReplaceMethod("annotation_data", "SynFeatureAnnotation", function(x, value) {
  if (!is.null(value) && !methods::is(value, "GRanges")) {
    stop("`annotation_data<-` expects a GRanges object or NULL.", call. = FALSE)
  }
  x@annotation <- value
  x@loaded <- !is.null(value)
  validObject(x)
  x
})

setGeneric("nucleotide_seq<-", function(x, value) {
  standardGeneric("nucleotide_seq<-")
})
setReplaceMethod("nucleotide_seq", "SynIndividual", function(x, value) {
  if (!is.null(value) && !methods::is(value, "DNAStringSet")) {
    stop("`nucleotide_seq<-` expects a DNAStringSet object or NULL.", call. = FALSE)
  }
  if (length(x@annotations) > 0L && active_annotation(x) %in% names(x@annotations)) {
    ann <- x@annotations[[active_annotation(x)]]
    ann@nucleotide_seq <- value
    x@annotations[[active_annotation(x)]] <- ann
  }
  validObject(x)
  x
})
setReplaceMethod("nucleotide_seq", "SynFeatureAnnotation", function(x, value) {
  if (!is.null(value) && !methods::is(value, "DNAStringSet")) {
    stop("`nucleotide_seq<-` expects a DNAStringSet object or NULL.", call. = FALSE)
  }
  x@nucleotide_seq <- value
  validObject(x)
  x
})

setGeneric("protein_seq<-", function(x, value) {
  standardGeneric("protein_seq<-")
})
setReplaceMethod("protein_seq", "SynIndividual", function(x, value) {
  if (!is.null(value) && !methods::is(value, "AAStringSet")) {
    stop("`protein_seq<-` expects an AAStringSet object or NULL.", call. = FALSE)
  }
  if (length(x@annotations) > 0L && active_annotation(x) %in% names(x@annotations)) {
    ann <- x@annotations[[active_annotation(x)]]
    ann@protein_seq <- value
    x@annotations[[active_annotation(x)]] <- ann
  }
  validObject(x)
  x
})
setReplaceMethod("protein_seq", "SynFeatureAnnotation", function(x, value) {
  if (!is.null(value) && !methods::is(value, "AAStringSet")) {
    stop("`protein_seq<-` expects an AAStringSet object or NULL.", call. = FALSE)
  }
  x@protein_seq <- value
  validObject(x)
  x
})

setGeneric("seqinfo<-", function(x, value) standardGeneric("seqinfo<-"))
setReplaceMethod("seqinfo", "SynIndividual", function(x, value) {
  if (!is.null(value) && !methods::is(value, "Seqinfo")) {
    stop("`seqinfo<-` expects a Seqinfo object or NULL.", call. = FALSE)
  }
  x@seqinfo <- value
  validObject(x)
  x
})

setGeneric("feature_index<-", function(x, value) {
  standardGeneric("feature_index<-")
})
setReplaceMethod("feature_index", "SynIndividual", function(x, value) {
  if (length(x@annotations) > 0L && active_annotation(x) %in% names(x@annotations)) {
    ann <- x@annotations[[active_annotation(x)]]
    ann@feature_index <- value
    x@annotations[[active_annotation(x)]] <- ann
  }
  validObject(x)
  x
})
setReplaceMethod("feature_index", "SynFeatureAnnotation", function(x, value) {
  x@feature_index <- value
  validObject(x)
  x
})

setGeneric("syn_metadata<-", function(x, value) {
  standardGeneric("syn_metadata<-")
})
setReplaceMethod("syn_metadata", "SynIndividual", function(x, value) {
  if (!is.list(value)) {
    stop("`syn_metadata<-` expects a list.", call. = FALSE)
  }
  x@metadata <- value
  validObject(x)
  x
})

setGeneric("plot_cache<-", function(x, value) standardGeneric("plot_cache<-"))
setReplaceMethod("plot_cache", "SynIndividual", function(x, value) {
  if (!is.list(value)) {
    stop("`plot_cache<-` expects a list.", call. = FALSE)
  }
  x@plot_cache <- value
  validObject(x)
  x
})
setReplaceMethod("plot_cache", "SynAnnotation", function(x, value) {
  if (!is.list(value)) {
    stop("`plot_cache<-` expects a list.", call. = FALSE)
  }
  x@plot_cache <- value
  validObject(x)
  x
})

setGeneric("projected_domains<-", function(x, value) {
  standardGeneric("projected_domains<-")
})
setReplaceMethod("projected_domains", "SynIndividual", function(x, value) {
  if (!is.list(value)) {
    stop("`projected_domains<-` expects a list.", call. = FALSE)
  }
  bad_entries <- !vapply(
    value,
    function(tbl) is.data.frame(tbl) || methods::is(tbl, "DataFrame"),
    logical(1)
  )
  if (any(bad_entries)) {
    stop(
      "`projected_domains<-` expects a list of data.frame-like tables.",
      call. = FALSE
    )
  }
  x@projected_domains <- lapply(value, as.data.frame)
  validObject(x)
  x
})

#' Store a projected protein-domain table on a SynIndividual
#'
#' @param x A `SynIndividual` object.
#' @param projected A data frame returned by `project_domains_to_genome()`.
#' @param name Name used to store the projected table.
#'
#' @return An updated `SynIndividual` object.
#' @export
store_projected_domains <- function(x, projected, name = "last_projection") {
  if (!methods::is(x, "SynIndividual")) {
    stop("`store_projected_domains()` expects a SynIndividual object.", call. = FALSE)
  }
  if (!(is.data.frame(projected) || methods::is(projected, "DataFrame"))) {
    stop(
      "`projected` must be a data.frame-like object.",
      call. = FALSE
    )
  }
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    stop("`name` must be a single non-empty character value.", call. = FALSE)
  }

  stored <- projected_domains(x)
  stored[[name]] <- as.data.frame(projected)
  projected_domains(x) <- stored
  x
}
