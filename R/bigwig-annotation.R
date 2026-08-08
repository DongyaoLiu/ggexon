#' Query a region-backed annotation
#'
#' @param x A region-backed `SynAnnotation` object.
#' @param region A length-one `GenomicRanges::GRanges` query.
#' @param ... Additional arguments passed to a class method.
#' @return A class-specific region query result.
#' @export
setGeneric(
  "query_annotation",
  function(x, region, ...) standardGeneric("query_annotation")
)

.validate_annotation_region <- function(region) {
  if (!methods::is(region, "GRanges") || length(region) != 1L) {
    cli::cli_abort("{.arg region} must be a length-one {.cls GRanges}.")
  }
  start <- BiocGenerics::start(region)
  end <- BiocGenerics::end(region)
  if (!is.finite(start) || !is.finite(end) || start < 1L || end < start) {
    cli::cli_abort("{.arg region} must contain valid positive coordinates.")
  }
  seqname <- as.character(GenomeInfoDb::seqnames(region))
  if (length(seqname) != 1L || is.na(seqname) || !nzchar(seqname)) {
    cli::cli_abort("{.arg region} must contain one non-empty chromosome name.")
  }
  region
}

.bigwig_query_window <- function(region) {
  paste0(
    as.character(GenomeInfoDb::seqnames(region)),
    ":",
    BiocGenerics::start(region),
    "-",
    BiocGenerics::end(region)
  )
}

.abort_bigwig_query_stage <- function(stage, cnd, x, path, region) {
  window <- .bigwig_query_window(region)
  cli::cli_abort(
    "Could not {stage} BigWig annotation {.val {annotation_name(x)}} at {.file {path}} for window {.val {window}}: {conditionMessage(cnd)}",
    parent = cnd
  )
}

#' @rdname query_annotation
setMethod(
  "query_annotation",
  "SynBigWigAnnotation",
  function(x, region, ...) {
    region <- .validate_annotation_region(region)
    path <- source_file(x)
    window <- .bigwig_query_window(region)
    if (!file.exists(path)) {
      cli::cli_abort(
        "BigWig annotation {.val {annotation_name(x)}} does not exist at {.file {path}} for window {.val {window}}."
      )
    }

    bw <- tryCatch(
      rtracklayer::BigWigFile(path),
      error = function(cnd) {
        .abort_bigwig_query_stage("open", cnd, x, path, region)
      }
    )
    available <- tryCatch(
      suppressWarnings(
        GenomeInfoDb::seqnames(GenomeInfoDb::seqinfo(bw))
      ),
      error = function(cnd) {
        .abort_bigwig_query_stage("inspect sequences in", cnd, x, path, region)
      }
    )
    requested <- as.character(GenomeInfoDb::seqnames(region))
    if (!requested %in% available) {
      cli::cli_abort(
        "Chromosome {.val {requested}} is absent from BigWig annotation {.val {annotation_name(x)}} at {.file {path}} for window {.val {window}}."
      )
    }

    tryCatch(
      suppressWarnings(
        rtracklayer::import.bw(bw, which = region, as = "GRanges")
      ),
      error = function(cnd) {
        .abort_bigwig_query_stage("import", cnd, x, path, region)
      }
    )
  }
)

#' Query BigWig signal
#'
#' @param x A `SynBigWigAnnotation` object.
#' @param chr Chromosome name.
#' @param start Start coordinate.
#' @param end End coordinate.
#' @param ... Additional arguments passed to [query_annotation()].
#'
#' @return A `GenomicRanges::GRanges` object with overlapping signal records.
#' @export
query_signal <- function(x, chr, start, end, ...) {
  if (!methods::is(x, "SynBigWigAnnotation")) {
    cli::cli_abort("{.fn query_signal} expects a {.cls SynBigWigAnnotation}.")
  }
  if (!is.character(chr) || length(chr) != 1L || is.na(chr) || !nzchar(chr)) {
    cli::cli_abort("{.arg chr} must be one non-empty chromosome name.")
  }
  start <- .validate_query_signal_coordinate(start, "start")
  end <- .validate_query_signal_coordinate(end, "end")
  if (end < start) {
    cli::cli_abort("{.arg end} must be greater than or equal to {.arg start}.")
  }
  region <- GenomicRanges::GRanges(
    chr,
    IRanges::IRanges(start, end)
  )
  query_annotation(x, region, ...)
}

.validate_query_signal_coordinate <- function(value, arg) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value)) {
    cli::cli_abort("{.arg {arg}} must be one finite, positive whole-number coordinate.")
  }
  if (value <= 0 || value != trunc(value)) {
    cli::cli_abort("{.arg {arg}} must be one positive whole-number coordinate.")
  }
  if (value > .Machine$integer.max) {
    cli::cli_abort("{.arg {arg}} must fit in the supported integer coordinate range.")
  }
  as.integer(value)
}
