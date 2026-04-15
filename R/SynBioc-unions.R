#' Bioconductor class unions used by ggexon S4 classes
#'
#' These unions let optional S4 slots accept either `NULL` or the referenced
#' Bioconductor class.
#'
#' @name SynBioc-unions
#' @keywords internal
#' @importClassesFrom GenomicRanges GRanges
#' @importClassesFrom GenomeInfoDb Seqinfo
#' @importClassesFrom Biostrings DNAStringSet AAStringSet
NULL

setClassUnion("NULLOrGRanges", c("NULL", "GRanges"))
setClassUnion("NULLOrSeqinfo", c("NULL", "Seqinfo"))
setClassUnion("NULLOrDNAStringSet", c("NULL", "DNAStringSet"))
setClassUnion("NULLOrAAStringSet", c("NULL", "AAStringSet"))
