#' Bioconductor class unions used by ggexon S4 classes
#'
#' These unions let optional S4 slots accept either `NULL` or the referenced
#' Bioconductor class.
#'
#' @name SynBioc-unions
#' @keywords internal
#' @importClassesFrom GenomicRanges GRanges
#' @importClassesFrom Biostrings DNAStringSet AAStringSet
NULL

setClassUnion("NULLOrGRanges", c("NULL", "GRanges"))
# GenomeInfoDb's Seqinfo class is not exported consistently across all
# Bioconductor releases used on CI. Keep the slot union permissive and enforce
# Seqinfo-or-NULL in class validity and replacement methods.
setClassUnion("NULLOrSeqinfo", c("NULL", "ANY"))
setClassUnion("NULLOrDNAStringSet", c("NULL", "DNAStringSet"))
setClassUnion("NULLOrAAStringSet", c("NULL", "AAStringSet"))
