# Re-encode the verified, one-based closed GTEx interval snapshot for native
# SynBigWigAnnotation dispatch. No source intervals or values are changed.
bundle <- "inst/extdata/as3mt"
source <- utils::read.delim(file.path(bundle, "human-rnaseq-GTEX-15G19.tsv"))
signal <- GenomicRanges::makeGRangesFromDataFrame(source, keep.extra.columns = TRUE)
# This source chromosome length is also checked in prepare-gtex-rnaseq.py.
GenomeInfoDb::seqlengths(signal) <- c(chr10 = 133797422L)
GenomeInfoDb::genome(signal) <- "GRCh38"
destination <- file.path(bundle, "human-rnaseq-GTEX-15G19.bw")
rtracklayer::export(signal, destination, format = "BigWig")
roundtrip <- as.data.frame(rtracklayer::import(destination))
stopifnot(isTRUE(all.equal(roundtrip[, c("start", "end", "score")],
  source[, c("start", "end", "score")], check.attributes = FALSE)))
cat("Verified", nrow(source), "unchanged GTEx intervals in", destination, "\n")
