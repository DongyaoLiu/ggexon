# Convert the verified table bundle to native inputs for the object-first tutorial.
# Run from the repository root on macOS/Linux (rtracklayer BigWig support).
suppressPackageStartupMessages({
  library(GenomicRanges)
  library(rtracklayer)
})
if (.Platform$OS.type == "windows") stop("Prepare BigWigs on macOS or Linux.")
source("inst/extdata/dox_synteny/load-dox-example.R")
base <- "inst/extdata/dox_synteny"
e <- load_dox_example(base)
out <- file.path(base, "objects")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
lengths <- c(Dmau = 23222241L, Dsim = 23123156L, Dsech = 22909512L)

for (id in e$windows$species) {
  genes <- e$genes[e$genes$species == id, ]
  pieces <- e$exons[e$exons$species == id, ]
  rows <- rbind(
    data.frame(start = genes$start, end = genes$end, strand = genes$strand,
      type = "gene", gene_id = genes$gene_id, gene_name = genes$gene_name,
      transcript_id = NA_character_),
    data.frame(start = genes$start, end = genes$end, strand = genes$strand,
      type = "transcript", gene_id = genes$gene_id, gene_name = genes$gene_name,
      transcript_id = genes$transcript_id),
    data.frame(start = pieces$xmin, end = pieces$xmax, strand = pieces$strand,
      type = pieces$type, gene_id = genes$gene_id[match(pieces$transcripts, genes$transcript_id)],
      gene_name = pieces$gene_name, transcript_id = pieces$transcripts)
  )
  rows <- rows[order(rows$start, rows$end, rows$type), ]
  gr <- GRanges("X", IRanges(rows$start, rows$end), strand = rows$strand,
    seqinfo = Seqinfo("X", seqlengths = lengths[[id]]))
  mcols(gr) <- S4Vectors::DataFrame(rows[, c("type", "gene_id", "gene_name", "transcript_id")])
  mcols(gr)$source <- "ggexon_curated_Dox_demo"
  mcols(gr)$phase <- NA_integer_ # Phase was not reconstructed for these display models.
  export(gr, file.path(out, paste0(id, ".gtf")), format = "gtf")

  for (strand in c("+", "-")) {
    s <- e$coverage[e$coverage$species == id & e$coverage$strand == strand, ]
    s <- s[order(s$start, s$end), ]
    gr <- GRanges("X", IRanges(s$start, s$end), score = abs(s$score),
      seqinfo = Seqinfo("X", seqlengths = lengths[[id]]))
    stopifnot(all(countOverlaps(gr, gr) == 1L))
    export(gr, file.path(out, paste0(id,
      if (strand == "+") ".RNAplus.bw" else ".RNAminus.bw")), format = "BigWig")
  }
}

for (pair in list(c("Dmau", "Dsim"), c("Dsim", "Dsech"))) {
  blocks <- e$links[e$links$tspecies == pair[1] & e$links$qspecies == pair[2], ]
  # PAF starts are zero-based; ends stay exclusive. MapQ 255 means unknown.
  paf <- data.frame(qname = "X", qlen = lengths[[pair[2]]],
    qstart = blocks$qstart - 1L, qend = blocks$qend, strand = blocks$strand,
    tname = "X", tlen = lengths[[pair[1]]], tstart = blocks$tstart - 1L,
    tend = blocks$tend, nmatch = blocks$nmatch, alen = blocks$length, mapq = 255L)
  write.table(paf, file.path(out, paste0(paste(pair, collapse = "_"), ".paf")),
    sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
}

records <- function(paths, root) data.frame(file = substring(paths, nchar(root) + 2L),
  md5 = unname(tools::md5sum(paths)))
inputs <- file.path(base, c("windows.tsv", "genes.tsv", "features.tsv", "links.tsv", "coverage.tsv.gz"))
outputs <- list.files(out, pattern = "\\.(gtf|paf|bw)$", full.names = TRUE)
manifest <- list(
  description = "Native file views of the verified Dox tables; no new alignment or signal estimation",
  coordinates = "GTF: 1-based closed; PAF source: 0-based half-open; BigWig: standard interval encoding; X is a seqname alias",
  annotation = "Gene/transcript spans plus disjoint mapped CDS/UTR pieces; Dox has coding exons only",
  signal = "Plus and reverse-magnitude files store nonnegative abs(source score), without smoothing or normalization",
  chromosome_lengths = as.list(lengths),
  inputs = records(inputs, base), outputs = records(outputs, out),
  versions = list(R = as.character(getRversion()), rtracklayer = as.character(packageVersion("rtracklayer")))
)
jsonlite::write_json(manifest, file.path(out, "manifest.json"), pretty = TRUE, auto_unbox = TRUE)
cat("Prepared", length(outputs), "native files in", out, "\n")
