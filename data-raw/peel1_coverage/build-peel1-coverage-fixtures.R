bam_dir <- Sys.getenv("GGEXON_TA_BAM_DIR")
source_gtf <- Sys.getenv("GGEXON_WS285_GTF")
output_dir <- file.path("inst", "extdata", "peel1_coverage")

if (!nzchar(bam_dir) || !dir.exists(bam_dir)) {
  stop("Set GGEXON_TA_BAM_DIR to the directory containing the four BAMs.")
}
if (!nzchar(source_gtf) || !file.exists(source_gtf)) {
  stop("Set GGEXON_WS285_GTF to the WS285 canonical geneset GTF.")
}

strains <- c("XZ1516", "ECA2091", "ECA701", "ECA2191")
gene_ids <- c(
  "WBGene00021464", "WBGene00021463",
  "WBGene00077563", "WBGene00021461"
)
chr <- "I"
window_start <- 2332338L
window_end <- 2373985L
bam_coverage_region_start <- window_start - 1L
bam_coverage <- Sys.getenv(
  "GGEXON_BAMCOVERAGE",
  unset = Sys.which("bamCoverage")
)
if (!nzchar(bam_coverage) || !file.exists(bam_coverage) ||
    file.access(bam_coverage, mode = 1L) != 0L) {
  stop("Set GGEXON_BAMCOVERAGE to an executable bamCoverage path.")
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

gtf_lines <- readLines(source_gtf, warn = FALSE)
header <- grepl("^#", gtf_lines)
keep_gene <- Reduce(`|`, lapply(gene_ids, grepl, x = gtf_lines, fixed = TRUE))
writeLines(
  gtf_lines[header | keep_gene],
  file.path(output_dir, "WS285.ugt31-zeel1-peel1-nekl1.gtf")
)

rows <- lapply(strains, function(strain) {
  bam <- file.path(bam_dir, paste0(strain, ".ChrI.ChrIII.ChrV.bam"))
  bai <- paste0(bam, ".bai")
  if (!file.exists(bam) || !file.exists(bai)) {
    stop("Missing BAM or index for ", strain)
  }

  bigwig_name <- paste0(strain, ".raw.bw")
  bigwig <- file.path(output_dir, bigwig_name)
  status <- system2(
    bam_coverage,
    c(
      "--bam", shQuote(bam),
      "--outFileName", shQuote(bigwig),
      "--outFileFormat", "bigwig",
      "--region", paste0(chr, ":", bam_coverage_region_start, ":", window_end),
      "--binSize", "1",
      "--normalizeUsing", "None"
    )
  )
  if (!identical(status, 0L) || !file.exists(bigwig)) {
    stop("bamCoverage failed for ", strain)
  }

  data.frame(
    strain = strain,
    source_bam = normalizePath(bam),
    bigwig = bigwig_name,
    chr = chr,
    start = window_start,
    end = window_end,
    bin_size = 1L,
    normalization = "None",
    bamCoverage_region = paste0(
      chr, ":", bam_coverage_region_start, ":", window_end
    ),
    bamCoverage_version = system2(bam_coverage, "--version", stdout = TRUE),
    checksum = unname(tools::md5sum(bigwig)),
    stringsAsFactors = FALSE
  )
})

utils::write.table(
  do.call(rbind, rows),
  file.path(output_dir, "manifest.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)
