# Reconstruct the original longest forward genomic alignment, offline.
# Run from the repository root after pkgload::load_all().
source_dir <- "data-raw/as3mt"
bundle_dir <- "inst/extdata/as3mt"
reference_start <- 102869470
query_start <- 104324156
regions <- c(102869750, 102870250) # E2/E3 plus intervening/flanking intron.

human <- as.character(Biostrings::readDNAStringSet(file.path(source_dir, "human.fa"))[[1]])
chimp <- as.character(Biostrings::readDNAStringSet(file.path(source_dir, "chimp.fa"))[[1]])
records <- strsplit(readLines(file.path(source_dir, "human_chimp.paf")), "\t")
records <- Filter(function(x) x[5] == "+", records)
r <- records[[which.max(vapply(records, function(x) as.numeric(x[11]), numeric(1)))]]
stopifnot(r[1] == "chimp", r[6] == "human", nchar(chimp) == as.numeric(r[2]), nchar(human) == as.numeric(r[7]))
cigar <- sub("^cg:Z:", "", grep("^cg:Z:", r, value = TRUE))
ops <- regmatches(cigar, gregexpr("[0-9]+[MID=X]", cigar))[[1]]
stopifnot(paste0(ops, collapse = "") == cigar)
t <- as.integer(r[8]) + 1L
q <- as.integer(r[3]) + 1L
hs <- qs <- character(length(ops))
for (i in seq_along(ops)) {
  op <- substring(ops[i], nchar(ops[i]))
  n <- as.integer(sub("[A-Z=]$", "", ops[i]))
  hs[i] <- if (op == "I") strrep("-", n) else substr(human, t, t + n - 1L)
  qs[i] <- if (op == "D") strrep("-", n) else substr(chimp, q, q + n - 1L)
  if (op != "I") t <- t + n
  if (op != "D") q <- q + n
}
stopifnot(t - 1L == as.integer(r[9]), q - 1L == as.integer(r[4]))
aligned <- c(human = paste0(hs, collapse = ""), chimp = paste0(qs, collapse = ""))
anchors <- data.frame(sequence = c("human", "chimp"),
  start = c(reference_start + as.integer(r[8]), query_start + as.integer(r[3])),
  strand = "+", track = c("human", "chimp"), chr = "chr10", y = 1.9)
all <- alignseq_data(aligned, anchors, "human")
h <- all[all$sequence == "human", ]
stopifnot(sum(h$event_type == "match") == as.integer(r[10]),
  sum(h$event_type == "substitution") == 468,
  sum(h$event_type == "insertion") == 146, sum(h$event_type == "deletion") == 489)
events <- h[!is.na(h$event_id) & !duplicated(h$event_id),
  c("event_id", "event_type", "event_length", "reference_x", "column")]
utils::write.table(events, file.path(source_dir, "alignseq-events.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
print(events[events$reference_x >= regions[1] & events$reference_x <= regions[2], ])
# Explicit user-editable windows: one substitution and one insertion near E2/E3.
# Coordinates are frozen after examining the event table, not ranked by biological importance.
details <- data.frame(start = c(102869774.5, 102869814), end = c(102869774.5, 102869814))
payload <- list(aligned = aligned, anchors = anchors, reference = "human",
  regions = regions, details = details, flank = 3L,
  provenance = list(source = "human_chimp.paf, longest forward gapped block",
    paf_record = paste(r, collapse = "\t"), cigar = cigar,
    reference_assembly = "GRCh38", query_assembly = "Pan_tro_3.0",
    human_fasta_md5 = unname(tools::md5sum(file.path(source_dir, "human.fa"))),
    chimp_fasta_md5 = unname(tools::md5sum(file.path(source_dir, "chimp.fa"))),
    scope = "One alignment block, not a genome-wide variant set; no biological importance ranking"))
saveRDS(payload, file.path(bundle_dir, "aligned-dna.rds"), version = 2)
stopifnot(file.copy(file.path(source_dir, "human_chimp.paf"), bundle_dir, overwrite = TRUE))
