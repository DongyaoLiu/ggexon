# Generate the bundled HOXA/Hoxa synteny demo from Ensembl release 115.
#
# The script downloads Ensembl GTF files, extracts annotated HOXA genes for the
# selected species, normalizes them into plot-ready TSV files, and records
# source provenance. It intentionally stores only small derived tables under
# inst/extdata; downloaded GTF files remain in a local cache or temp directory.

release <- "115"

`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) > 0L) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath("data-raw/hoxa_ensembl115/build-hoxa-demo.R", mustWork = FALSE)
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
if (!dir.exists(file.path(repo_root, "inst"))) {
  repo_root <- normalizePath(getwd(), mustWork = TRUE)
}

out_dir <- file.path(repo_root, "inst", "extdata", "hoxa_ensembl115")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cache_dir <- Sys.getenv("GGEXON_HOXA_CACHE", unset = file.path(tempdir(), "ggexon_hoxa_ensembl115"))
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

species <- data.frame(
  species = c("human", "macaque", "mouse", "chicken", "anole"),
  display_name = c("Human", "Rhesus macaque", "Mouse", "Chicken", "Green anole"),
  scientific_name = c(
    "Homo sapiens",
    "Macaca mulatta",
    "Mus musculus",
    "Gallus gallus",
    "Anolis carolinensis"
  ),
  ensembl_name = c(
    "homo_sapiens",
    "macaca_mulatta",
    "mus_musculus",
    "gallus_gallus",
    "anolis_carolinensis"
  ),
  assembly = c(
    "GRCh38",
    "Mmul_10",
    "GRCm39",
    "bGalGal1.mat.broiler.GRCg7b",
    "AnoCar2.0v2"
  ),
  gtf_file = c(
    "Homo_sapiens.GRCh38.115.chr.gtf.gz",
    "Macaca_mulatta.Mmul_10.115.chr.gtf.gz",
    "Mus_musculus.GRCm39.115.chr.gtf.gz",
    "Gallus_gallus.bGalGal1.mat.broiler.GRCg7b.115.chr.gtf.gz",
    "Anolis_carolinensis.AnoCar2.0v2.115.gtf.gz"
  ),
  gtf_scope = c("chr", "chr", "chr", "chr", "full"),
  source_note = c(
    "HOXA genes are on chromosome 7 in the chromosome-only GTF.",
    "HOXA genes are on chromosome 3 in the chromosome-only GTF.",
    "Hoxa genes are on chromosome 6 in the chromosome-only GTF.",
    "HOXA genes are on chromosome 2 in the chromosome-only GTF.",
    "HOXA genes are on scaffold GL343275.1 in the full GTF; they are absent from the chromosome-only GTF."
  ),
  stringsAsFactors = FALSE
)

species$source_url <- paste0(
  "https://ftp.ensembl.org/pub/release-", release, "/gtf/",
  species$ensembl_name, "/", species$gtf_file
)
species$ensembl_release <- release
species$species_order <- seq_len(nrow(species))

download_gtf <- function(url, cache_dir) {
  dest <- file.path(cache_dir, basename(url))
  if (!file.exists(dest) || file.info(dest)$size == 0L) {
    message("Downloading ", url)
    utils::download.file(url, dest, mode = "wb", quiet = FALSE)
  }
  dest
}

parse_attributes <- function(x) {
  attrs <- strsplit(x, ";", fixed = TRUE)[[1L]]
  attrs <- trimws(attrs)
  attrs <- attrs[nzchar(attrs)]
  keys <- sub(" .*", "", attrs)
  values <- sub("^[^ ]+ ", "", attrs)
  values <- gsub('^"|"$', "", values)
  stats::setNames(values, keys)
}

attr_value <- function(attrs, key, default = NA_character_) {
  if (key %in% names(attrs)) {
    attrs[[key]]
  } else {
    default
  }
}

read_hoxa_gene_rows <- function(path, species_row) {
  con <- gzfile(path, open = "rt")
  on.exit(close(con), add = TRUE)

  rows <- list()
  chunk <- 50000L
  repeat {
    lines <- readLines(con, n = chunk, warn = FALSE)
    if (length(lines) == 0L) {
      break
    }
    lines <- lines[!startsWith(lines, "#")]
    if (length(lines) == 0L) {
      next
    }
    fields <- strsplit(lines, "\t", fixed = TRUE)
    keep <- vapply(fields, function(x) length(x) >= 9L && identical(x[[3L]], "gene"), logical(1))
    if (!any(keep)) {
      next
    }
    gene_fields <- fields[keep]
    for (field in gene_fields) {
      attrs <- parse_attributes(field[[9L]])
      gene_name <- attr_value(attrs, "gene_name", attr_value(attrs, "gene_id"))
      group <- toupper(gene_name)
      if (!grepl("^HOXA[0-9]+$", group)) {
        next
      }
      rows[[length(rows) + 1L]] <- data.frame(
        species = species_row$species,
        display_name = species_row$display_name,
        scientific_name = species_row$scientific_name,
        assembly = species_row$assembly,
        ensembl_release = species_row$ensembl_release,
        source_url = species_row$source_url,
        source_file = species_row$gtf_file,
        gtf_scope = species_row$gtf_scope,
        seqname = field[[1L]],
        source = field[[2L]],
        feature = field[[3L]],
        genomic_start = as.integer(field[[4L]]),
        genomic_end = as.integer(field[[5L]]),
        score = field[[6L]],
        genomic_strand = field[[7L]],
        phase = field[[8L]],
        gene_id = attr_value(attrs, "gene_id"),
        gene_version = attr_value(attrs, "gene_version"),
        gene_name = gene_name,
        gene_source = attr_value(attrs, "gene_source"),
        gene_biotype = attr_value(attrs, "gene_biotype"),
        hox_group = group,
        hox_number = as.integer(sub("^HOXA", "", group)),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0L) {
    stop("No HOXA genes found for ", species_row$species, " in ", path, call. = FALSE)
  }

  out <- do.call(rbind, rows)
  out[order(out$hox_number, out$genomic_start), , drop = FALSE]
}

flip_strand <- function(x) {
  ifelse(x == "+", "-", ifelse(x == "-", "+", x))
}

orient_gene_coordinates <- function(gene_rows, padding = 100000L) {
  split_rows <- split(gene_rows, gene_rows$species)
  out <- lapply(split_rows, function(df) {
    df <- df[order(df$genomic_start, df$genomic_end), , drop = FALSE]
    window_start <- max(1L, min(df$genomic_start) - padding)
    window_end <- max(df$genomic_end) + padding

    hoxa1 <- df[df$hox_group == "HOXA1", , drop = FALSE]
    hoxa13 <- df[df$hox_group == "HOXA13", , drop = FALSE]
    reverse_for_display <- nrow(hoxa1) > 0L &&
      nrow(hoxa13) > 0L &&
      hoxa1$genomic_start[[1L]] < hoxa13$genomic_start[[1L]]

    if (reverse_for_display) {
      xmin <- window_end - df$genomic_end + 1L
      xmax <- window_end - df$genomic_start + 1L
      strand <- flip_strand(df$genomic_strand)
      display_orientation <- "reversed_to_hoxa13_left"
    } else {
      xmin <- df$genomic_start - window_start + 1L
      xmax <- df$genomic_end - window_start + 1L
      strand <- df$genomic_strand
      display_orientation <- "forward_hoxa13_left"
    }

    df$source_seqname <- unique(df$seqname)[[1L]]
    df$window_start <- window_start
    df$window_end <- window_end
    df$window_padding_bp <- padding
    df$display_orientation <- display_orientation
    df$xmin <- as.integer(pmin(xmin, xmax))
    df$xmax <- as.integer(pmax(xmin, xmax))
    df$y <- 0.4
    df$strand <- strand
    df$track <- df$species
    df$individual <- df$species
    df$label <- df$hox_group
    df$gene <- df$hox_group
    df$genomic_xmin <- df$genomic_start
    df$genomic_xmax <- df$genomic_end
    df
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out[order(match(out$species, species$species), out$hox_number), , drop = FALSE]
}

build_links <- function(genes, species_order) {
  pairs <- data.frame(
    tspecies = species_order[-length(species_order)],
    qspecies = species_order[-1L],
    stringsAsFactors = FALSE
  )

  rows <- list()
  for (i in seq_len(nrow(pairs))) {
    target <- genes[genes$species == pairs$tspecies[[i]], , drop = FALSE]
    query <- genes[genes$species == pairs$qspecies[[i]], , drop = FALSE]
    shared <- intersect(target$hox_group, query$hox_group)
    shared <- shared[order(as.integer(sub("^HOXA", "", shared)))]
    for (group in shared) {
      trow <- target[target$hox_group == group, , drop = FALSE][1L, ]
      qrow <- query[query$hox_group == group, , drop = FALSE][1L, ]
      rows[[length(rows) + 1L]] <- data.frame(
        track = paste0("link_", pairs$tspecies[[i]], "_", pairs$qspecies[[i]]),
        tspecies = pairs$tspecies[[i]],
        tchr = trow$source_seqname,
        tstart = trow$xmin,
        tend = trow$xmax,
        qspecies = pairs$qspecies[[i]],
        qchr = qrow$source_seqname,
        qstart = qrow$xmin,
        qend = qrow$xmax,
        strand = ifelse(trow$strand == qrow$strand, "+", "-"),
        hox_group = group,
        hox_number = trow$hox_number,
        group = paste(pairs$tspecies[[i]], pairs$qspecies[[i]], group, sep = "_"),
        target_gene_id = trow$gene_id,
        query_gene_id = qrow$gene_id,
        target_gene_name = trow$gene_name,
        query_gene_name = qrow$gene_name,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0L) {
    stop("No shared HOXA genes found for adjacent species links.", call. = FALSE)
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

build_homology <- function(genes, reference_species = "human") {
  ref <- genes[genes$species == reference_species, , drop = FALSE]
  query <- genes[genes$species != reference_species, , drop = FALSE]
  query <- query[query$hox_group %in% ref$hox_group, , drop = FALSE]
  ref_idx <- match(query$hox_group, ref$hox_group)

  out <- data.frame(
    reference_species = reference_species,
    query_species = query$species,
    query_gene = query$gene_id,
    query_gene_name = query$gene_name,
    reference_gene = query$hox_group,
    reference_gene_id = ref$gene_id[ref_idx],
    reference_gene_name = ref$gene_name[ref_idx],
    hox_group = query$hox_group,
    hox_number = query$hox_number,
    stringsAsFactors = FALSE
  )
  out[order(match(out$query_species, species$species), out$hox_number), , drop = FALSE]
}

gff3_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  utils::URLencode(x, reserved = TRUE)
}

gff3_attributes <- function(...) {
  attrs <- list(...)
  paste(paste0(names(attrs), "=", vapply(attrs, gff3_escape, character(1))), collapse = ";")
}

write_hoxa_gff3 <- function(genes, out_dir) {
  annotation_dir <- file.path(out_dir, "annotations")
  dir.create(annotation_dir, showWarnings = FALSE, recursive = TRUE)

  for (sp in species$species) {
    df <- genes[genes$species == sp, , drop = FALSE]
    df <- df[order(df$genomic_start, df$genomic_end), , drop = FALSE]
    seqname <- unique(df$seqname)[[1L]]
    lines <- c(
      "##gff-version 3",
      sprintf(
        "##sequence-region %s %d %d",
        seqname,
        min(df$genomic_start),
        max(df$genomic_end)
      )
    )

    feature_lines <- vapply(seq_len(nrow(df)), function(i) {
      row <- df[i, , drop = FALSE]
      attrs <- gff3_attributes(
        ID = row$gene_id,
        Name = row$gene_name,
        gene_id = row$gene_id,
        gene_name = row$gene_name,
        hox_group = row$hox_group,
        hox_number = row$hox_number,
        reference_gene = row$reference_gene,
        display_xmin = row$xmin,
        display_xmax = row$xmax,
        display_orientation = row$display_orientation
      )
      paste(
        row$seqname,
        row$source,
        "gene",
        row$genomic_start,
        row$genomic_end,
        row$score,
        row$genomic_strand,
        row$phase,
        attrs,
        sep = "\t"
      )
    }, character(1))

    writeLines(c(lines, feature_lines), file.path(annotation_dir, paste0(sp, ".gff3")))
  }

  annotation_dir
}

gtf_paths <- vapply(species$source_url, download_gtf, character(1), cache_dir = cache_dir)
gene_rows <- do.call(rbind, lapply(seq_len(nrow(species)), function(i) {
  read_hoxa_gene_rows(gtf_paths[[i]], species[i, , drop = FALSE])
}))
genes <- orient_gene_coordinates(gene_rows, padding = 100000L)
genes$reference_gene <- genes$hox_group
links <- build_links(genes, species$species)
homology <- build_homology(genes, reference_species = "human")
annotation_dir <- write_hoxa_gff3(genes, out_dir)

species_out <- species
species_out$source_seqname <- vapply(species_out$species, function(sp) {
  unique(genes$source_seqname[genes$species == sp])[[1L]]
}, character(1))
species_out$hoxa_gene_count <- vapply(species_out$species, function(sp) {
  sum(genes$species == sp)
}, integer(1))
species_out$hoxa_groups <- vapply(species_out$species, function(sp) {
  paste(genes$hox_group[genes$species == sp], collapse = ",")
}, character(1))

utils::write.table(
  genes,
  file = file.path(out_dir, "hoxa_genes.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)
utils::write.table(
  links,
  file = file.path(out_dir, "hoxa_links.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)
utils::write.table(
  species_out,
  file = file.path(out_dir, "hoxa_species.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)
utils::write.table(
  homology,
  file = file.path(out_dir, "hoxa_homology.tsv"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)

message("Wrote:")
message("  ", file.path(out_dir, "hoxa_genes.tsv"))
message("  ", file.path(out_dir, "hoxa_links.tsv"))
message("  ", file.path(out_dir, "hoxa_species.tsv"))
message("  ", file.path(out_dir, "hoxa_homology.tsv"))
message("  ", annotation_dir)
