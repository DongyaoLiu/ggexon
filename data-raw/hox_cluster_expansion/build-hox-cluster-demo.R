# Build the bundled vertebrate HOX-cluster expansion tutorial dataset.
#
# Sources are pinned to Ensembl release 116 for the five vertebrates and
# Ensembl Metazoa release 63 for Branchiostoma lanceolatum (BraLan2). The
# script selects, for every annotated protein-coding Hox gene, the coding
# transcript with the greatest genomic span among transcripts that have a
# usable CDS and strand, after excluding three curated unsafe merged gar
# transcripts. Complete explicit start_codon and stop_codon features
# supply codon centers when present. Otherwise the anchors fall back to the
# second and penultimate bases, respectively, of the CDS in transcription
# order. Ties prefer an Ensembl-canonical transcript and then the
# lexicographically first stable transcript ID.
#
# Run from any directory with:
#   Rscript data-raw/hox_cluster_expansion/build-hox-cluster-demo.R
#
# Set HOX_GTF_CACHE to an existing directory containing files named
# human.gtf.gz, mouse.gtf.gz, chicken.gtf.gz, gar.gtf.gz, zebrafish.gtf.gz,
# and amphioxus.gtf.gz to reuse a local cache.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) > 0L) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath(
    "data-raw/hox_cluster_expansion/build-hox-cluster-demo.R",
    mustWork = FALSE
  )
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
if (!dir.exists(file.path(repo_root, "inst"))) {
  repo_root <- normalizePath(getwd(), mustWork = TRUE)
}

out_dir <- file.path(repo_root, "inst", "extdata", "hox_cluster_expansion")
annotation_dir <- file.path(out_dir, "annotations")
default_download_dir <- file.path(
  repo_root,
  "data-raw",
  "hox_cluster_expansion",
  "downloads"
)
download_dir <- Sys.getenv("HOX_GTF_CACHE", unset = default_download_dir)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(annotation_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

species <- data.frame(
  species = c("human", "mouse", "chicken", "gar", "zebrafish", "amphioxus"),
  display_name = c(
    "Human", "Mouse", "Chicken", "Spotted gar", "Zebrafish", "Amphioxus"
  ),
  scientific_name = c(
    "Homo sapiens",
    "Mus musculus",
    "Gallus gallus",
    "Lepisosteus oculatus",
    "Danio rerio",
    "Branchiostoma lanceolatum"
  ),
  source_database = c(rep("Ensembl", 5L), "Ensembl Metazoa"),
  release = c(rep(116L, 5L), 63L),
  assembly = c(
    "GRCh38",
    "GRCm39",
    "bGalGal1.mat.broiler.GRCg7b",
    "LepOcu1",
    "GRCz11",
    "BraLan2"
  ),
  assembly_accession = c(
    "GCA_000001405.29",
    "GCA_000001635.9",
    "GCA_016699485.1",
    "GCA_000242695.1",
    "GCA_000002035.4",
    "GCA_900088365.1"
  ),
  source_file = c(
    "Homo_sapiens.GRCh38.116.chr.gtf.gz",
    "Mus_musculus.GRCm39.116.chr.gtf.gz",
    "Gallus_gallus.bGalGal1.mat.broiler.GRCg7b.116.chr.gtf.gz",
    "Lepisosteus_oculatus.LepOcu1.116.chr.gtf.gz",
    "Danio_rerio.GRCz11.116.chr.gtf.gz",
    "Branchiostoma_lanceolatum.BraLan2.63.gtf.gz"
  ),
  local_file = paste0(
    c("human", "mouse", "chicken", "gar", "zebrafish", "amphioxus"),
    ".gtf.gz"
  ),
  source_url = c(
    paste0(
      "https://ftp.ensembl.org/pub/release-116/gtf/homo_sapiens/",
      "Homo_sapiens.GRCh38.116.chr.gtf.gz"
    ),
    paste0(
      "https://ftp.ensembl.org/pub/release-116/gtf/mus_musculus/",
      "Mus_musculus.GRCm39.116.chr.gtf.gz"
    ),
    paste0(
      "https://ftp.ensembl.org/pub/release-116/gtf/gallus_gallus/",
      "Gallus_gallus.bGalGal1.mat.broiler.GRCg7b.116.chr.gtf.gz"
    ),
    paste0(
      "https://ftp.ensembl.org/pub/release-116/gtf/lepisosteus_oculatus/",
      "Lepisosteus_oculatus.LepOcu1.116.chr.gtf.gz"
    ),
    paste0(
      "https://ftp.ensembl.org/pub/release-116/gtf/danio_rerio/",
      "Danio_rerio.GRCz11.116.chr.gtf.gz"
    ),
    paste0(
      "https://ftp.ensemblgenomes.ebi.ac.uk/pub/metazoa/release-63/gtf/",
      "branchiostoma_lanceolatum/",
      "Branchiostoma_lanceolatum.BraLan2.63.gtf.gz"
    )
  ),
  sha256 = c(
    "f101fe4b1a350deaf26ab04bb565ba3e638e467fdc14c5c20b989b03edc87e05",
    "b13bd6454a48c38d533e8df31e4f807d0ac93d1b23322921703b971e68cbe88f",
    "786501f79c39e1b2b9547f2bf033f87e43feb5a443e1d41d5135c2e05f238c6c",
    "892b4366b849d2f79851dc92344d65a14e7440700da3ecebc90c14097002013b",
    "fdca8df68935081e947e970a0b782f03e326446dc833f4bcf960f8febef629b2",
    "b4c69024b22cb41469164515d58fb41a639bd9862b9eb32b39ef0c9a3a93978f"
  ),
  retrieved_on = "2026-07-20",
  stringsAsFactors = FALSE
)
species$species_order <- seq_len(nrow(species))

# BraLan2 does not attach gene symbols to these models. The single Hox locus
# is the run of same-strand homeobox-containing models on Sc0000000. Mapping
# follows physical collinearity from the 3-prime Hox1 end to the 5-prime
# Hox15 end. No BraLan2 protein-coding model occurs in the Hox13 position.
# BL02747 (Hox7) and BL14546 (Hox12) are part of the collinear run even though
# release-63 BioMart does not attach IPR001356 to their selected translations.
amphioxus_mapping <- data.frame(
  gene_id = c(
    "BL06042", "BL11262", "BL11259", "BL01764", "BL22794",
    "BL02721", "BL02747", "BL01142", "BL02690", "BL01497",
    "BL11265", "BL14546", "BL01409", "BL12289"
  ),
  hox_number = c(1:12, 14L, 15L),
  gene_symbol = paste0("Hox", c(1:12, 14L, 15L)),
  release63_biomart_IPR001356 = !c(1:12, 14L, 15L) %in% c(7L, 12L),
  mapping_method = "manual_collinear_order_on_BraLan2_Sc0000000",
  mapping_note = c(
    rep(
      paste0(
        "Same-strand model in the single BraLan2 Hox cluster; assigned by ",
        "physical order."
      ),
      12L
    ),
    paste0(
      "Same-strand model after the unannotated Hox13 position; assigned ",
      "Hox14 by physical order."
    ),
    paste0(
      "Terminal same-strand model; assigned Hox15 by physical order and ",
      "the documented 15-gene Branchiostoma complement."
    )
  ),
  stringsAsFactors = FALSE
)

# Some release-116 Hox models are protein-coding but lack a GTF gene_name.
# Keep every such rescue explicit: stable ID, cluster position, and the
# evidence used to assign the paralogy group are reviewable here and in the
# generated manual_hox_mapping.tsv. The large gar model
# ENSLOCG00000011824 is deliberately absent: it is a 13-CDS, 673-aa chimeric
# prediction spanning several expected HoxA positions and cannot safely be
# assigned to one Hox slot.
manual_gene_mapping <- data.frame(
  species = c("zebrafish", rep("gar", 7L)),
  gene_id = c(
    "ENSDARG00000100358",
    "ENSLOCG00000011830", "ENSLOCG00000011842",
    "ENSLOCG00000006407",
    "ENSLOCG00000007019", "ENSLOCG00000007028",
    "ENSLOCG00000007085", "ENSLOCG00000007106"
  ),
  gene_symbol = c(
    "hoxa4a",
    "hoxa10b", "hoxa13b", "hoxc3",
    "hoxd1", "hoxd2", "hoxd8", "hoxd9a"
  ),
  hox_number = c(4L, 10L, 13L, 3L, 1L, 2L, 8L, 9L),
  cluster_family = c("A", "A", "A", "C", "D", "D", "D", "D"),
  cluster = c("AA", "A", "A", "C", "D", "D", "D", "D"),
  matrix_row = c("zebrafish_a", rep("gar", 7L)),
  matrix_column = c("A", "A", "A", "C", "D", "D", "D", "D"),
  xref_display_id = c(
    "published_previous_ID_ENSDARG00000057724",
    "hoxa10b", "hoxa13b", "LOC102687275",
    "LOC102685063", "LOC102685261", "LOC102686566", "hoxd9a"
  ),
  mapping_method = c(
    "manual_stable_ID_history_and_cluster_collinearity",
    rep("manual_Ensembl_xref_and_cluster_collinearity", 7L)
  ),
  mapping_note = c(
    paste0(
      "Unnamed Ensembl-116 Havana model at the expected HoxAa4 position; ",
      "the published hoxa4a model ENSDARG00000057724 maps to this locus."
    ),
    "Ensembl external xref hoxa10b; collinear HoxA10 position.",
    "Ensembl external xref hoxa13b; collinear HoxA13 position.",
    paste0(
      "Ensembl external xref describes a Hox-D3-like protein; its physical ",
      "position inside the HoxC cluster fixes the assignment as HoxC3."
    ),
    "Ensembl external xref homeobox protein Hox-D1; collinear HoxD1 position.",
    paste0(
      "Ensembl external xref homeobox protein Hox-A2-like; its physical ",
      "position inside the HoxD cluster fixes the assignment as HoxD2."
    ),
    "Ensembl external xref homeobox protein Hox-D8; collinear HoxD8 position.",
    "Ensembl external xref hoxd9a; collinear HoxD9 position."
  ),
  evidence_url = c(
    "https://pmc.ncbi.nlm.nih.gov/articles/PMC3897358/",
    rep("https://rest.ensembl.org/documentation/info/xref_id", 7L)
  ),
  stringsAsFactors = FALSE
)

# Two gar source genes have a coherent Hox3 isoform and a longer merged
# isoform containing Hox4-like, Hox3-like, and Hox2-like homeodomains. The merged
# isoforms are excluded before applying the longest-span rule. This exception
# is surfaced on the selected gene rows and on the affected Hox2 gap rows.
curated_transcript_exclusions <- data.frame(
  species = rep("gar", 3L),
  gene_id = c(
    "ENSLOCG00000011801",
    "ENSLOCG00000013436",
    "ENSLOCG00000006348"
  ),
  transcript_id = c(
    "ENSLOCT00000014539",
    "ENSLOCT00000016594",
    "ENSLOCT00000007673"
  ),
  protein_id = c(
    "ENSLOCP00000014510",
    "ENSLOCP00000016564",
    "ENSLOCP00000007664"
  ),
  source_gene_name = c("hoxa3a", "hoxb3a", "hoxc6a"),
  affected_cluster = c("A", "B", "C"),
  affected_hox_numbers = c("2,3,4", "2,3,4", "6,9"),
  exclusion_class = c(
    rep("unsafe_merged_three_homeodomain_transcript", 2L),
    "unsafe_merged_two_homeodomain_transcript"
  ),
  candidate_model_assessment = c(
    rep("unsafe_merged_three_homeodomain_transcript_not_assigned", 2L),
    "unsafe_merged_two_homeodomain_transcript_not_assigned"
  ),
  exclusion_reason = c(
    paste0(
      "856-aa, 11-CDS merged prediction with HoxA4-, HoxA3-, and HoxA2-like ",
      "homeodomains; exclude before selecting the coherent HoxA3 isoform."
    ),
    paste0(
      "862-aa, 11-CDS merged prediction with HoxB4-, HoxB3-, and HoxB2-like ",
      "homeodomains; exclude before selecting the coherent HoxB3 isoform."
    ),
    paste0(
      "469-aa, 4-CDS merged prediction with HoxC9-like and HoxC6-like ",
      "homeodomains; no coherent alternative transcript is available."
    )
  ),
  stringsAsFactors = FALSE
)

# The selected gar HoxA3 protein has one conflicting external label. Ensembl
# gene/transcript names, ZFIN, and RefSeq identify Hox-A3a and the homeodomain
# sequence is Hox3-like; UniProt/TrEMBL W5N1L5.67 instead says Hox-A4a. Keep
# the HoxA3 assignment but make the disagreement explicit in every audit row.
xref_conflicts <- data.frame(
  species = "gar",
  gene_id = "ENSLOCG00000011801",
  transcript_id = "ENSLOCT00000014553",
  protein_id = "ENSLOCP00000014524",
  xref_conflict_flag = TRUE,
  conflicting_xref_database = "UniProt/TrEMBL",
  conflicting_xref_id = "W5N1L5.67",
  conflicting_xref_label = "Homeobox A4a",
  retained_assignment = "HoxA3",
  retained_assignment_support =
    "Ensembl_gene_and_transcript;ZFIN;RefSeq;cluster_collinearity;Hox3_like_homeodomain",
  xref_conflict_note = paste0(
    "Ensembl gene/transcript, ZFIN, RefSeq, cluster position, and a Hox3-like ",
    "homeodomain support HoxA3a; UniProt/TrEMBL W5N1L5.67 conflicts by ",
    "labelling the same translation Homeobox A4a."
  ),
  stringsAsFactors = FALSE
)

# This unnamed gar locus prediction spans several expected HoxA positions and
# is not safely assignable to any one slot. It is parsed for audit only and is
# excluded from plotted-gene selection.
unsafe_gene_models <- data.frame(
  species = "gar",
  gene_id = "ENSLOCG00000011824",
  transcript_id = "ENSLOCT00000014567",
  protein_id = "ENSLOCP00000014538",
  source_gene_name = NA_character_,
  affected_cluster = "A",
  affected_hox_numbers = "6,7,9",
  candidate_model_assessment =
    "unsafe_long_multiexon_prediction_not_assignable_to_one_slot",
  exclusion_reason = paste0(
    "Unnamed 673-aa, 13-CDS prediction spanning several expected HoxA ",
    "positions; do not assign it to one Hox slot."
  ),
  stringsAsFactors = FALSE
)

# Literature/curation-defined functional complements used for QA. These are
# not inferred from which gene_name strings happen to occur in a GTF. That
# distinction prevents an annotation miss from being plotted as gene loss.
expected_inventories <- list(
  human = list(
    A = c(1:7, 9:11, 13),
    B = c(1:9, 13),
    C = c(4:6, 8:13),
    D = c(1, 3, 4, 8:13)
  ),
  mouse = list(
    A = c(1:7, 9:11, 13),
    B = c(1:9, 13),
    C = c(4:6, 8:13),
    D = c(1, 3, 4, 8:13)
  ),
  chicken = list(
    A = c(1:7, 9:11, 13),
    B = c(1:9, 13),
    C = c(4:6, 8:13),
    D = c(1, 3, 4, 8:13)
  ),
  gar = list(
    A = c(1:7, 9:11, 13),
    B = c(1:10, 13),
    C = c(1, 3:6, 8:13),
    D = c(1:4, 8:13)
  ),
  zebrafish = list(
    AA = c(1, 3:5, 9, 11, 13),
    AB = c(2, 9:11, 13),
    BA = c(1:10, 13),
    BB = c(1, 5, 6, 8),
    CA = c(1, 3:6, 8:13),
    CB = c(6, 11:13),
    DA = c(3, 4, 9:13)
  ),
  amphioxus = list(ancestral = 1:15)
)

inventory_reference <- c(
  human = "Ensembl_116_curated_39_gene_complement",
  mouse = "Ensembl_116_curated_39_gene_complement",
  chicken = "published_avian_39_gene_complement_and_Ensembl_116_locus_audit",
  gar = "Braasch_et_al_2016_Supplementary_Figure_12_43_protein_coding_genes",
  zebrafish = "Braasch_et_al_2016_Supplementary_Figure_12_49_protein_coding_genes",
  amphioxus = "Pascual-Anaya_et_al_2012_15_gene_complement"
)

expected_complement <- do.call(rbind, lapply(names(expected_inventories), function(sp) {
  do.call(rbind, lapply(names(expected_inventories[[sp]]), function(cluster_id) {
    numbers <- expected_inventories[[sp]][[cluster_id]]
    family <- if (identical(cluster_id, "ancestral")) {
      "ancestral"
    } else {
      substr(cluster_id, 1L, 1L)
    }
    suffix <- if (sp == "zebrafish") substr(cluster_id, 2L, 2L) else ""
    matrix_row <- if (sp == "zebrafish") {
      paste0("zebrafish_", tolower(suffix))
    } else {
      sp
    }
    data.frame(
      species = sp,
      matrix_row = matrix_row,
      matrix_column = if (identical(sp, "amphioxus")) "A" else family,
      cluster_family = family,
      cluster = cluster_id,
      hox_number = as.integer(numbers),
      slot = paste0("Hox", numbers),
      expected_status = "expected_functional_member",
      inventory_reference = unname(inventory_reference[[sp]]),
      stringsAsFactors = FALSE
    )
  }))
}))
row.names(expected_complement) <- NULL

download_file_once <- function(url, dest) {
  if (!file.exists(dest) || file.info(dest)$size == 0L) {
    message("Downloading ", url)
    utils::download.file(url, dest, mode = "wb", quiet = FALSE)
  }
  dest
}

sha256_file <- function(path) {
  if (requireNamespace("openssl", quietly = TRUE)) {
    con <- file(path, open = "rb")
    on.exit(close(con), add = TRUE)
    return(unclass(as.character(openssl::sha256(con))))
  }
  bin <- Sys.which("shasum")
  if (!nzchar(bin)) {
    stop("Need either the openssl R package or the shasum executable.", call. = FALSE)
  }
  out <- system2(bin, c("-a", "256", shQuote(path)), stdout = TRUE)
  sub("[[:space:]].*$", "", out[[1L]])
}

gtf_paths <- setNames(character(nrow(species)), species$species)
for (i in seq_len(nrow(species))) {
  dest <- file.path(download_dir, species$local_file[[i]])
  download_file_once(species$source_url[[i]], dest)
  observed_sha <- sha256_file(dest)
  if (!identical(observed_sha, species$sha256[[i]])) {
    stop(
      "SHA-256 mismatch for ", species$source_file[[i]],
      ": expected ", species$sha256[[i]], ", observed ", observed_sha,
      call. = FALSE
    )
  }
  gtf_paths[[species$species[[i]]]] <- dest
}

attribute_value <- function(x, key) {
  pattern <- paste0("(?:^|;[[:space:]]*)", key, " \\\"([^\\\"]*)\\\"")
  hit <- grepl(pattern, x, perl = TRUE)
  out <- rep(NA_character_, length(x))
  out[hit] <- sub(paste0(".*", pattern, ".*"), "\\1", x[hit], perl = TRUE)
  out
}

read_hox_gtf <- function(
  path,
  species_id,
  amphioxus_ids = character(),
  manual_ids = character()
) {
  con <- gzfile(path, open = "rt")
  on.exit(close(con), add = TRUE)
  kept <- character()
  feature_pattern <- "\\t(gene|transcript|CDS|start_codon|stop_codon)\\t"
  repeat {
    lines <- readLines(con, n = 100000L, warn = FALSE)
    if (length(lines) == 0L) break
    lines <- lines[grepl(feature_pattern, lines, perl = TRUE)]
    if (length(lines) == 0L) next
    if (identical(species_id, "amphioxus")) {
      id_pattern <- paste0(
        "gene_id \\\"(", paste(amphioxus_ids, collapse = "|"), ")\\\""
      )
      lines <- lines[grepl(id_pattern, lines, perl = TRUE)]
    } else {
      keep <- grepl(
        "gene_name \\\"hox[abcd][0-9]+[ab]?\\\"(?:;|$)",
        lines,
        ignore.case = TRUE,
        perl = TRUE
      )
      if (length(manual_ids) > 0L) {
        id_pattern <- paste0(
          "gene_id \\\"(", paste(manual_ids, collapse = "|"), ")\\\""
        )
        keep <- keep | grepl(id_pattern, lines, perl = TRUE)
      }
      lines <- lines[keep]
    }
    kept <- c(kept, lines)
  }

  if (length(kept) == 0L) {
    stop("No candidate Hox features found for ", species_id, call. = FALSE)
  }
  fields <- strsplit(kept, "\t", fixed = TRUE)
  bad <- lengths(fields) != 9L
  if (any(bad)) {
    stop("Malformed GTF lines for ", species_id, call. = FALSE)
  }
  mat <- do.call(rbind, fields)
  attrs <- mat[, 9L]
  data.frame(
    species = species_id,
    seqname = mat[, 1L],
    source = mat[, 2L],
    feature = mat[, 3L],
    start = as.integer(mat[, 4L]),
    end = as.integer(mat[, 5L]),
    score = mat[, 6L],
    strand = mat[, 7L],
    phase = mat[, 8L],
    gene_id = attribute_value(attrs, "gene_id"),
    gene_version = attribute_value(attrs, "gene_version"),
    gene_name = attribute_value(attrs, "gene_name"),
    gene_source = attribute_value(attrs, "gene_source"),
    gene_biotype = attribute_value(attrs, "gene_biotype"),
    transcript_id = attribute_value(attrs, "transcript_id"),
    transcript_version = attribute_value(attrs, "transcript_version"),
    transcript_name = attribute_value(attrs, "transcript_name"),
    transcript_source = attribute_value(attrs, "transcript_source"),
    transcript_biotype = attribute_value(attrs, "transcript_biotype"),
    protein_id = attribute_value(attrs, "protein_id"),
    protein_version = attribute_value(attrs, "protein_version"),
    exon_number = suppressWarnings(as.integer(attribute_value(attrs, "exon_number"))),
    is_ensembl_canonical = grepl('tag "Ensembl_canonical"', attrs, fixed = TRUE),
    stringsAsFactors = FALSE
  )
}

annotations <- lapply(species$species, function(species_id) {
  cache_path <- file.path(download_dir, paste0(species_id, "-hox-subset-v4.rds"))
  if (file.exists(cache_path)) {
    message("Reading cached Hox features for ", species_id)
    return(readRDS(cache_path))
  }
  message("Reading Hox features for ", species_id)
  result <- read_hox_gtf(
      gtf_paths[[species_id]],
      species_id,
      amphioxus_ids = amphioxus_mapping$gene_id,
      manual_ids = union(
        manual_gene_mapping$gene_id[
          manual_gene_mapping$species == species_id
        ],
        unsafe_gene_models$gene_id[
          unsafe_gene_models$species == species_id
        ]
      )
    )
  saveRDS(result, cache_path, version = 3)
  result
})
names(annotations) <- species$species

classify_gene <- function(gene_row, species_id) {
  if (identical(species_id, "amphioxus")) {
    mapped <- amphioxus_mapping[
      amphioxus_mapping$gene_id == gene_row$gene_id[[1L]],
      ,
      drop = FALSE
    ]
    if (nrow(mapped) != 1L) stop("Missing amphioxus mapping.", call. = FALSE)
    return(data.frame(
      gene_symbol = mapped$gene_symbol,
      hox_number = mapped$hox_number,
      cluster_family = "ancestral",
      cluster = "ancestral",
      matrix_row = "amphioxus",
      matrix_column = "A",
      mapping_method = mapped$mapping_method,
      mapping_note = mapped$mapping_note,
      stringsAsFactors = FALSE
    ))
  }

  mapped <- manual_gene_mapping[
    manual_gene_mapping$species == species_id &
      manual_gene_mapping$gene_id == gene_row$gene_id[[1L]],
    ,
    drop = FALSE
  ]
  if (nrow(mapped) > 0L) {
    if (nrow(mapped) != 1L) {
      stop("Duplicate manual mapping for ", species_id, ":", gene_row$gene_id[[1L]])
    }
    return(data.frame(
      gene_symbol = mapped$gene_symbol,
      hox_number = mapped$hox_number,
      cluster_family = mapped$cluster_family,
      cluster = mapped$cluster,
      matrix_row = mapped$matrix_row,
      matrix_column = mapped$matrix_column,
      mapping_method = mapped$mapping_method,
      mapping_note = mapped$mapping_note,
      stringsAsFactors = FALSE
    ))
  }

  symbol <- gene_row$gene_name[[1L]]
  if (identical(species_id, "zebrafish")) {
    match <- regexec("^hox([abcd])([0-9]+)([ab])$", symbol, ignore.case = TRUE)
    parts <- regmatches(symbol, match)[[1L]]
    if (length(parts) != 4L) {
      stop("Unexpected zebrafish Hox symbol: ", symbol, call. = FALSE)
    }
    family <- toupper(parts[[2L]])
    suffix <- tolower(parts[[4L]])
    cluster <- paste0(family, toupper(suffix))
    matrix_row <- paste0("zebrafish_", suffix)
  } else {
    match <- regexec("^hox([abcd])([0-9]+)[ab]?$", symbol, ignore.case = TRUE)
    parts <- regmatches(symbol, match)[[1L]]
    if (length(parts) != 3L) {
      stop("Unexpected Hox symbol: ", symbol, call. = FALSE)
    }
    family <- toupper(parts[[2L]])
    cluster <- family
    matrix_row <- species_id
  }
  data.frame(
    gene_symbol = symbol,
    hox_number = as.integer(parts[[3L]]),
    cluster_family = family,
    cluster = cluster,
    matrix_row = matrix_row,
    matrix_column = family,
    mapping_method = "source_gene_symbol",
    mapping_note = "Hox family and number parsed from the Ensembl GTF gene_name.",
    stringsAsFactors = FALSE
  )
}

feature_coordinates <- function(rows, strand) {
  if (nrow(rows) == 0L) return(integer())
  if (identical(strand, "+")) {
    rows <- rows[order(rows$start, rows$end), , drop = FALSE]
    unlist(Map(seq.int, rows$start, rows$end), use.names = FALSE)
  } else {
    rows <- rows[order(rows$start, rows$end, decreasing = TRUE), , drop = FALSE]
    unlist(Map(function(start, end) seq.int(end, start), rows$start, rows$end), use.names = FALSE)
  }
}

codon_coordinates <- function(rows, strand) {
  coords <- feature_coordinates(rows, strand)
  if (length(coords) == 3L) coords else integer()
}

combine_stable_version <- function(id, version) {
  ifelse(is.na(version) | !nzchar(version), id, paste0(id, ".", version))
}

select_gene_transcript <- function(annotation, gene_id, species_id) {
  rows <- annotation[annotation$gene_id == gene_id, , drop = FALSE]
  gene_row <- rows[rows$feature == "gene", , drop = FALSE]
  if (nrow(gene_row) != 1L) {
    stop("Expected one gene row for ", species_id, ":", gene_id, call. = FALSE)
  }
  if (!identical(gene_row$gene_biotype[[1L]], "protein_coding")) {
    stop("Non-protein-coding gene reached selection: ", gene_id, call. = FALSE)
  }
  classification <- classify_gene(gene_row, species_id)

  all_transcript_rows <- rows[rows$feature == "transcript", , drop = FALSE]
  all_transcript_rows <- all_transcript_rows[
    all_transcript_rows$transcript_biotype == "protein_coding" &
      !is.na(all_transcript_rows$transcript_id),
    ,
    drop = FALSE
  ]
  excluded_transcripts <- curated_transcript_exclusions[
    curated_transcript_exclusions$species == species_id &
      curated_transcript_exclusions$gene_id == gene_id,
    ,
    drop = FALSE
  ]
  transcript_rows <- all_transcript_rows[
    !all_transcript_rows$transcript_id %in% excluded_transcripts$transcript_id,
    ,
    drop = FALSE
  ]
  valid <- list()
  for (i in seq_len(nrow(transcript_rows))) {
    transcript <- transcript_rows[i, , drop = FALSE]
    tx_id <- transcript$transcript_id[[1L]]
    tx <- rows[rows$transcript_id == tx_id & !is.na(rows$transcript_id), , drop = FALSE]
    cds <- tx[tx$feature == "CDS", , drop = FALSE]
    init <- tx[tx$feature == "start_codon", , drop = FALSE]
    stop <- tx[tx$feature == "stop_codon", , drop = FALSE]
    init_coords <- codon_coordinates(init, transcript$strand[[1L]])
    stop_coords <- codon_coordinates(stop, transcript$strand[[1L]])
    cds_coords <- feature_coordinates(cds, transcript$strand[[1L]])
    if (length(cds_coords) < 3L) {
      next
    }
    if (
      any(cds$seqname != transcript$seqname[[1L]]) ||
      any(cds$strand != transcript$strand[[1L]]) ||
      (nrow(init) > 0L && any(init$seqname != transcript$seqname[[1L]])) ||
      (nrow(stop) > 0L && any(stop$seqname != transcript$seqname[[1L]]))
    ) {
      stop("Inconsistent seqname or strand for transcript ", tx_id, call. = FALSE)
    }
    has_explicit_init <- length(init_coords) == 3L
    has_explicit_stop <- length(stop_coords) == 3L
    valid[[length(valid) + 1L]] <- data.frame(
      transcript_index = i,
      transcript_id = tx_id,
      transcript_span_bp = transcript$end[[1L]] - transcript$start[[1L]] + 1L,
      is_ensembl_canonical = transcript$is_ensembl_canonical[[1L]],
      initiation_codon_center = if (has_explicit_init) {
        init_coords[[2L]]
      } else {
        cds_coords[[2L]]
      },
      stop_codon_center = if (has_explicit_stop) {
        stop_coords[[2L]]
      } else {
        cds_coords[[length(cds_coords) - 1L]]
      },
      initiation_anchor_source = if (has_explicit_init) {
        "explicit_start_codon"
      } else {
        "terminal_CDS_positional_proxy"
      },
      stop_anchor_source = if (has_explicit_stop) {
        "explicit_stop_codon"
      } else {
        "terminal_CDS_positional_proxy"
      },
      initiation_anchor_fallback = !has_explicit_init,
      stop_anchor_fallback = !has_explicit_stop,
      stringsAsFactors = FALSE
    )
  }
  if (length(valid) == 0L) {
    has_cds <- vapply(transcript_rows$transcript_id, function(tx_id) {
      any(rows$transcript_id == tx_id & rows$feature == "CDS", na.rm = TRUE)
    }, logical(1))
    has_complete_start <- vapply(transcript_rows$transcript_id, function(tx_id) {
      tx <- rows[rows$transcript_id == tx_id & rows$feature == "start_codon", , drop = FALSE]
      nrow(tx) > 0L && sum(tx$end - tx$start + 1L) == 3L
    }, logical(1))
    has_complete_stop <- vapply(transcript_rows$transcript_id, function(tx_id) {
      tx <- rows[rows$transcript_id == tx_id & rows$feature == "stop_codon", , drop = FALSE]
      nrow(tx) > 0L && sum(tx$end - tx$start + 1L) == 3L
    }, logical(1))
    metadata <- species[species$species == species_id, , drop = FALSE]
    gap <- data.frame(
      species = species_id,
      matrix_row = classification$matrix_row,
      matrix_column = classification$matrix_column,
      cluster_family = classification$cluster_family,
      cluster = classification$cluster,
      hox_number = classification$hox_number,
      slot = paste0("Hox", classification$hox_number),
      gene_symbol = classification$gene_symbol,
      gene_id = gene_row$gene_id,
      seqname = gene_row$seqname,
      gene_start = gene_row$start,
      gene_end = gene_row$end,
      strand = gene_row$strand,
      protein_coding_transcript_count = nrow(all_transcript_rows),
      eligible_safe_protein_coding_transcript_count = nrow(transcript_rows),
      excluded_transcript_count = nrow(excluded_transcripts),
      excluded_transcript_ids = paste(
        excluded_transcripts$transcript_id,
        collapse = ","
      ),
      transcript_with_cds_count = sum(has_cds),
      transcript_with_usable_cds_count = sum(has_cds),
      transcript_with_complete_start_codon_count = sum(has_complete_start),
      transcript_with_complete_stop_codon_count = sum(has_complete_stop),
      exclusion_reason = if (nrow(excluded_transcripts) > 0L) {
        "all_source_coding_transcripts_excluded_as_unsafe_merged_models"
      } else {
        "no_coding_transcript_with_usable_CDS_and_strand"
      },
      annotation_gap_class = "source_annotation_gap_no_safe_gene_model",
      candidate_gene_id = if (nrow(excluded_transcripts) > 0L) {
        gene_row$gene_id
      } else {
        NA_character_
      },
      candidate_transcript_id = paste(
        excluded_transcripts$transcript_id,
        collapse = ","
      ),
      candidate_protein_id = paste(
        excluded_transcripts$protein_id,
        collapse = ","
      ),
      candidate_model_assessment = paste(
        excluded_transcripts$candidate_model_assessment,
        collapse = ","
      ),
      mapping_method = classification$mapping_method,
      mapping_note = if (nrow(excluded_transcripts) > 0L) {
        paste0(
          classification$mapping_note,
          " No coherent alternative transcript remains after excluding: ",
          paste(excluded_transcripts$exclusion_reason, collapse = " | ")
        )
      } else {
        classification$mapping_note
      },
      source_database = metadata$source_database,
      release = metadata$release,
      assembly = metadata$assembly,
      source_url = metadata$source_url,
      source_file = metadata$source_file,
      stringsAsFactors = FALSE
    )
    return(list(gene = NULL, cds = NULL, transcript = NULL, features = NULL, gap = gap))
  }
  candidates <- do.call(rbind, valid)
  candidates <- candidates[order(
    -candidates$transcript_span_bp,
    -as.integer(candidates$is_ensembl_canonical),
    candidates$transcript_id
  ), , drop = FALSE]
  winner <- candidates[1L, , drop = FALSE]
  transcript <- transcript_rows[winner$transcript_index[[1L]], , drop = FALSE]
  tx <- rows[
    rows$transcript_id == winner$transcript_id[[1L]] & !is.na(rows$transcript_id),
    ,
    drop = FALSE
  ]
  cds <- tx[tx$feature == "CDS", , drop = FALSE]
  cds <- if (identical(transcript$strand[[1L]], "+")) {
    cds[order(cds$start, cds$end), , drop = FALSE]
  } else {
    cds[order(cds$start, cds$end, decreasing = TRUE), , drop = FALSE]
  }
  cds$cds_rank <- seq_len(nrow(cds))
  protein_ids <- unique(stats::na.omit(cds$protein_id))
  protein_versions <- unique(stats::na.omit(cds$protein_version))
  if (length(protein_ids) > 1L) {
    stop("Multiple protein IDs for ", winner$transcript_id[[1L]], call. = FALSE)
  }
  protein_id <- if (length(protein_ids) == 1L) protein_ids[[1L]] else NA_character_
  protein_version <- if (length(protein_versions) == 1L) {
    protein_versions[[1L]]
  } else {
    NA_character_
  }
  coding_midpoint <- (
    winner$initiation_codon_center[[1L]] + winner$stop_codon_center[[1L]]
  ) / 2
  metadata <- species[species$species == species_id, , drop = FALSE]
  conflict <- xref_conflicts[
    xref_conflicts$species == species_id &
      xref_conflicts$gene_id == gene_id &
      xref_conflicts$transcript_id == transcript$transcript_id[[1L]],
    ,
    drop = FALSE
  ]
  if (nrow(conflict) > 1L) {
    stop("Duplicate xref-conflict record for ", gene_id, call. = FALSE)
  }
  gene_out <- data.frame(
    species = species_id,
    species_row = classification$matrix_row,
    display_name = metadata$display_name,
    scientific_name = metadata$scientific_name,
    assembly = metadata$assembly,
    assembly_accession = metadata$assembly_accession,
    source_database = metadata$source_database,
    release = metadata$release,
    matrix_row = classification$matrix_row,
    matrix_column = classification$matrix_column,
    cluster_column = classification$matrix_column,
    cluster_family = classification$cluster_family,
    cluster = classification$cluster,
    cluster_name = classification$cluster,
    hox_number = classification$hox_number,
    hox_slot = 16L - classification$hox_number,
    hox_slot_label = paste0("Hox", classification$hox_number),
    slot = paste0("Hox", classification$hox_number),
    gene_symbol = classification$gene_symbol,
    gene_id = gene_row$gene_id,
    gene_version = gene_row$gene_version,
    gene_stable_id_version = combine_stable_version(
      gene_row$gene_id,
      gene_row$gene_version
    ),
    gene_source = gene_row$gene_source,
    gene_biotype = gene_row$gene_biotype,
    seqname = gene_row$seqname,
    gene_start = gene_row$start,
    gene_end = gene_row$end,
    strand = gene_row$strand,
    transcript_id = transcript$transcript_id,
    transcript_version = transcript$transcript_version,
    transcript_stable_id_version = combine_stable_version(
      transcript$transcript_id,
      transcript$transcript_version
    ),
    transcript_name = transcript$transcript_name,
    transcript_source = transcript$transcript_source,
    transcript_biotype = transcript$transcript_biotype,
    transcript_start = transcript$start,
    transcript_end = transcript$end,
    transcript_span_bp = winner$transcript_span_bp,
    protein_id = protein_id,
    protein_version = protein_version,
    protein_stable_id_version = combine_stable_version(protein_id, protein_version),
    cds_piece_count = nrow(cds),
    cds_length_bp = sum(cds$end - cds$start + 1L),
    initiation_codon_center = winner$initiation_codon_center,
    coding_midpoint = coding_midpoint,
    stop_codon_center = winner$stop_codon_center,
    genomic_x_start = winner$initiation_codon_center,
    genomic_x_middle = coding_midpoint,
    genomic_x_end = winner$stop_codon_center,
    x = coding_midpoint,
    initiation_anchor_source = winner$initiation_anchor_source,
    stop_anchor_source = winner$stop_anchor_source,
    initiation_anchor_fallback = winner$initiation_anchor_fallback,
    stop_anchor_fallback = winner$stop_anchor_fallback,
    any_anchor_fallback = winner$initiation_anchor_fallback |
      winner$stop_anchor_fallback,
    coding_transcript_candidate_count = nrow(candidates),
    protein_coding_transcript_count = nrow(all_transcript_rows),
    eligible_safe_protein_coding_transcript_count = nrow(transcript_rows),
    curated_transcript_exclusion_applied = nrow(excluded_transcripts) > 0L,
    unsafe_merged_transcript_exclusion_applied = any(
      startsWith(excluded_transcripts$exclusion_class, "unsafe_merged")
    ),
    excluded_transcript_count = nrow(excluded_transcripts),
    excluded_transcript_ids = paste(
      excluded_transcripts$transcript_id,
      collapse = ","
    ),
    excluded_protein_ids = paste(
      excluded_transcripts$protein_id,
      collapse = ","
    ),
    excluded_transcript_assessments = paste(
      excluded_transcripts$candidate_model_assessment,
      collapse = " | "
    ),
    excluded_transcript_reason = paste(
      excluded_transcripts$exclusion_reason,
      collapse = " | "
    ),
    selected_is_ensembl_canonical = winner$is_ensembl_canonical,
    transcript_selection_rule = if (nrow(excluded_transcripts) > 0L) {
      paste0(
        "exclude_curated_unsafe_merged_transcripts_then_greatest_genomic_",
        "span_with_usable_CDS_and_strand;ties_Ensembl_canonical_then_",
        "transcript_id"
      )
    } else {
      paste0(
        "greatest_genomic_span_with_usable_CDS_and_strand;",
        "ties_Ensembl_canonical_then_transcript_id"
      )
    },
    codon_anchor_rule = paste0(
      "explicit_codon_middle_when_complete_else_terminal_CDS_positional_",
      "proxy_in_transcription_order"
    ),
    midpoint_rule = "arithmetic_genomic_midpoint_between_codon_centers",
    mapping_method = classification$mapping_method,
    mapping_note = classification$mapping_note,
    xref_conflict_flag = nrow(conflict) == 1L,
    xref_conflict_note = if (nrow(conflict) == 1L) {
      conflict$xref_conflict_note
    } else {
      NA_character_
    },
    source_url = metadata$source_url,
    source_file = metadata$source_file,
    source_sha256 = metadata$sha256,
    stringsAsFactors = FALSE
  )
  cds_out <- data.frame(
    species = species_id,
    matrix_row = classification$matrix_row,
    matrix_column = classification$matrix_column,
    cluster_family = classification$cluster_family,
    cluster = classification$cluster,
    hox_number = classification$hox_number,
    gene_symbol = classification$gene_symbol,
    gene_id = gene_row$gene_id,
    transcript_id = transcript$transcript_id,
    protein_id = protein_id,
    cds_rank = cds$cds_rank,
    seqname = cds$seqname,
    start = cds$start,
    end = cds$end,
    strand = cds$strand,
    phase = cds$phase,
    source = cds$source,
    stringsAsFactors = FALSE
  )
  feature_out <- tx[tx$feature %in% c("CDS", "start_codon", "stop_codon"), , drop = FALSE]
  list(
    gene = gene_out,
    cds = cds_out,
    transcript = transcript,
    features = feature_out,
    gap = NULL
  )
}

selected <- list()
for (species_id in names(annotations)) {
  annotation <- annotations[[species_id]]
  gene_rows <- annotation[
    annotation$feature == "gene" &
      annotation$gene_biotype == "protein_coding" &
      !annotation$gene_id %in% unsafe_gene_models$gene_id,
    ,
    drop = FALSE
  ]
  for (gene_id in unique(gene_rows$gene_id)) {
    selected[[paste(species_id, gene_id, sep = ":")]] <- select_gene_transcript(
      annotation,
      gene_id,
      species_id
    )
  }
}

genes <- do.call(rbind, lapply(selected, `[[`, "gene"))
row.names(genes) <- NULL
cds <- do.call(rbind, lapply(selected, `[[`, "cds"))
row.names(cds) <- NULL
annotation_gaps <- do.call(rbind, lapply(selected, `[[`, "gap"))
if (is.null(annotation_gaps)) annotation_gaps <- data.frame()
row.names(annotation_gaps) <- NULL

# Expected members without a safe source model are annotation gaps, not
# biological losses and not fabricated boxes. Chicken's four gaps follow the
# published 39-gene avian complement. Gar's 12 gaps reconcile the 43-member
# protein-coding complement in Braasch et al. (2016) with the release-116
# LepOcu1 models. The nearby 13-CDS gar prediction ENSLOCG00000011824 is
# recorded as a candidate affecting HoxA6/A7/A9, but is not assigned to one
# slot because its chimeric structure is unsafe. BraLan2 Hox13 remains the
# explicitly documented lancelet annotation gap.
manual_gap_specs <- data.frame(
  species = c(
    rep("chicken", 4L),
    rep("gar", 11L),
    "amphioxus"
  ),
  cluster = c(
    rep("C", 3L), "D",
    rep("A", 5L), rep("B", 2L), rep("C", 4L),
    "ancestral"
  ),
  hox_number = c(
    4:6, 1L,
    2L, 4L, 6L, 7L, 9L, 2L, 4L, 1L, 8:10,
    13L
  ),
  candidate_gene_id = c(
    rep(NA_character_, 4L),
    NA_character_, NA_character_,
    rep("ENSLOCG00000011824", 3L),
    rep(NA_character_, 6L),
    NA_character_
  ),
  stringsAsFactors = FALSE
)
manual_gap_specs$candidate_transcript_id <- NA_character_
manual_gap_specs$candidate_protein_id <- NA_character_
manual_gap_specs$candidate_model_assessment <- NA_character_

gar_a24 <- manual_gap_specs$species == "gar" &
  manual_gap_specs$cluster == "A" &
  manual_gap_specs$hox_number %in% c(2L, 4L)
manual_gap_specs$candidate_gene_id[gar_a24] <- "ENSLOCG00000011801"
manual_gap_specs$candidate_transcript_id[gar_a24] <- "ENSLOCT00000014539"
manual_gap_specs$candidate_protein_id[gar_a24] <- "ENSLOCP00000014510"
manual_gap_specs$candidate_model_assessment[gar_a24] <-
  "unsafe_merged_three_homeodomain_transcript_not_assigned"

gar_b24 <- manual_gap_specs$species == "gar" &
  manual_gap_specs$cluster == "B" &
  manual_gap_specs$hox_number %in% c(2L, 4L)
manual_gap_specs$candidate_gene_id[gar_b24] <- "ENSLOCG00000013436"
manual_gap_specs$candidate_transcript_id[gar_b24] <- "ENSLOCT00000016594"
manual_gap_specs$candidate_protein_id[gar_b24] <- "ENSLOCP00000016564"
manual_gap_specs$candidate_model_assessment[gar_b24] <-
  "unsafe_merged_three_homeodomain_transcript_not_assigned"

gar_multislot <- !is.na(manual_gap_specs$candidate_gene_id) &
  manual_gap_specs$candidate_gene_id == "ENSLOCG00000011824"
manual_gap_specs$candidate_transcript_id[gar_multislot] <- "ENSLOCT00000014567"
manual_gap_specs$candidate_protein_id[gar_multislot] <- "ENSLOCP00000014538"
manual_gap_specs$candidate_model_assessment[gar_multislot] <-
  "unsafe_long_multiexon_prediction_not_assignable_to_one_slot"

gar_c9 <- manual_gap_specs$species == "gar" &
  manual_gap_specs$cluster == "C" &
  manual_gap_specs$hox_number == 9L
manual_gap_specs$candidate_gene_id[gar_c9] <- "ENSLOCG00000006348"
manual_gap_specs$candidate_transcript_id[gar_c9] <- "ENSLOCT00000007673"
manual_gap_specs$candidate_protein_id[gar_c9] <- "ENSLOCP00000007664"
manual_gap_specs$candidate_model_assessment[gar_c9] <-
  "unsafe_merged_two_homeodomain_transcript_not_assigned"
manual_gap_specs$cluster_family <- ifelse(
  manual_gap_specs$cluster == "ancestral",
  "ancestral",
  substr(manual_gap_specs$cluster, 1L, 1L)
)
manual_gap_specs$matrix_column <- ifelse(
  manual_gap_specs$species == "amphioxus",
  "A",
  manual_gap_specs$cluster_family
)
manual_gap_specs$matrix_row <- manual_gap_specs$species
manual_gap_specs$slot <- paste0("Hox", manual_gap_specs$hox_number)
manual_gap_specs$gene_symbol <- ifelse(
  manual_gap_specs$species == "chicken",
  paste0("HOX", manual_gap_specs$cluster, manual_gap_specs$hox_number),
  ifelse(
    manual_gap_specs$species == "amphioxus",
    paste0("Hox", manual_gap_specs$hox_number),
    paste0("hox", tolower(manual_gap_specs$cluster), manual_gap_specs$hox_number)
  )
)

manual_annotation_gaps <- do.call(rbind, lapply(seq_len(nrow(manual_gap_specs)), function(i) {
  spec <- manual_gap_specs[i, , drop = FALSE]
  metadata <- species[species$species == spec$species, , drop = FALSE]
  seqname <- if (spec$species == "chicken" && spec$cluster == "C") {
    "34"
  } else if (spec$species == "chicken" && spec$cluster == "D") {
    "7"
  } else if (spec$species == "gar" && spec$cluster == "A") {
    "LG11"
  } else if (spec$species == "gar" && spec$cluster == "B") {
    "LG15"
  } else if (spec$species == "gar" && spec$cluster == "C") {
    "LG4"
  } else {
    "Sc0000000"
  }
  strand <- if (spec$species == "gar" && spec$cluster == "C") "+" else "-"
  has_candidate <- !is.na(spec$candidate_gene_id)
  note <- if (spec$species == "amphioxus") {
    paste0(
      "No release-63 BraLan2 gene model lies between the collinearly mapped ",
      "Hox12 and Hox14 models; this is an annotation gap, not inferred loss."
    )
  } else if (spec$species == "chicken") {
    paste0(
      "The published 39-gene avian complement expects this slot, but the ",
      "Ensembl-116 GRCg7b locus has no protein-coding gene model for it."
    )
  } else if (
    has_candidate &&
      spec$candidate_model_assessment ==
        "unsafe_merged_three_homeodomain_transcript_not_assigned"
  ) {
    paste0(
      "Braasch et al. (2016) expect this Hox", spec$hox_number,
      " member. The longest source ",
      "transcript (", spec$candidate_transcript_id, "; ",
      spec$candidate_protein_id, ") merges Hox4-like, Hox3-like, and ",
      "Hox2-like homeodomains and cannot supply separate Hox2/Hox4 boxes. ",
      "A shorter ",
      "coherent isoform from the same source gene is used only for Hox3."
    )
  } else if (
    has_candidate &&
      spec$candidate_model_assessment ==
        "unsafe_merged_two_homeodomain_transcript_not_assigned"
  ) {
    paste0(
      "Braasch et al. (2016) expect this HoxC9 member. The only source ",
      "transcript (", spec$candidate_transcript_id, "; ",
      spec$candidate_protein_id, ") merges HoxC9-like and HoxC6-like ",
      "homeodomains. It is excluded, leaving both slots as annotation gaps."
    )
  } else if (has_candidate) {
    paste0(
      "Braasch et al. (2016) expect this functional HoxA member. Nearby ",
      "ENSLOCG00000011824 is a 13-CDS, 673-aa chimeric prediction spanning ",
      "several expected positions and is not safely assignable to one slot."
    )
  } else {
    paste0(
      "Braasch et al. (2016) expect this member in the 43-gene gar ",
      "complement, but Ensembl-116 LepOcu1 has no safe protein-coding Hox ",
      "model assignable to the slot."
    )
  }
  data.frame(
    species = spec$species,
    matrix_row = spec$matrix_row,
    matrix_column = spec$matrix_column,
    cluster_family = spec$cluster_family,
    cluster = spec$cluster,
    hox_number = spec$hox_number,
    slot = spec$slot,
    gene_symbol = spec$gene_symbol,
    gene_id = NA_character_,
    candidate_gene_id = spec$candidate_gene_id,
    candidate_transcript_id = spec$candidate_transcript_id,
    candidate_protein_id = spec$candidate_protein_id,
    candidate_model_assessment = spec$candidate_model_assessment,
    seqname = seqname,
    gene_start = NA_integer_,
    gene_end = NA_integer_,
    strand = strand,
    protein_coding_transcript_count = 0L,
    transcript_with_cds_count = 0L,
    transcript_with_usable_cds_count = 0L,
    transcript_with_complete_start_codon_count = 0L,
    transcript_with_complete_stop_codon_count = 0L,
    exclusion_reason = if (spec$species == "amphioxus") {
      "no_BraLan2_protein_coding_gene_model_at_Hox13_position"
    } else {
      "expected_functional_member_without_safe_source_gene_model"
    },
    annotation_gap_class = "source_annotation_gap_no_safe_gene_model",
    mapping_method = "literature_inventory_plus_release_locus_audit",
    mapping_note = note,
    source_database = metadata$source_database,
    release = metadata$release,
    assembly = metadata$assembly,
    source_url = metadata$source_url,
    source_file = metadata$source_file,
    stringsAsFactors = FALSE
  )
}))

bind_rows_fill <- function(x, y) {
  if (ncol(x) == 0L) return(y)
  columns <- union(names(x), names(y))
  for (column in setdiff(columns, names(x))) x[[column]] <- NA
  for (column in setdiff(columns, names(y))) y[[column]] <- NA
  rbind(x[, columns, drop = FALSE], y[, columns, drop = FALSE])
}
if (ncol(annotation_gaps) > 0L) {
  if (!"annotation_gap_class" %in% names(annotation_gaps)) {
    annotation_gaps$annotation_gap_class <-
      "source_annotation_gap_no_safe_gene_model"
  }
  if (!"candidate_gene_id" %in% names(annotation_gaps)) {
    annotation_gaps$candidate_gene_id <- NA_character_
  }
  if (!"candidate_transcript_id" %in% names(annotation_gaps)) {
    annotation_gaps$candidate_transcript_id <- NA_character_
  }
  if (!"candidate_protein_id" %in% names(annotation_gaps)) {
    annotation_gaps$candidate_protein_id <- NA_character_
  }
  if (!"candidate_model_assessment" %in% names(annotation_gaps)) {
    annotation_gaps$candidate_model_assessment <- NA_character_
  }
}
annotation_gaps <- bind_rows_fill(annotation_gaps, manual_annotation_gaps)

genes <- genes[order(
  species$species_order[match(genes$species, species$species)],
  match(genes$matrix_row, c(
    "human", "mouse", "chicken", "gar", "zebrafish_a", "zebrafish_b", "amphioxus"
  )),
  match(genes$matrix_column, c("A", "B", "C", "D")),
  -genes$hox_number
), , drop = FALSE]
cds <- cds[order(
  match(cds$gene_id, genes$gene_id),
  cds$cds_rank
), , drop = FALSE]

# Build every matrix cell so structural blanks, a non-retained cluster, and
# missing genes inside a retained cluster remain distinct states.
matrix_rows <- c(
  "human", "mouse", "chicken", "gar", "zebrafish_a", "zebrafish_b", "amphioxus"
)
matrix_columns <- c("A", "B", "C", "D")
clusters <- expand.grid(
  matrix_row = matrix_rows,
  matrix_column = matrix_columns,
  stringsAsFactors = FALSE
)
clusters <- clusters[order(
  match(clusters$matrix_row, matrix_rows),
  match(clusters$matrix_column, matrix_columns)
), , drop = FALSE]
clusters$species <- sub("_.*$", "", clusters$matrix_row)
clusters$species[clusters$matrix_row == "amphioxus"] <- "amphioxus"
clusters$cluster <- NA_character_
clusters$cell_status <- "structural_blank"

for (i in seq_len(nrow(clusters))) {
  row <- clusters$matrix_row[[i]]
  column <- clusters$matrix_column[[i]]
  if (row %in% c("human", "mouse", "chicken", "gar") && column %in% LETTERS[1:4]) {
    clusters$cluster[[i]] <- column
    clusters$cell_status[[i]] <- "retained"
  } else if (row == "zebrafish_a" && column %in% LETTERS[1:4]) {
    clusters$cluster[[i]] <- paste0(column, "A")
    clusters$cell_status[[i]] <- "retained"
  } else if (row == "zebrafish_b" && column %in% LETTERS[1:4]) {
    clusters$cluster[[i]] <- paste0(column, "B")
    clusters$cell_status[[i]] <- if (column == "D") {
      "cluster_not_retained"
    } else {
      "retained"
    }
  } else if (row == "amphioxus" && column == "A") {
    clusters$cluster[[i]] <- "ancestral"
    clusters$cell_status[[i]] <- "retained"
  }
}
clusters$annotated_gene_count <- 0L
clusters$expected_functional_member_count <- 0L
clusters$annotation_gap_count <- 0L
for (i in which(clusters$cell_status == "retained")) {
  clusters$annotated_gene_count[[i]] <- sum(
    genes$matrix_row == clusters$matrix_row[[i]] &
      genes$matrix_column == clusters$matrix_column[[i]]
  )
  clusters$expected_functional_member_count[[i]] <- sum(
    expected_complement$matrix_row == clusters$matrix_row[[i]] &
      expected_complement$matrix_column == clusters$matrix_column[[i]]
  )
  clusters$annotation_gap_count[[i]] <- sum(
    annotation_gaps$matrix_row == clusters$matrix_row[[i]] &
      annotation_gaps$matrix_column == clusters$matrix_column[[i]]
  )
}
clusters$missing_hox_slots <- vapply(seq_len(nrow(clusters)), function(i) {
  if (clusters$cell_status[[i]] != "retained") return(NA_character_)
  present <- genes$hox_number[
    genes$matrix_row == clusters$matrix_row[[i]] &
      genes$matrix_column == clusters$matrix_column[[i]]
  ]
  paste(setdiff(15:1, present), collapse = ",")
}, character(1))
clusters$annotation_gap_hox_slots <- vapply(seq_len(nrow(clusters)), function(i) {
  if (clusters$cell_status[[i]] != "retained") return(NA_character_)
  gap_numbers <- annotation_gaps$hox_number[
    annotation_gaps$matrix_row == clusters$matrix_row[[i]] &
      annotation_gaps$matrix_column == clusters$matrix_column[[i]]
  ]
  paste(sort(unique(gap_numbers), decreasing = TRUE), collapse = ",")
}, character(1))
clusters$empty_hox_slots_excluding_annotation_gaps <- vapply(
  seq_len(nrow(clusters)),
  function(i) {
    if (clusters$cell_status[[i]] != "retained") return(NA_character_)
    present <- genes$hox_number[
      genes$matrix_row == clusters$matrix_row[[i]] &
        genes$matrix_column == clusters$matrix_column[[i]]
    ]
    gap_numbers <- annotation_gaps$hox_number[
      annotation_gaps$matrix_row == clusters$matrix_row[[i]] &
        annotation_gaps$matrix_column == clusters$matrix_column[[i]]
    ]
    paste(setdiff(setdiff(15:1, present), gap_numbers), collapse = ",")
  },
  character(1)
)
clusters$biologically_not_retained_hox_slots <- vapply(
  seq_len(nrow(clusters)),
  function(i) {
    if (clusters$cell_status[[i]] != "retained") return(NA_character_)
    expected <- expected_complement$hox_number[
      expected_complement$matrix_row == clusters$matrix_row[[i]] &
        expected_complement$matrix_column == clusters$matrix_column[[i]]
    ]
    paste(setdiff(15:1, expected), collapse = ",")
  },
  character(1)
)
clusters$unresolved_expected_hox_slots <- vapply(
  seq_len(nrow(clusters)),
  function(i) {
    if (clusters$cell_status[[i]] != "retained") return(NA_character_)
    expected <- expected_complement$hox_number[
      expected_complement$matrix_row == clusters$matrix_row[[i]] &
        expected_complement$matrix_column == clusters$matrix_column[[i]]
    ]
    plotted <- genes$hox_number[
      genes$matrix_row == clusters$matrix_row[[i]] &
        genes$matrix_column == clusters$matrix_column[[i]]
    ]
    gaps <- annotation_gaps$hox_number[
      annotation_gaps$matrix_row == clusters$matrix_row[[i]] &
        annotation_gaps$matrix_column == clusters$matrix_column[[i]]
    ]
    paste(setdiff(expected, union(plotted, gaps)), collapse = ",")
  },
  character(1)
)
clusters$panel_note <- NA_character_
clusters$panel_note[
  clusters$matrix_row == "amphioxus" & clusters$matrix_column == "A"
] <- paste0(
  "The ancestral amphioxus cluster is displayed under HOXA for layout only; ",
  "Hox13 is a documented BraLan2 annotation gap and is not interpreted as ",
  "biological absence."
)
clusters$panel_note[
  clusters$matrix_row == "zebrafish_b" & clusters$matrix_column == "D"
] <- "HoxDb is not retained in the Ensembl 116 zebrafish complement."
clusters$panel_note[
  clusters$matrix_row == "chicken" & clusters$matrix_column %in% c("C", "D")
] <- paste0(
  "The published 39-gene complement expects the listed annotation-gap slots; ",
  "Ensembl 116 GRCg7b supplies no safe protein-coding model for them."
)
clusters$panel_note[
  clusters$matrix_row == "gar" & clusters$matrix_column == "A"
] <- paste0(
  "HoxA14 is the ancestral bony-vertebrate member absent from gar. Expected ",
  "functional members without a safe Ensembl model are annotation gaps."
)
clusters$panel_note[
  clusters$matrix_row == "gar" & clusters$matrix_column == "D"
] <- paste0(
  "Gar HoxD14 is a recognizable pseudogene and is excluded from the ",
  "43-protein-coding-gene complement and from gene boxes."
)
clusters$display_reverse <- NA
for (i in which(clusters$cell_status == "retained")) {
  rows <- genes[
    genes$matrix_row == clusters$matrix_row[[i]] &
      genes$matrix_column == clusters$matrix_column[[i]],
    ,
    drop = FALSE
  ]
  if (nrow(rows) >= 2L) {
    slope <- stats::coef(stats::lm(coding_midpoint ~ hox_number, data = rows))[[2L]]
    clusters$display_reverse[[i]] <- slope > 0
  }
}

# A complete per-slot state table makes the distinction between a plotted
# model, an annotation gap, and biological non-retention machine-readable.
# Gar A14 and D14 receive the more specific states supported by Braasch et al.
# (2016): lineage absence and recognizable pseudogene, respectively.
retained_clusters <- clusters[clusters$cell_status == "retained", , drop = FALSE]
slot_states <- do.call(rbind, lapply(seq_len(nrow(retained_clusters)), function(i) {
  panel <- retained_clusters[i, , drop = FALSE]
  do.call(rbind, lapply(15:1, function(number) {
    plotted <- genes[
      genes$matrix_row == panel$matrix_row &
        genes$matrix_column == panel$matrix_column &
        genes$hox_number == number,
      ,
      drop = FALSE
    ]
    gap <- annotation_gaps[
      annotation_gaps$matrix_row == panel$matrix_row &
        annotation_gaps$matrix_column == panel$matrix_column &
        annotation_gaps$hox_number == number,
      ,
      drop = FALSE
    ]
    expected <- any(
      expected_complement$matrix_row == panel$matrix_row &
        expected_complement$matrix_column == panel$matrix_column &
        expected_complement$hox_number == number
    )
    state <- if (nrow(plotted) == 1L) {
      "plotted_gene_model"
    } else if (nrow(gap) == 1L) {
      "annotation_gap_expected_functional_member"
    } else {
      "biologically_not_retained"
    }
    note <- if (nrow(gap) == 1L) {
      gap$mapping_note[[1L]]
    } else if (!expected) {
      "Not part of the curated functional complement for this cluster."
    } else {
      NA_character_
    }
    if (
      panel$species == "gar" &&
        panel$matrix_column == "A" &&
        number == 14L
    ) {
      state <- "lineage_absence"
      note <- paste0(
        "HoxA14 is the only ancestral bony-vertebrate Hox member absent ",
        "from the gar lineage in Braasch et al. (2016)."
      )
    }
    if (
      panel$species == "gar" &&
        panel$matrix_column == "D" &&
        number == 14L
    ) {
      state <- "recognizable_pseudogene_not_plotted"
      note <- paste0(
        "A recognizable gar HoxD14 pseudogene is present but is excluded ",
        "from the 43 protein-coding members and from gene boxes."
      )
    }
    data.frame(
      species = panel$species,
      matrix_row = panel$matrix_row,
      matrix_column = panel$matrix_column,
      cluster = panel$cluster,
      hox_number = number,
      slot = paste0("Hox", number),
      slot_state = state,
      expected_functional_member = expected,
      gene_symbol = if (nrow(plotted) == 1L) plotted$gene_symbol else {
        if (nrow(gap) == 1L) gap$gene_symbol else NA_character_
      },
      gene_id = if (nrow(plotted) == 1L) plotted$gene_id else NA_character_,
      transcript_id = if (nrow(plotted) == 1L) plotted$transcript_id else NA_character_,
      state_note = note,
      stringsAsFactors = FALSE
    )
  }))
}))

# The curated source inventory is the authority for the seven zebrafish
# clusters. It contains HoxCb and no HoxDb; fail if the plotted complement
# diverges from that inventory.
expected_zebrafish_clusters <- c("AA", "AB", "BA", "BB", "CA", "CB", "DA")
observed_zebrafish_clusters <- sort(unique(genes$cluster[genes$species == "zebrafish"]))
if (!setequal(observed_zebrafish_clusters, expected_zebrafish_clusters)) {
  stop(
    "Unexpected zebrafish cluster complement: ",
    paste(observed_zebrafish_clusters, collapse = ", "),
    call. = FALSE
  )
}
if (any(genes$cluster == "DB")) {
  stop("Ensembl 116 unexpectedly contains a zebrafish HoxDb gene.", call. = FALSE)
}
amphioxus_annotated_numbers <- c(
  genes$hox_number[genes$species == "amphioxus"],
  annotation_gaps$hox_number[annotation_gaps$species == "amphioxus"]
)
if (!setequal(unique(amphioxus_annotated_numbers), 1:15)) {
  stop("Unexpected BraLan2 annotated Hox complement.", call. = FALSE)
}
key_for <- function(x) {
  paste(x$species, x$cluster, x$hox_number, sep = ":")
}
expected_keys <- key_for(expected_complement)
plotted_keys <- key_for(genes)
gap_keys <- key_for(annotation_gaps)
if (anyDuplicated(expected_keys)) {
  stop("Duplicate member in expected Hox complement.", call. = FALSE)
}
if (length(intersect(plotted_keys, gap_keys)) > 0L) {
  stop("A Hox slot is both plotted and classified as an annotation gap.", call. = FALSE)
}
if (!setequal(union(plotted_keys, gap_keys), expected_keys)) {
  missing_keys <- setdiff(expected_keys, union(plotted_keys, gap_keys))
  extra_keys <- setdiff(union(plotted_keys, gap_keys), expected_keys)
  stop(
    "Plotted models plus annotation gaps do not equal expected complement. ",
    "Missing: ", paste(missing_keys, collapse = ", "),
    "; extra: ", paste(extra_keys, collapse = ", "),
    call. = FALSE
  )
}
expected_species_counts <- c(
  human = 39L,
  mouse = 39L,
  chicken = 39L,
  gar = 43L,
  zebrafish = 49L,
  amphioxus = 15L
)
observed_species_counts <- table(c(genes$species, annotation_gaps$species))
if (!identical(
  as.integer(observed_species_counts[names(expected_species_counts)]),
  unname(expected_species_counts)
)) {
  stop("Unexpected literature-complement totals by species.", call. = FALSE)
}
if (sum(genes$species == "zebrafish") != 49L) {
  stop("The zebrafish plotted complement must contain 49 gene models.", call. = FALSE)
}
if (sum(annotation_gaps$species == "chicken") != 4L) {
  stop("The chicken release audit must retain four explicit annotation gaps.", call. = FALSE)
}
if (sum(annotation_gaps$species == "gar") != 12L) {
  stop("The gar release audit must retain 12 explicit annotation gaps.", call. = FALSE)
}
if (any(genes$gene_id == "ENSLOCG00000011824")) {
  stop("Unsafe chimeric gar prediction was assigned to a Hox slot.", call. = FALSE)
}
if (nrow(genes) != 207L || nrow(cds) != 424L || nrow(annotation_gaps) != 17L) {
  stop("Unexpected final plotted-gene, CDS, or annotation-gap count.", call. = FALSE)
}
safe_override_expectation <- data.frame(
  gene_id = c("ENSLOCG00000011801", "ENSLOCG00000013436"),
  transcript_id = c("ENSLOCT00000014553", "ENSLOCT00000016597"),
  excluded_transcript_id = c(
    "ENSLOCT00000014539",
    "ENSLOCT00000016594"
  ),
  stringsAsFactors = FALSE
)
for (i in seq_len(nrow(safe_override_expectation))) {
  observed <- genes[
    genes$species == "gar" &
      genes$gene_id == safe_override_expectation$gene_id[[i]],
    ,
    drop = FALSE
  ]
  if (
    nrow(observed) != 1L ||
      observed$transcript_id != safe_override_expectation$transcript_id[[i]] ||
      !observed$curated_transcript_exclusion_applied ||
      observed$excluded_transcript_ids !=
        safe_override_expectation$excluded_transcript_id[[i]] ||
      !grepl("^exclude_curated_unsafe_merged_transcripts", observed$transcript_selection_rule)
  ) {
    stop("Unsafe gar merged-transcript override did not round-trip.", call. = FALSE)
  }
}
if (!identical(
  genes$xref_conflict_flag[
    genes$gene_id == "ENSLOCG00000011801"
  ],
  TRUE
)) {
  stop("Gar HoxA3 UniProt xref conflict is not recorded.", call. = FALSE)
}
if (any(genes$gene_id == "ENSLOCG00000006348")) {
  stop("Merged gar HoxC9/HoxC6 prediction was plotted.", call. = FALSE)
}
required_merged_gap_candidates <- data.frame(
  cluster = c("A", "A", "B", "B", "C", "C"),
  hox_number = c(2L, 4L, 2L, 4L, 6L, 9L),
  candidate_transcript_id = c(
    rep("ENSLOCT00000014539", 2L),
    rep("ENSLOCT00000016594", 2L),
    rep("ENSLOCT00000007673", 2L)
  ),
  stringsAsFactors = FALSE
)
for (i in seq_len(nrow(required_merged_gap_candidates))) {
  observed <- annotation_gaps[
    annotation_gaps$species == "gar" &
      annotation_gaps$cluster == required_merged_gap_candidates$cluster[[i]] &
      annotation_gaps$hox_number == required_merged_gap_candidates$hox_number[[i]],
    ,
    drop = FALSE
  ]
  if (
    nrow(observed) != 1L ||
      observed$candidate_transcript_id !=
        required_merged_gap_candidates$candidate_transcript_id[[i]] ||
      !grepl("^unsafe_merged_", observed$candidate_model_assessment)
  ) {
    stop("Merged gar candidate is missing from an affected gap slot.", call. = FALSE)
  }
}
if (any(
  clusters$cell_status == "retained" &
    !is.na(clusters$unresolved_expected_hox_slots) &
    nzchar(clusters$unresolved_expected_hox_slots)
)) {
  stop("A retained panel has unresolved expected Hox slots.", call. = FALSE)
}
if (any(
  clusters$cell_status == "retained" &
    clusters$empty_hox_slots_excluding_annotation_gaps !=
      clusters$biologically_not_retained_hox_slots
)) {
  stop("Annotation gaps were conflated with biological non-retention.", call. = FALSE)
}
if (!identical(
  slot_states$slot_state[
    slot_states$species == "gar" &
      slot_states$cluster == "A" &
      slot_states$hox_number == 14L
  ],
  "lineage_absence"
)) {
  stop("Gar HoxA14 must be represented as lineage absence.", call. = FALSE)
}
if (!identical(
  slot_states$slot_state[
    slot_states$species == "gar" &
      slot_states$cluster == "D" &
      slot_states$hox_number == 14L
  ],
  "recognizable_pseudogene_not_plotted"
)) {
  stop("Gar HoxD14 must be represented as an unplotted pseudogene.", call. = FALSE)
}
if (anyDuplicated(paste(genes$species, genes$cluster, genes$hox_number, sep = ":"))) {
  stop("Duplicate Hox-number assignment within a cluster.", call. = FALSE)
}
if (any(genes$initiation_codon_center == genes$stop_codon_center)) {
  stop("Coincident initiation and stop anchors detected.", call. = FALSE)
}
if (any(
  (genes$strand == "+" & genes$initiation_codon_center >= genes$stop_codon_center) |
    (genes$strand == "-" & genes$initiation_codon_center <= genes$stop_codon_center)
)) {
  stop("Codon anchors are inconsistent with transcription strand.", call. = FALSE)
}

write_tsv <- function(x, path) {
  utils::write.table(
    x,
    file = path,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE,
    na = ""
  )
}

write_tsv(genes, file.path(out_dir, "hox_genes.tsv"))
write_tsv(cds, file.path(out_dir, "hox_cds.tsv"))
write_tsv(clusters, file.path(out_dir, "hox_clusters.tsv"))
write_tsv(species, file.path(out_dir, "hox_species.tsv"))
write_tsv(annotation_gaps, file.path(out_dir, "hox_annotation_gaps.tsv"))
write_tsv(expected_complement, file.path(out_dir, "hox_expected_complement.tsv"))
write_tsv(slot_states, file.path(out_dir, "hox_slot_states.tsv"))
write_tsv(
  curated_transcript_exclusions,
  file.path(out_dir, "curated_transcript_exclusions.tsv")
)
write_tsv(xref_conflicts, file.path(out_dir, "hox_xref_conflicts.tsv"))

manual_mapping_out <- merge(
  manual_gene_mapping,
  genes[
    genes$gene_id %in% manual_gene_mapping$gene_id,
    c(
      "species", "gene_id", "gene_stable_id_version", "seqname",
      "gene_start", "gene_end", "strand", "transcript_id",
      "transcript_stable_id_version", "protein_stable_id_version",
      "initiation_anchor_source", "stop_anchor_source"
    ),
    drop = FALSE
  ],
  by = c("species", "gene_id"),
  all.x = TRUE,
  sort = FALSE
)
manual_mapping_out <- manual_mapping_out[
  match(
    paste(manual_gene_mapping$species, manual_gene_mapping$gene_id),
    paste(manual_mapping_out$species, manual_mapping_out$gene_id)
  ),
  ,
  drop = FALSE
]
manual_mapping_out$mapping_status <- ifelse(
  is.na(manual_mapping_out$transcript_id),
  "expected_mapping_not_selected",
  "selected_protein_coding_gene_model"
)
if (any(manual_mapping_out$mapping_status != "selected_protein_coding_gene_model")) {
  stop("A manually rescued Hox model did not reach the plotted table.", call. = FALSE)
}
write_tsv(manual_mapping_out, file.path(out_dir, "manual_hox_mapping.tsv"))

amphioxus_mapping_out <- merge(
  data.frame(
    hox_number = 1:15,
    gene_symbol = paste0("Hox", 1:15),
    stringsAsFactors = FALSE
  ),
  amphioxus_mapping,
  by = c("hox_number", "gene_symbol"),
  all.x = TRUE,
  sort = FALSE
)
amphioxus_mapping_out <- merge(
  amphioxus_mapping_out,
  genes[
    genes$species == "amphioxus",
    c("gene_id", "seqname", "gene_start", "gene_end", "strand", "transcript_id"),
    drop = FALSE
  ],
  by = "gene_id",
  all.x = TRUE,
  sort = FALSE
)
amphioxus_mapping_out <- amphioxus_mapping_out[
  match(1:15, amphioxus_mapping_out$hox_number),
  ,
  drop = FALSE
]
amphioxus_mapping_out$annotation_status <- ifelse(
  amphioxus_mapping_out$hox_number == 13L,
  "annotation_gap_no_gene_model",
  "protein_coding_gene_model"
)
amphioxus_mapping_out$mapping_method[
  amphioxus_mapping_out$hox_number == 13L
] <- "manual_collinear_annotation_gap"
amphioxus_mapping_out$mapping_note[
  amphioxus_mapping_out$hox_number == 13L
] <- annotation_gaps$mapping_note[
  annotation_gaps$species == "amphioxus" &
    annotation_gaps$hox_number == 13L
]
write_tsv(amphioxus_mapping_out, file.path(out_dir, "amphioxus_hox_mapping.tsv"))

escape_gff3 <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- gsub("%", "%25", x, fixed = TRUE)
  x <- gsub(";", "%3B", x, fixed = TRUE)
  x <- gsub("=", "%3D", x, fixed = TRUE)
  x <- gsub(",", "%2C", x, fixed = TRUE)
  x <- gsub("\t", "%09", x, fixed = TRUE)
  x
}

write_compact_gff3 <- function(species_id, path) {
  entries <- selected[startsWith(names(selected), paste0(species_id, ":"))]
  con <- file(path, open = "wt")
  on.exit(close(con), add = TRUE)
  writeLines("##gff-version 3", con)
  metadata <- species[species$species == species_id, , drop = FALSE]
  writeLines(paste0("##genome-build ", metadata$assembly), con)
  writeLines(paste0("##source-release ", metadata$source_database, " ", metadata$release), con)
  for (entry in entries) {
    if (is.null(entry$gene)) next
    g <- entry$gene
    tx <- entry$transcript
    features <- entry$features
    gene_attr <- paste0(
      "ID=gene:", escape_gff3(g$gene_id),
      ";Name=", escape_gff3(g$gene_symbol),
      ";biotype=protein_coding",
      ";hox_number=", g$hox_number,
      ";hox_cluster=", escape_gff3(g$cluster),
      ";slot=", escape_gff3(g$slot),
      ";mapping_method=", escape_gff3(g$mapping_method)
    )
    writeLines(paste(
      g$seqname, g$gene_source, "gene", g$gene_start, g$gene_end,
      ".", g$strand, ".", gene_attr,
      sep = "\t"
    ), con)
    tx_attr <- paste0(
      "ID=transcript:", escape_gff3(g$transcript_id),
      ";Parent=gene:", escape_gff3(g$gene_id),
      ";Name=", escape_gff3(ifelse(is.na(g$transcript_name), g$transcript_id, g$transcript_name)),
      ";biotype=protein_coding",
      ";selection=", escape_gff3(g$transcript_selection_rule),
      ";hox_number=", g$hox_number,
      ";hox_cluster=", escape_gff3(g$cluster),
      ";slot=", escape_gff3(g$slot),
      ";matrix_row=", escape_gff3(g$matrix_row),
      ";matrix_column=", escape_gff3(g$matrix_column),
      ";mapping_method=", escape_gff3(g$mapping_method),
      ";curated_transcript_exclusion_applied=",
      tolower(as.character(g$curated_transcript_exclusion_applied)),
      ";xref_conflict_flag=",
      tolower(as.character(g$xref_conflict_flag)),
      ";initiation_anchor_source=", escape_gff3(g$initiation_anchor_source),
      ";stop_anchor_source=", escape_gff3(g$stop_anchor_source)
    )
    if (isTRUE(g$curated_transcript_exclusion_applied)) {
      tx_attr <- paste0(
        tx_attr,
        ";excluded_transcript_ids=",
        escape_gff3(g$excluded_transcript_ids),
        ";excluded_protein_ids=",
        escape_gff3(g$excluded_protein_ids)
      )
    }
    if (isTRUE(g$xref_conflict_flag)) {
      tx_attr <- paste0(
        tx_attr,
        ";xref_conflict_note=",
        escape_gff3(g$xref_conflict_note)
      )
    }
    writeLines(paste(
      tx$seqname, tx$source, "mRNA", tx$start, tx$end,
      ".", tx$strand, ".", tx_attr,
      sep = "\t"
    ), con)
    for (feature_type in c("CDS", "start_codon", "stop_codon")) {
      rows <- features[features$feature == feature_type, , drop = FALSE]
      if (nrow(rows) == 0L) next
      if (identical(g$strand[[1L]], "+")) {
        rows <- rows[order(rows$start, rows$end), , drop = FALSE]
      } else {
        rows <- rows[order(rows$start, rows$end, decreasing = TRUE), , drop = FALSE]
      }
      for (j in seq_len(nrow(rows))) {
        feature_attr <- paste0(
          "ID=", tolower(feature_type), ":", escape_gff3(g$transcript_id), ":", j,
          ";Parent=transcript:", escape_gff3(g$transcript_id)
        )
        if (feature_type == "CDS" && !is.na(g$protein_id)) {
          feature_attr <- paste0(
            feature_attr,
            ";protein_id=", escape_gff3(g$protein_id)
          )
        }
        writeLines(paste(
          rows$seqname[[j]], rows$source[[j]], feature_type,
          rows$start[[j]], rows$end[[j]], ".", rows$strand[[j]],
          rows$phase[[j]], feature_attr,
          sep = "\t"
        ), con)
      }
    }
  }
}

gff3_attribute_value <- function(x, key) {
  pattern <- paste0("(?:^|;)", key, "=([^;]*)")
  hit <- grepl(pattern, x, perl = TRUE)
  out <- rep(NA_character_, length(x))
  out[hit] <- sub(paste0(".*", pattern, ".*"), "\\1", x[hit], perl = TRUE)
  out
}

verify_gff3_slot_roundtrip <- function(species_id, path) {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[!startsWith(lines, "#") & grepl("\tmRNA\t", lines, fixed = TRUE)]
  fields <- strsplit(lines, "\t", fixed = TRUE)
  if (any(lengths(fields) != 9L)) {
    stop("Malformed compact GFF3 mRNA row for ", species_id, call. = FALSE)
  }
  attrs <- vapply(fields, function(x) x[[9L]], character(1))
  transcript_id <- sub(
    "^transcript:",
    "",
    gff3_attribute_value(attrs, "ID")
  )
  observed <- setNames(gff3_attribute_value(attrs, "slot"), transcript_id)
  expected_rows <- genes[genes$species == species_id, , drop = FALSE]
  expected <- setNames(expected_rows$slot, expected_rows$transcript_id)
  if (
    anyDuplicated(names(observed)) ||
      !setequal(names(observed), names(expected)) ||
      !identical(
        unname(observed[sort(names(observed))]),
        unname(expected[sort(names(expected))])
      )
  ) {
    stop(
      "Compact GFF3 transcript-slot round-trip failed for ",
      species_id,
      call. = FALSE
    )
  }
  invisible(TRUE)
}

for (species_id in species$species) {
  gff3_path <- file.path(annotation_dir, paste0(species_id, ".gff3"))
  write_compact_gff3(species_id, gff3_path)
  verify_gff3_slot_roundtrip(species_id, gff3_path)
}

message(
  "Wrote ", nrow(genes), " Hox genes, ", nrow(cds),
  " CDS pieces, and ", nrow(clusters), " matrix cells to ", out_dir
)
message("Cluster counts:")
print(stats::xtabs(~ species + cluster, data = genes))
