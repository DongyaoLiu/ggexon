test_odgi_node_table <- function() {
  data.frame(
    node_id = c(1L, 2L),
    sequence = c("AC", "G"),
    XZ1516_chromosome = c("V_RagTag", "V_RagTag"),
    XZ1516_strand = c("+", "-"),
    XZ1516_absolute_start = c(100L, 102L),
    XZ1516_absolute_end = c(101L, 102L),
    N2_chromosome = c("V", "V"),
    N2_strand = c("+", "+"),
    N2_absolute_start = c(200L, 202L),
    N2_absolute_end = c(201L, 202L),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

test_that("odgi_node_table exposes the bundled script path", {
  script <- odgi_node_table_script()

  expect_true(file.exists(script))
  expect_match(basename(script), "^odgi_node_table\\.py$")
})

test_that("odgi_node_table builds a node table via the bundled Python script", {
  skip_if(.Platform$OS.type == "windows")

  python <- Sys.which("python3")
  if (!nzchar(python)) {
    python <- Sys.which("python")
  }
  skip_if(!nzchar(python), "Python is required for odgi_node_table().")

  og_file <- tempfile(fileext = ".og")
  writeLines("fake graph", og_file)

  fake_odgi <- tempfile(fileext = ".py")
  writeLines(
    c(
      "#!/usr/bin/env python3",
      "import sys",
      "",
      "cmd = sys.argv[1]",
      "if cmd == 'view':",
      "    sys.stdout.write('S\\t1\\tAC\\nS\\t2\\tG\\nP\\tXZ1516#V_RagTag:100-102\\t1+,2-\\t*\\n')",
      "elif cmd == 'paths':",
      "    sys.stdout.write('XZ1516#V_RagTag:100-102\\n')",
      "elif cmd == 'position':",
      "    sys.stdout.write('path\\tnode_id\\tposition\\nXZ1516#V_RagTag:100-102\\t1\\t0\\nXZ1516#V_RagTag:100-102\\t2\\t2\\n')",
      "else:",
      "    raise SystemExit(f'Unexpected command: {cmd}')"
    ),
    fake_odgi
  )
  Sys.chmod(fake_odgi, mode = "0755")

  out_path <- odgi_node_table(
    og_file = og_file,
    odgi = fake_odgi,
    python = python,
    read = FALSE
  )

  expect_true(file.exists(out_path))

  tbl <- odgi_node_table(
    og_file = og_file,
    odgi = fake_odgi,
    python = python,
    read = TRUE
  )

  expect_s3_class(tbl, "data.frame")
  expect_identical(tbl$node_id, c(1L, 2L))
  expect_identical(tbl$sequence, c("AC", "G"))
  expect_identical(tbl$XZ1516_chromosome, c("V_RagTag", "V_RagTag"))
  expect_identical(tbl$XZ1516_strand, c("+", "-"))
  expect_identical(tbl$XZ1516_absolute_start, c(100L, 102L))
  expect_identical(tbl$XZ1516_absolute_end, c(101L, 102L))
})

test_that("odgi_multi_alignment parses an ODGI node table into SynMultiAlignment", {
  tbl <- test_odgi_node_table()

  msa <- odgi_multi_alignment(tbl, name = "worm-graph")

  expect_s4_class(msa, "SynMultiAlignment")
  expect_identical(annotation_name(msa), "worm-graph")
  expect_identical(msa@format, "odgi")
  expect_identical(msa@individuals, c("XZ1516", "N2"))
  expect_identical(msa@source_file, "<odgi-node-table>")
  expect_identical(msa@metadata$odgi_labels, c(XZ1516 = "XZ1516", N2 = "N2"))
  expect_identical(multiple_alignment_data(msa), tbl)
})

test_that("odgi_multi_alignment supports file-backed tables and SynSpecies lookup", {
  tbl <- test_odgi_node_table()
  tsv <- tempfile(fileext = ".tsv")
  utils::write.table(tbl, file = tsv, sep = "\t", quote = FALSE, row.names = FALSE)

  msa <- odgi_multi_alignment(
    tsv,
    name = "worm-graph",
    individuals = c(XZ1516 = "caenorhabditis_XZ1516", N2 = "caenorhabditis_N2")
  )

  expect_identical(msa@individuals, c("caenorhabditis_XZ1516", "caenorhabditis_N2"))
  expect_identical(
    msa@metadata$odgi_labels,
    c(caenorhabditis_XZ1516 = "XZ1516", caenorhabditis_N2 = "N2")
  )

  lazy_msa <- SynMultiAlignment(
    name = "worm-graph-lazy",
    individuals = c("caenorhabditis_XZ1516", "caenorhabditis_N2"),
    file = tsv,
    format = "odgi"
  )
  expect_identical(multiple_alignment_data(lazy_msa), tbl)

  sp <- SynSpecies(name = "worms")
  sp <- add_multiple_alignment(sp, msa)
  expect_identical(multiple_alignment_data(sp, alignment = "worm-graph"), tbl)
})

test_that("load_alignment infers ODGI label mappings for manual SynMultiAlignment objects", {
  tbl <- test_odgi_node_table()
  tsv <- tempfile(fileext = ".tsv")
  utils::write.table(tbl, file = tsv, sep = "\t", quote = FALSE, row.names = FALSE)

  msa <- SynMultiAlignment(
    name = "worm-graph-manual",
    individuals = c("XZ1516", "N2.w285"),
    file = tsv,
    format = "odgi"
  ) |>
    load_alignment()

  expect_identical(
    msa@metadata$odgi_labels,
    c(XZ1516 = "XZ1516", N2.w285 = "N2")
  )

  pair <- odgi_pairwise_alignment(
    msa,
    query_individual = "XZ1516",
    target_individual = "N2.w285"
  )

  paf_like <- pairwise_alignment_data(pair)
  expect_identical(nrow(paf_like), 2L)
  expect_identical(paf_like$tchr, c("V", "V"))
  expect_identical(paf_like$tstart, c(200L, 202L))
})

test_that("odgi alignments can load directly from .og graph files", {
  skip_if(.Platform$OS.type == "windows")

  python <- Sys.which("python3")
  if (!nzchar(python)) {
    python <- Sys.which("python")
  }
  skip_if(!nzchar(python), "Python is required for ODGI graph loading.")

  og_file <- tempfile(fileext = ".og")
  writeLines("fake graph", og_file)

  fake_odgi <- tempfile(fileext = ".py")
  writeLines(
    c(
      "#!/usr/bin/env python3",
      "import sys",
      "",
      "cmd = sys.argv[1]",
      "if cmd == 'view':",
      "    sys.stdout.write('S\\t1\\tAC\\nS\\t2\\tG\\nP\\tXZ1516#V_RagTag:100-102\\t1+,2-\\nP\\tN2#V:200-202\\t1+,2+\\n')",
      "elif cmd == 'paths':",
      "    sys.stdout.write('XZ1516#V_RagTag:100-102\\nN2#V:200-202\\n')",
      "elif cmd == 'position':",
      "    sys.stdout.write('path\\tnode_id\\tposition\\nXZ1516#V_RagTag:100-102\\t1\\t0\\nXZ1516#V_RagTag:100-102\\t2\\t2\\nN2#V:200-202\\t1\\t0\\nN2#V:200-202\\t2\\t2\\n')",
      "else:",
      "    raise SystemExit(f'Unexpected command: {cmd}')"
    ),
    fake_odgi
  )
  Sys.chmod(fake_odgi, mode = "0755")

  old_odgi <- Sys.getenv("ODGI_BIN", unset = NA_character_)
  on.exit({
    if (is.na(old_odgi)) Sys.unsetenv("ODGI_BIN") else Sys.setenv(ODGI_BIN = old_odgi)
  }, add = TRUE)
  Sys.setenv(ODGI_BIN = fake_odgi)

  msa <- SynMultiAlignment(
    name = "worm-graph-og",
    individuals = c("XZ1516", "N2"),
    file = og_file,
    format = "odgi"
  )

  loaded <- load_alignment(msa)

  expect_true(is.data.frame(loaded@data))
  expect_true(isTRUE(loaded@loaded))
  expect_false(isTRUE(loaded@lazy))
  expect_identical(loaded@data$node_id, c(1L, 2L))
  expect_identical(loaded@data$XZ1516_chromosome, c("V_RagTag", "V_RagTag"))
  expect_identical(loaded@data$N2_chromosome, c("V", "V"))

  loaded_explicit <- load_alignment(msa, odgi = fake_odgi, python = python)
  expect_identical(loaded_explicit@data$node_id, c(1L, 2L))
})

test_that("odgi_multi_alignment validates path label groups", {
  bad_tbl <- data.frame(
    node_id = 1L,
    sequence = "AC",
    XZ1516_chromosome = "V_RagTag",
    XZ1516_strand = "+",
    XZ1516_absolute_start = 100L,
    XZ1516_absolute_end = 101L,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  expect_error(
    odgi_multi_alignment(bad_tbl),
    "at least two path label groups"
  )
})

test_that("odgi_pairwise_alignment derives a PAF-like pairwise table", {
  tbl <- test_odgi_node_table()

  pair <- odgi_pairwise_alignment(
    tbl,
    query_individual = "XZ1516",
    target_individual = "N2",
    name = "XZ1516_vs_N2_odgi"
  )

  expect_s4_class(pair, "SynPairAlignment")
  expect_identical(alignment_name(pair), "XZ1516_vs_N2_odgi")
  expect_identical(alignment_format(pair), "odgi")
  expect_identical(query_individual(pair), "XZ1516")
  expect_identical(target_individual(pair), "N2")

  paf_like <- pairwise_alignment_data(pair)
  expect_true(is.data.frame(paf_like))
  expect_identical(names(paf_like)[seq_len(12L)], c(
    "qchr", "qlen", "qstart", "qend", "strand",
    "tchr", "tlen", "tstart", "tend", "nmatch", "alen", "mapq"
  ))
  expect_identical(paf_like$qchr, c("V_RagTag", "V_RagTag"))
  expect_identical(paf_like$tchr, c("V", "V"))
  expect_identical(paf_like$strand, c("+", "-"))
  expect_identical(paf_like$qstart, c(100L, 102L))
  expect_identical(paf_like$tstart, c(200L, 202L))
  expect_identical(paf_like$alen, c(2L, 1L))
})

test_that("odgi parser ignores path chromosome labels for nodes absent from that path", {
  tbl <- data.frame(
    node_id = c(1L, 2L),
    sequence = c("AC", "G"),
    XZ1516_chromosome = c("V_RagTag", "V_RagTag"),
    XZ1516_strand = c("+", "-"),
    XZ1516_absolute_start = c(100L, 102L),
    XZ1516_absolute_end = c(101L, 102L),
    CB4856_chromosome = c("CP084673.1", "CP084673.1"),
    CB4856_strand = c("NA", "+"),
    CB4856_absolute_start = c("NA", "300"),
    CB4856_absolute_end = c("NA", "300"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  msa <- odgi_multi_alignment(
    tbl,
    name = "worm-graph-partial",
    individuals = c(XZ1516 = "XZ1516", CB4856 = "CB4856")
  )

  pair <- odgi_pairwise_alignment(
    msa,
    query_individual = "XZ1516",
    target_individual = "CB4856"
  )

  paf_like <- pairwise_alignment_data(pair)
  expect_identical(nrow(paf_like), 1L)
  expect_identical(paf_like$qstart, 102L)
  expect_identical(paf_like$tstart, 300L)
})
