test_that("preparation shares inserted keys and every copy in native-coordinate slots", {
  x <- prepare_gene_lollipop(prepared_gene_lollipop_fixture(), where = list(gene_role = "MHC"),
    key = "shared", reference = "ref", nrow = 2)
  genes <- query_gene_lollipop(x)
  slots <- query_gene_lollipop(x, component = "slots")
  expect_equal(nrow(genes), 8L)
  expect_equal(slots$shared_key, c("A", "X", "B", "C", "C"))
  expect_equal(slots$head_row, c(2L, 1L, 2L, 1L, 2L))
  expect_equal(slots$head_col, c(1L, 1L, 2L, 2L, 3L))
  expect_equal(unique(genes$grid_ncol), 3L)
  expect_equal(genes$x[genes$individual == "ref"], c(120, 220, 320))
  expect_equal(genes$strand[genes$individual == "ref"], c("+", "-", "+"))
  expect_equal(genes$gene_name[genes$individual == "ref"], c("source_A", "source_B", "source_C"))
  expect_equal(genes$label[genes$individual == "ref"], c("A", "B", "C"))
  expect_equal(genes$fill, rep("#0077BB", 8))
  expect_true(all(genes$gene_role == "MHC"))
  expect_equal(slots$n_occupants, c(2L, 1L, 2L, 2L, 1L))
  expect_equal(slots$occupied_by[[2L]], "other")
  expect_equal(slots$copy_auto, rep(TRUE, 5))
  expect_equal(length(unique(genes$lollipop_grid)), 1L)
  expect_false(any(c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y") %in% names(genes)))
  expect_equal(query_gene_lollipop(individuals(x)[["ref"]])$head_col, c(1L, 2L, 2L))
})

test_that("grid dimensions and slot overrides enforce capacity and order", {
  fixture <- prepared_gene_lollipop_fixture()
  args <- list(x = fixture, key = "shared", reference = "ref", where = list(gene_role = "MHC"))
  expect_error(do.call(prepare_gene_lollipop, args), "nrow|ncol")
  expect_error(do.call(prepare_gene_lollipop, c(args, list(nrow = 2, ncol = 2))), "capacity|slots")
  expect_error(do.call(prepare_gene_lollipop, c(args, list(nrow = 1.5))), "integer")
  expect_error(do.call(prepare_gene_lollipop, c(args, list(nrow = 2, slots = data.frame(
    shared_key = c("A", "B"), head_row = c(2, 2), head_col = c(1, 1))))), "collid|same cell")
  expect_error(do.call(prepare_gene_lollipop, c(args, list(nrow = 2, slots = data.frame(
    shared_key = c("A", "B"), head_row = c(2, 1), head_col = c(2, 1))))), "order")
  x <- do.call(prepare_gene_lollipop, c(args, list(ncol = 3, slots = data.frame(
    shared_key = "B", head_row = 1, head_col = 2))))
  slots <- query_gene_lollipop(x, component = "slots")
  expect_equal(slots$head_row, c(2L, 1L, 1L, 2L, 1L))
  expect_equal(slots$head_col, c(1L, 1L, 2L, 3L, 3L))
  expect_true(slots$slot_explicit[slots$shared_key == "B"])
})

test_that("ambiguous reference flanks require complete explicit ordering", {
  x <- add_individual(SynSpecies("ambiguous"), prepared_gene_lollipop_individual("ref", c("A", "B")))
  x <- add_individual(x, prepared_gene_lollipop_individual("one", c("A", "X", "B")))
  x <- add_individual(x, prepared_gene_lollipop_individual("two", c("A", "Y", "B")))
  expect_error(prepare_gene_lollipop(x, key = "shared", reference = "ref", nrow = 2), "ambiguous.*X.*Y|ambiguous.*Y.*X")
  expect_error(prepare_gene_lollipop(x, key = "shared", reference = "ref", nrow = 2,
    order = c("A", "X", "B")), "complete")
  x <- prepare_gene_lollipop(x, key = "shared", reference = "ref", nrow = 2,
    order = c("A", "Y", "X", "B"))
  expect_equal(query_gene_lollipop(x, component = "slots")$shared_key, c("A", "Y", "X", "B"))
  expect_false(any(query_gene_lollipop(x, component = "slots")$order_auto))
})

test_that("explicit copy keys retain correspondence when native copy order differs", {
  x <- add_individual(SynSpecies("copies"), prepared_gene_lollipop_individual("ref", c("A", "C", "C"), copies = c("a", "first", "second")))
  x <- add_individual(x, prepared_gene_lollipop_individual("other", c("A", "C", "C"), copies = c("a", "second", "first")))
  x <- prepare_gene_lollipop(x, key = "shared", copy_key = "copy", reference = "ref", nrow = 2)
  genes <- query_gene_lollipop(x)
  expect_equal(genes$head_col[genes$individual == "other"], c(1L, 2L, 1L))
  expect_false(any(query_gene_lollipop(x, component = "slots")$copy_auto))
  bad <- prepared_gene_lollipop_individual("bad", c("C", "C"), copies = c("same", "same"))
  expect_error(prepare_gene_lollipop(bad, key = "shared", copy_key = "copy", ncol = 2), "copy.*unique|unique.*copy")
})

test_that("homology requires actual hits and rejects conflicting mappings", {
  x <- add_individual(SynSpecies("homology"), prepared_gene_lollipop_individual("ref", c("A", "B")))
  x <- add_individual(x, prepared_gene_lollipop_individual("other", c("A", "B")))
  hit <- HomologyAnnotation("hits", "ref", "other", data.frame(query_gene = "other_1", reference_gene = "ref_1"))
  x <- add_homology_annotation(x, hit)
  ready <- prepare_gene_lollipop(x, nrow = 2, order = c("ref_1", "ref_2", "other::other_2"))
  genes <- query_gene_lollipop(ready)
  expect_equal(genes$shared_key[genes$individual == "other"], c("ref_1", "other::other_2"))
  expect_error(prepare_gene_lollipop(x, key = "shared", homology = "hits", nrow = 2), "key.*homology|homology.*key")
  conflicting <- HomologyAnnotation("conflict", "ref", "other", data.frame(query_gene = "other_1", reference_gene = "ref_2"))
  x <- add_homology_annotation(x, conflicting)
  expect_error(prepare_gene_lollipop(x, nrow = 2), "[Cc]onflict.*homology|homology.*conflict")
  chosen <- prepare_gene_lollipop(x, homology = "hits", nrow = 2, order = c("ref_1", "ref_2", "other::other_2"))
  expect_equal(nrow(query_gene_lollipop(chosen)), 4L)
  x@homology_annotations$hits@homology_table$reference_gene <- "ref_2"
  ready@homology_annotations <- x@homology_annotations
  expect_error(query_gene_lollipop(ready), "stale|changed|again")
})

test_that("source edits and cleared caches invalidate preparation instead of falling back", {
  x <- prepare_gene_lollipop(prepared_gene_lollipop_individual("ref", c("A", "B")), key = "shared", nrow = 2)
  changed <- x
  gr <- annotation_data(changed)
  S4Vectors::mcols(gr)$plot_label[1] <- "Changed"
  annotation_data(changed) <- gr
  expect_error(query_gene_lollipop(changed), "stale|invalidated|again")
  layer <- geom_gene_lollipop()
  expect_error(.gene_lollipop_prepared_data(changed, layer), "stale|invalidated|again")
  cleared <- x
  cleared@plot_cache <- list()
  expect_error(query_gene_lollipop(cleared), "invalidated|again")
  expect_error(.gene_lollipop_prepared_data(cleared, layer), "invalidated|again")
  x <- prepare_gene_lollipop(x, key = "shared", nrow = 2, name = "another")
  expect_error(query_gene_lollipop(x), "multiple|name")
  expect_equal(nrow(query_gene_lollipop(x, "another")), 2L)
  expect_null(.gene_lollipop_prepared_data(prepared_gene_lollipop_individual("fresh", "A"), layer))
})

test_that("context dispatch repeats tracks and crops anchors without repacking slots", {
  x <- prepare_gene_lollipop(prepared_gene_lollipop_fixture(), where = list(gene_role = "MHC"),
    key = "shared", reference = "ref", nrow = 2)
  layer <- geom_gene_lollipop()
  layer$syn_plot_context <- list(windows = list(
    first = list(individual = "other", chr = "chr1", start = 100, end = 250),
    second = list(individual = "other", chr = "chr1", start = 350, end = 550)))
  out <- .gene_lollipop_prepared_data(x, layer)
  expect_equal(out$track, c("first", "first", "second", "second"))
  expect_equal(out$x, c(120, 220, 420, 520))
  expect_equal(out$head_row, c(2L, 1L, 1L, 2L))
  expect_equal(out$head_col, c(1L, 1L, 2L, 3L))
  expect_true(all(out$grid_nrow == 2 & out$grid_ncol == 3))
  expect_true(all(out$genomic_x == out$x & out$y == 0))
  expect_true(all(out$individual == "other"))
  layer$syn_plot_context <- NULL
  layer$geom_params$species <- "ref"
  layer$geom_params$chr <- "chr1"
  layer$geom_params$subset <- c(200, 250)
  expect_equal(.gene_lollipop_prepared_data(x, layer)$label, "B")
})

test_that("gene selectors combine with metadata filters and permit empty participants", {
  x <- prepared_gene_lollipop_fixture()
  ready <- prepare_gene_lollipop(x, genes = list(ref = c("source_A", "ref_3"), other = "other_1"),
    where = list(gene_role = c("MHC", "other"), shared = c("A", "C")), key = "shared",
    reference = "ref", ncol = 2)
  expect_equal(query_gene_lollipop(ready)$gene_id, c("ref_1", "ref_3", "other_1"))
  empty <- prepare_gene_lollipop(x, where = list(shared = "X"), key = "shared",
    reference = "ref", nrow = 2)
  expect_equal(query_gene_lollipop(empty)$gene_id, "other_2")
  expect_equal(nrow(query_gene_lollipop(individuals(empty)[["ref"]])), 0L)
  expect_error(prepare_gene_lollipop(x, key = "shared", nrow = 2), "reference")
  expect_error(prepare_gene_lollipop(x, key = "missing", reference = "ref", nrow = 2), "metadata.*missing")
})

test_that("a selected track still validates all available participants and grid identities", {
  ready <- prepare_gene_lollipop(prepared_gene_lollipop_fixture(), where = list(gene_role = "MHC"),
    key = "shared", reference = "ref", nrow = 2)
  layer <- geom_gene_lollipop(species = "ref")
  stale <- ready
  gr <- annotation_data(stale@individuals$other)
  S4Vectors::mcols(gr)$shared[1] <- "edited"
  annotation_data(stale@individuals$other) <- gr
  expect_error(.gene_lollipop_prepared_data(stale, layer), "stale|invalidated|again")
  mixed <- ready
  mixed@individuals$other <- prepare_gene_lollipop(mixed@individuals$other,
    key = "shared", reference = "other", nrow = 2)
  expect_error(.gene_lollipop_prepared_data(mixed, layer), "incompatible|together")
})

test_that("new automatically discovered homology inputs invalidate the old recipe", {
  x <- add_individual(SynSpecies("homology"), prepared_gene_lollipop_individual("ref", c("A", "B")))
  x <- add_individual(x, prepared_gene_lollipop_individual("other", c("A", "B")))
  x <- add_homology_annotation(x, HomologyAnnotation("hits", "ref", "other",
    data.frame(query_gene = c("other_1", "other_2"), reference_gene = c("ref_1", "ref_2"))))
  auto <- prepare_gene_lollipop(x, nrow = 2)
  explicit <- prepare_gene_lollipop(x, homology = "hits", nrow = 2)
  extra <- HomologyAnnotation("new", "ref", "other",
    data.frame(query_gene = "other_1", reference_gene = "ref_2"))
  expect_error(query_gene_lollipop(add_homology_annotation(auto, extra)), "homology.*changed|again")
  expect_equal(nrow(query_gene_lollipop(add_homology_annotation(explicit, extra))), 4L)
  expect_equal(nrow(query_gene_lollipop(individuals(auto)[["other"]])), 2L)
})

test_that("cropped dispatch retains full prepared labels for common text measurement", {
  x <- prepare_gene_lollipop(prepared_gene_lollipop_individual("ref", c("A", "LongestLabel")),
    key = "shared", nrow = 2)
  layer <- geom_gene_lollipop(chr = "chr1", subset = c(100, 150))
  rows <- .gene_lollipop_prepared_data(x, layer)
  expect_equal(rows$label, "A")
  expect_equal(attr(rows, "gene_lollipop_labels"), c("A", "LongestLabel"))
})

test_that("private genes in reversed native runs use reference-oriented flanks", {
  x <- add_individual(SynSpecies("reverse"), prepared_gene_lollipop_individual("ref", c("A", "B", "C")))
  x <- add_individual(x, prepared_gene_lollipop_individual("other", c("C", "X", "B", "A")))
  ready <- prepare_gene_lollipop(x, key = "shared", reference = "ref", nrow = 2)
  expect_equal(query_gene_lollipop(ready, component = "slots")$shared_key, c("A", "B", "X", "C"))
  genes <- query_gene_lollipop(ready)
  expect_equal(genes$x[genes$individual == "other"], c(120, 220, 320, 420))
  expect_equal(genes$head_col[genes$individual == "other"], c(2L, 2L, 1L, 1L))
})

test_that("real homology matches normalized ID aliases without treating labels as hits", {
  x <- add_individual(SynSpecies("normalized"), prepared_gene_lollipop_individual("ref", c("A", "B")))
  other <- prepared_gene_lollipop_individual("other", c("A", "B"))
  ann <- get_annotation(other)
  S4Vectors::mcols(ann@annotation)$homology_query_aliases <- c("unmatched\rtranscript:alias_1.t2", "no_hit")
  other@annotations[[annotation_name(ann)]] <- ann
  x <- add_individual(x, other)
  x <- add_homology_annotation(x, HomologyAnnotation("normalized", "ref", "other",
    data.frame(query_gene = "gene:alias_1", reference_gene = "cds:ref_1.t3")))
  ready <- prepare_gene_lollipop(x, nrow = 2,
    order = c("ref_1", "ref_2", "other::other_2"))
  expect_equal(query_gene_lollipop(ready)$shared_key, c("ref_1", "ref_2", "ref_1", "other::other_2"))
  x <- add_homology_annotation(x, HomologyAnnotation("conflict", "ref", "other",
    data.frame(query_gene = "mRNA:alias_1-T1", reference_gene = "ref_2")))
  expect_error(prepare_gene_lollipop(x, nrow = 2), "[Cc]onflict.*homology|homology.*conflict")
})

test_that("fingerprints reject source mutation even when plot caches remain populated", {
  x <- prepare_gene_lollipop(prepared_gene_lollipop_individual("ref", c("A", "B")), key = "shared", nrow = 2)
  ann <- get_annotation(x)
  S4Vectors::mcols(ann@annotation)$plot_label[1L] <- "Changed directly"
  x@annotations[[annotation_name(ann)]] <- ann
  expect_error(query_gene_lollipop(x), "stale.*metadata|source.*changed")
})

test_that("unselected nonreference individuals retain an empty prepared entry", {
  x <- prepared_gene_lollipop_fixture()
  ready <- prepare_gene_lollipop(x, genes = list(ref = "ref_1", other = "missing"),
    reference = "ref", nrow = 2)
  expect_equal(query_gene_lollipop(ready)$gene_id, "ref_1")
  expect_equal(nrow(query_gene_lollipop(individuals(ready)[["other"]])), 0L)
})

test_that("explicit copies survive source columns also used by prepared output", {
  for (column in c("copy_id", "label", "fill", "shared_key", "individual")) {
    x <- add_individual(SynSpecies("source-copies"), prepared_gene_lollipop_individual(
      "ref", c("A", "C", "C"), copies = c("a", "first", "second"), copy_column = column))
    x <- add_individual(x, prepared_gene_lollipop_individual(
      "other", c("A", "C", "C"), copies = c("a", "second", "first"), copy_column = column))
    ready <- prepare_gene_lollipop(x, key = "shared", copy_key = column,
      reference = "ref", nrow = 2)
    rows <- query_gene_lollipop(ready)
    expect_equal(rows$copy_id, c("a", "first", "second", "a", "second", "first"), info = column)
    expect_equal(rows$head_col[rows$individual == "other"], c(1L, 2L, 1L), info = column)
    expect_false(any(query_gene_lollipop(ready, component = "slots")$copy_auto), info = column)
  }
})

test_that("copy grouping distinguishes dotted individual IDs and shared keys", {
  x <- add_individual(SynSpecies("dotted-copies"),
    prepared_gene_lollipop_individual("ref", c("anchor", "A.X")))
  x <- add_individual(x, prepared_gene_lollipop_individual("ref.A", c("anchor", "X")))
  ready <- prepare_gene_lollipop(x, key = "shared", reference = "ref", nrow = 1, ncol = 3,
    order = c("anchor", "A.X", "X"))
  rows <- query_gene_lollipop(ready)
  expect_equal(rows$copy_id, rep("1", 4L))
  expect_equal(query_gene_lollipop(ready, component = "slots")$shared_key, c("anchor", "A.X", "X"))
  expect_equal(rows$head_col, c(1L, 2L, 1L, 3L))
})

test_that("flank grouping distinguishes dotted individual IDs and chromosome names", {
  x <- add_individual(SynSpecies("dotted-flanks"), prepared_gene_lollipop_individual(
    "ref", c("A", "B", "C"), chr = "A.chr1"))
  x <- add_individual(x, prepared_gene_lollipop_individual(
    "ref.A", c("B", "X", "C"), chr = "chr1", starts = c(80, 130, 380)))
  ready <- prepare_gene_lollipop(x, key = "shared", reference = "ref", nrow = 2)
  expect_equal(query_gene_lollipop(ready, component = "slots")$shared_key, c("A", "B", "X", "C"))
  expect_equal(query_gene_lollipop(ready)$x, c(120, 220, 320, 100, 150, 400))
})
