# ggexon Repository Audit Remediation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Repair the confirmed security, coordinate, cache, API, documentation, provenance, packaging, and CI defects in ggexon while explicitly deferring full BCF and file-backed MAF parsers.

**Architecture:** Keep the existing S4 class layout and large source-file organization, but make each public boundary honest and internally consistent. Normalize source alignment coordinates at import, make authoritative annotation replacement invalidate all dependent state, keep query methods value-returning, and separate read-only build validation from write-enabled publication. Each remediation unit has a focused red/green test cycle followed by a repository-wide release gate.

**Tech Stack:** R 4.1+, S4, Bioconductor `GRanges`/`Seqinfo`, Rsamtools Tabix access, ggplot2 4.0+, testthat 3, roxygen2, rmarkdown/knitr, pkgdown, GitHub Actions.

## Global Constraints

- [ ] Work in the current checkout only after the user chooses an execution mode; keep every change unstaged and uncommitted unless the user separately asks for a commit.
- [ ] Preserve the user's unrelated untracked research directories `data-raw/notch2nl_primate/` and `data-raw/smn_splice_candidate/`, and preserve all ignored local research assets under `inst/extdata/`.
- [ ] Treat public genomic coordinates as one-based closed intervals. Convert PAF/PSL zero-based starts with `start + 1L`; retain source ends unchanged.
- [ ] Support VCF and custom Tabix index paths. Reject `.bcf` and `.bcf.gz` explicitly; do not add a BCF parser.
- [ ] Allow `SynMultiAlignment` to store caller-supplied parsed MAF data. Reject file-backed MAF loading explicitly; do not add a MAF parser.
- [ ] Declare `R (>= 4.1)` and `ggplot2 (>= 4.0.0)` in `DESCRIPTION`.
- [ ] Generate `NAMESPACE` and `man/` only through `devtools::document()`; do not hand-edit generated files.
- [ ] Use failure-first focused tests for behavioral fixes and rerun each focused slice after implementation.
- [ ] Keep package-owned tests deterministic, offline, and independent of author-machine paths.
- [ ] Before claiming completion, run the complete tests, documentation idempotence check, `R CMD build`, `R CMD check --as-cran`, pkgdown build, and archive-content audit from an isolated temporary source copy.
- [ ] Do not report an environmental failure as a package failure. Record the exact command and error for missing TeX, Pandoc, network, repository, or external system-tool failures.

## File and Interface Map

| Responsibility | Source files | Primary tests |
| --- | --- | --- |
| Literal ODGI process arguments | `R/odgi.R` | `tests/testthat/test-odgi.R` |
| Feature replacement and cache semantics | `R/SynIndividual-class.R`, `R/SynAnnotation-class.R`, `R/protein-mutation-annotation.R` | `tests/testthat/test-synannotation.R`, `tests/testthat/test-synindividual.R`, `tests/testthat/test-protein-mutation-annotation.R` |
| VCF indexed querying and BCF rejection | `R/SynAnnotation-class.R` | `tests/testthat/test-synannotation.R` |
| PAF/PSL coordinate normalization | `R/SynSpecies-class.R` | `tests/testthat/test-pairwise-coordinate-contract.R`, `tests/testthat/test-paf-detail.R`, `tests/testthat/test-psl.R`, `tests/testthat/test-synspecies.R` |
| BLAST metadata and public accessors | `R/homology-annotation.R`, `R/SynIndividual-class.R`, `_pkgdown.yml` | `tests/testthat/test-homology-annotation.R`, `tests/testthat/test-synindividual.R` |
| Honest tutorials and MAF limits | `README.md`, `vignettes/ggexon-classes-and-verbs.Rmd`, `vignettes/ggexon-workflow.Rmd`, `R/SynSpecies-class.R` | direct vignette renders and alignment tests |
| Package metadata and portable manual | `DESCRIPTION`, `R/ggexon-package.R`, `R/geom_genelabel.R` | `tests/testthat/test-package-contract.R`, `R CMD Rd2pdf` |
| Portable fixtures and repository hygiene | `data-raw/`, `inst/extdata/`, `.Rbuildignore`, `.gitignore` | `tests/testthat/test-provenance-portability.R`, dataset tests, archive audit |
| Continuous integration | `.github/workflows/R-CMD-check.yaml`, `.github/workflows/pkgdown.yaml` | YAML/static inspection and release gates |

The public behavioral interfaces retained or clarified by this plan are:

```r
annotation_data(x) <- value
query_variants(x, chr, start, end)
query_domains(x, ids = NULL, domains = NULL)
protein_mutation_data(x)
pairwise_alignment_data(x, ...)
read_pairwise_psl(path, query_individual = NULL, target_individual = NULL, more = FALSE)
syn_id(x)
protein_seq(x)
projected_domains(x)
load_alignment(x, ...)
```

---

### Task 1: Preserve ODGI Arguments as Literal Process Arguments

**Files:**

- Modify: `tests/testthat/test-odgi.R`
- Modify: `R/odgi.R:39-45`

**Interfaces:**

- Consumes: `.run_python_script(python, script, args)` where each input is character data.
- Produces: the same character stdout contract, with every element of `c(script, args)` passed as one literal child-process argument.

- [ ] **Step 1: Add a harmless injection and whitespace regression test.**

Append this test to `tests/testthat/test-odgi.R`:

```r
test_that(".run_python_script preserves literal argv boundaries", {
  skip_on_os("windows")
  python <- Sys.which("python3")
  skip_if(!nzchar(python), "python3 is not installed")

  root <- tempfile("ggexon odgi ; literal ")
  dir.create(root)
  script <- file.path(root, "capture argv.py")
  capture <- file.path(root, "captured argv.txt")
  marker <- file.path(root, "GGEXON_PWNED")
  payload <- paste0("literal; touch ", marker)

  writeLines(c(
    "from pathlib import Path",
    "import sys",
    "Path(sys.argv[1]).write_text('\\n'.join(sys.argv[2:]))"
  ), script)

  .run_python_script(
    python,
    script,
    c(capture, "value with spaces", payload)
  )

  expect_false(file.exists(marker))
  expect_identical(readLines(capture), c("value with spaces", payload))
})
```

- [ ] **Step 2: Run the focused test and observe the current unsafe behavior.**

Run:

```sh
Rscript -e 'devtools::test(filter = "odgi", stop_on_failure = TRUE)'
```

Expected RED: the script/output path is split or the temporary `GGEXON_PWNED` marker is created because raw arguments reach the shell.

- [ ] **Step 3: Quote each argument once at the R process boundary.**

Change only the `args` expression inside `.run_python_script()`:

```r
output <- system2(
  python,
  args = shQuote(c(script, args)),
  stdout = TRUE,
  stderr = TRUE
)
```

Do not collapse the vector to one string and do not quote `python`; `system2()` resolves the executable separately.

- [ ] **Step 4: Run the ODGI test slice green.**

Run:

```sh
Rscript -e 'devtools::test(filter = "odgi", stop_on_failure = TRUE)'
```

Expected GREEN: the existing fake-ODGI integration tests and the literal-argv test pass, and no marker file is created.

- [ ] **Step 5: Review the narrow diff.**

Run:

```sh
git diff --check -- R/odgi.R tests/testthat/test-odgi.R
git diff -- R/odgi.R tests/testthat/test-odgi.R
```

Leave the changes unstaged and uncommitted.

---

### Task 2: Make Feature Replacement Authoritative and Query Mutation Semantics Honest

**Files:**

- Modify: `tests/testthat/test-synannotation.R`
- Modify: `tests/testthat/test-synindividual.R`
- Modify: `R/SynIndividual-class.R:2575-2597`
- Modify: `R/SynAnnotation-class.R:35-148, 752-792, 1231-1317`
- Modify: `R/protein-mutation-annotation.R:444-460`
- Generate: affected `man/SynFeatureAnnotation*.Rd`, `man/SynVCFAnnotation*.Rd`, `man/SynProteinDomainAnnotation*.Rd`, and `man/SynProteinMutationAnnotation*.Rd`

**Interfaces:**

- Consumes: replacement `value` as `GRanges` or `NULL`.
- Produces: a replacement annotation whose base equals the new active value, with patches, labels, feature index, sequence caches, and plot cache cleared.
- Produces: an updated `SynIndividual` whose active layer, owner plot cache, and `seqinfo` agree with the replacement.
- Preserves: `set_gene_labels()` changes labels and clears plotting state without discarding patches, indexes, or sequence caches.
- Clarifies: data-returning query helpers may read pre-populated slots but do not mutate the caller's S4 object.

- [ ] **Step 1: Add the feature-layer replacement regression.**

Add this focused test to `tests/testthat/test-synannotation.R`:

```r
test_that("annotation_data replacement resets authoritative feature state", {
  old <- GenomicRanges::GRanges("chrOld", IRanges::IRanges(10, 20))
  replacement <- GenomicRanges::GRanges("chrNew", IRanges::IRanges(30, 40))
  ann <- SynFeatureAnnotation("features", "fixture.gff3")
  ann@annotation <- old
  ann@base_annotation <- old
  ann@patches <- list(SynAnnotationPatch("patch", old, target_ids = "g1"))
  ann@label_map <- data.frame(feature_id = "g1", plot_label = "old")
  ann@feature_index <- list(gene = 1L)
  ann@nucleotide_seq <- stats::setNames(Biostrings::DNAStringSet("ATG"), "old")
  ann@protein_seq <- stats::setNames(Biostrings::AAStringSet("M"), "old")
  ann@plot_cache <- list(old = data.frame(x = 1L))
  ann@loaded <- TRUE

  annotation_data(ann) <- replacement

  expect_identical(annotation_data(ann), replacement)
  expect_identical(base_annotation(ann), replacement)
  expect_length(list_patches(ann), 0L)
  expect_null(label_map(ann))
  expect_null(feature_index(ann))
  expect_null(nucleotide_seq(ann))
  expect_null(protein_seq(ann))
  expect_identical(plot_cache(ann), list())
  expect_true(is_loaded(ann))

  annotation_data(ann) <- NULL
  expect_null(annotation_data(ann))
  expect_null(base_annotation(ann))
  expect_false(is_loaded(ann))
})
```

- [ ] **Step 2: Add the owner-state and label-preservation regressions.**

Add this complete owner test to `tests/testthat/test-synindividual.R`:

```r
test_that("annotation_data replacement refreshes SynIndividual owner state", {
  x <- SynIndividual(id = "sample")
  x <- add_annotation(
    x,
    SynFeatureAnnotation("default", "fixture.gff3"),
    set_active = TRUE
  )
  x@plot_cache <- list(stale = TRUE)
  replacement <- GenomicRanges::GRanges(
    "chrReplacement",
    IRanges::IRanges(5, 12),
    seqinfo = GenomeInfoDb::Seqinfo("chrReplacement", seqlengths = 100L)
  )

  annotation_data(x) <- replacement

  embedded <- get_annotation(x, "default")
  expect_identical(annotation_data(embedded), replacement)
  expect_identical(base_annotation(embedded), replacement)
  expect_identical(seqinfo(x), GenomeInfoDb::seqinfo(replacement))
  expect_identical(plot_cache(x), list())
  expect_null(feature_index(x))

  annotation_data(x) <- NULL
  expect_null(annotation_data(x))
  expect_null(seqinfo(x))
})
```

Add this complete label test to `tests/testthat/test-synannotation.R`:

```r
test_that("set_gene_labels preserves authoritative state and clears only plot cache", {
  original <- GenomicRanges::GRanges("chr1", IRanges::IRanges(10, 20))
  S4Vectors::mcols(original)$gene_id <- "g1"
  ann <- SynFeatureAnnotation("features", "fixture.gff3")
  ann@annotation <- original
  ann@base_annotation <- original
  ann@patches <- list(SynAnnotationPatch("patch", original, target_ids = "g1"))
  ann@feature_index <- list(gene = 1L)
  ann@nucleotide_seq <- stats::setNames(Biostrings::DNAStringSet("ATG"), "g1")
  ann@protein_seq <- stats::setNames(Biostrings::AAStringSet("M"), "g1")
  ann@plot_cache <- list(stale = TRUE)
  base_before <- ann@base_annotation
  patches_before <- ann@patches
  index_before <- ann@feature_index
  nucleotide_before <- ann@nucleotide_seq
  protein_before <- ann@protein_seq

  labelled <- set_gene_labels(ann, c(g1 = "Gene one"))

  expect_identical(as.character(label_map(labelled)$label), "Gene one")
  expect_identical(
    as.character(S4Vectors::mcols(annotation_data(labelled))$plot_label),
    "Gene one"
  )
  expect_identical(base_annotation(labelled), base_before)
  expect_identical(list_patches(labelled), patches_before)
  expect_identical(feature_index(labelled), index_before)
  expect_identical(nucleotide_seq(labelled), nucleotide_before)
  expect_identical(protein_seq(labelled), protein_before)
  expect_identical(plot_cache(labelled), list())
})
```

- [ ] **Step 3: Run the replacement tests red.**

Run:

```sh
Rscript -e 'devtools::test(filter = "synannotation|synindividual", stop_on_failure = TRUE)'
```

Expected RED: stale base/patch/index/sequence/plot state survives replacement, the owner retains old `seqinfo`, or label application is broken after making the setter destructive.

- [ ] **Step 4: Implement authoritative replacement.**

Replace the `SynFeatureAnnotation` replacement method body with:

```r
x@annotation <- value
x@base_annotation <- value
x@patches <- list()
x@label_map <- NULL
x@feature_index <- NULL
x@nucleotide_seq <- NULL
x@protein_seq <- NULL
x@plot_cache <- list()
x@loaded <- !is.null(value)
validObject(x)
x
```

Replace the active-layer body of the `SynIndividual` method with:

```r
ann_name <- active_feature_annotation(x)
if (ann_name %in% names(x@annotations)) {
  ann <- x@annotations[[ann_name]]
  annotation_data(ann) <- value
  x@annotations[[ann_name]] <- ann
}
x@seqinfo <- if (is.null(value)) NULL else GenomeInfoDb::seqinfo(value)
x@plot_cache <- list()
validObject(x)
x
```

- [ ] **Step 5: Preserve the distinct label-update contract.**

After normalizing `map_df`, replace the current setter-based update inside `set_gene_labels()` with direct state updates:

```r
x@label_map <- map_df
if (!is.null(x@annotation)) {
  x@annotation <- .apply_label_mapping(x@annotation, map_df)
}
x@plot_cache <- list()
validObject(x)
x
```

This intentionally preserves `base_annotation`, patches, feature index, and both sequence slots.

- [ ] **Step 6: Remove dead local cache writes and correct slot prose.**

Delete only these writes from value-returning functions:

```r
x@region_cache[[cache_key]] <- result
x@loaded <- TRUE
```

from `query_variants()`; these writes from `query_domains()`:

```r
x@domain_data <- domain_data
x@loaded <- TRUE
```

and replace the mutation getter's local-write pattern with this complete value-returning method:

```r
setMethod("protein_mutation_data", "SynProteinMutationAnnotation", function(x) {
  mutation_data <- x@mutation_data
  if (is.null(mutation_data)) {
    mutation_data <- read_protein_mutation_counts(source_file(x))
  }
  mutation_data
})
```

Keep reads of already populated `region_cache`, `domain_data`, and `mutation_data`. Update the corresponding slot roxygen to say they are pre-populated/compatibility stores and that on-demand value-returning queries do not populate the caller-owned object.

- [ ] **Step 7: Run the state/query slices green and regenerate documentation.**

Run:

```sh
Rscript -e 'devtools::test(filter = "synannotation|synindividual|protein-mutation-annotation", stop_on_failure = TRUE)'
Rscript -e 'devtools::document()'
git diff --check
```

Expected GREEN: all focused tests pass and generated documentation reflects the value-returning semantics.

---

### Task 3: Honor Custom Tabix Indexes and Reject BCF Explicitly

**Files:**

- Modify: `tests/testthat/test-synannotation.R`
- Modify: `R/SynAnnotation-class.R:21, 288-315, 552-577, 1231-1269`
- Generate: `man/SynAnnotation-class-hierarchy.Rd`, `man/SynVCFAnnotation-class.Rd`, `man/SynVCFAnnotation.Rd`, `man/query_variants.Rd`

**Interfaces:**

- Consumes: a VCF/VCF.GZ source and optional non-default Tabix index path.
- Produces: region rows from the configured index, with BCF rejected using one stable message.

- [ ] **Step 1: Add a custom-index regression.**

Add this test to `tests/testthat/test-synannotation.R`:

```r
test_that("query_variants uses the configured non-default Tabix index", {
  root <- tempfile("ggexon vcf ")
  dir.create(root)
  source <- file.path(root, "variants.vcf")
  bgzf <- file.path(root, "variants with spaces.vcf.gz")
  custom_index <- file.path(root, "custom index location.tbi")
  writeLines(c(
    "##fileformat=VCFv4.2",
    "##contig=<ID=chr1,length=100>",
    "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO",
    "chr1\t10\t.\tA\tG\t.\tPASS\t.",
    "chr1\t20\t.\tC\tT\t.\tPASS\t."
  ), source)
  Rsamtools::bgzip(source, dest = bgzf, overwrite = TRUE)
  default_index <- Rsamtools::indexTabix(bgzf, format = "vcf")
  expect_true(file.rename(default_index, custom_index))
  expect_false(file.exists(paste0(bgzf, ".tbi")))

  layer <- SynVCFAnnotation("variants", bgzf, index_file = custom_index)
  hits <- query_variants(layer, chr = "chr1", start = 10L, end = 10L)

  expect_identical(nrow(hits), 1L)
  expect_identical(as.integer(hits$POS), 10L)
  expect_identical(as.character(hits$REF), "A")
  expect_identical(as.character(hits$ALT), "G")
})
```

- [ ] **Step 2: Add the BCF contract regression.**

```r
test_that("SynVCFAnnotation rejects unsupported BCF inputs explicitly", {
  expect_error(
    SynVCFAnnotation("variants", "sample.bcf"),
    "BCF.*not supported.*VCF"
  )
  expect_error(
    SynVCFAnnotation("variants", "sample.BCF.GZ"),
    "BCF.*not supported.*VCF"
  )
  expect_s4_class(SynVCFAnnotation("variants", "sample.vcf"), "SynVCFAnnotation")
  expect_s4_class(SynVCFAnnotation("variants", "sample.vcf.gz"), "SynVCFAnnotation")
})
```

- [ ] **Step 3: Run the focused tests red.**

Run:

```sh
Rscript -e 'devtools::test(filter = "synannotation", stop_on_failure = TRUE)'
```

Expected RED: `scanTabix()` searches for the adjacent default sidecar, and BCF construction does not reject the unsupported format.

- [ ] **Step 4: Implement one BCF predicate and error contract.**

Add:

```r
.is_bcf_file <- function(path) {
  length(path) == 1L && !is.na(path) &&
    grepl("\\.bcf(?:\\.gz)?$", path, ignore.case = TRUE, perl = TRUE)
}

.abort_unsupported_bcf <- function() {
  stop(
    "BCF parsing is not supported; provide a VCF or VCF.GZ file.",
    call. = FALSE
  )
}
```

Call the predicate at the start of `SynVCFAnnotation()` and defensively at the start of `query_variants()`.

- [ ] **Step 5: Route indexed reads through the configured TabixFile.**

Replace the indexed branch with:

```r
param <- sprintf("%s:%s-%s", chr, start, end)
tabix <- Rsamtools::TabixFile(source_file(x), index = x@index_file)
vcf_lines <- Rsamtools::scanTabix(tabix, param = param)[[1L]]
lines <- vcf_lines[!grepl("^#", vcf_lines)]
```

Leave the current plain-text/on-demand fallback intact for supported VCF sources without a usable index.

- [ ] **Step 6: Make all VCF/BCF documentation truthful and verify.**

Use this statement consistently in the hierarchy, class, constructor, and query roxygen:

> VCF querying is supported. BCF parsing is not implemented; BCF input raises an unsupported-format error.

Run:

```sh
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test(filter = "synannotation", stop_on_failure = TRUE)'
git diff --check -- R/SynAnnotation-class.R tests/testthat/test-synannotation.R man NAMESPACE
```

Expected GREEN: the custom index returns the position-10 record, both BCF suffixes raise the deliberate error, and `.vcf`/`.vcf.gz` remain constructible.

---

### Task 4: Normalize PAF and PSL to One-Based Closed Coordinates

**Files:**

- Create: `tests/testthat/test-pairwise-coordinate-contract.R`
- Modify: `tests/testthat/test-paf-detail.R`
- Modify: `tests/testthat/test-psl.R`
- Modify: `tests/testthat/test-synspecies.R`
- Modify: `R/SynSpecies-class.R:2359-2403, 2673-2785, 2872-2950, 3118-3135, 3198-3227, 3602-3637`
- Generate: `man/SynPairAlignment.Rd`, `man/pairwise_alignment_data.Rd`, `man/read_pairwise_psl.Rd`

**Interfaces:**

- Consumes: PAF/PSL source records with zero-based half-open coordinates.
- Produces: coarse and expanded rows with one-based closed `qstart/qend/tstart/tend`, while `*_raw` fields retain source coordinates.
- Produces: closed-interval filtering and overlap lengths.

- [ ] **Step 1: Add first-base, last-base, and reverse-strand import tests.**

Create `tests/testthat/test-pairwise-coordinate-contract.R` with these complete fixtures:

```r
test_that("PAF imports expose one-based closed coordinates at boundaries", {
  path <- tempfile(fileext = ".paf")
  writeLines(paste(c(
    "q", 10, 0, 1, "+", "t", 10, 0, 1, 1, 1, 60, "cg:Z:1M"
  ), collapse = "\t"), path)
  pair <- SynPairAlignment("q_to_t", "q_species", "t_species", path, "paf")

  coarse <- pairwise_alignment_data(pair)
  detail <- pairwise_alignment_data(pair, cigar = TRUE)
  boundary <- pairwise_alignment_data(pair, subset = c(q_species = "q:1-1"))
  outside <- pairwise_alignment_data(pair, subset = c(q_species = "q:2-2"))

  expect_identical(coarse[c("qstart", "qend", "tstart", "tend")],
                   data.frame(qstart = 1L, qend = 1L, tstart = 1L, tend = 1L))
  expect_identical(detail$qstart, 1L)
  expect_identical(detail$qend, 1L)
  expect_identical(detail$qstart_raw, 0L)
  expect_identical(detail$qend_raw, 1L)
  expect_identical(nrow(boundary), 1L)
  expect_identical(nrow(outside), 0L)
})

test_that("PSL coarse and expanded rows expose one-based closed coordinates", {
  path <- tempfile(fileext = ".psl")
  writeLines(c(
    paste(c(1, 0, 0, 0, 0, 0, 0, 0, "++", "q", 10, 0, 1,
            "t", 10, 0, 1, 1, "1,", "0,", "0,"), collapse = "\t"),
    paste(c(1, 0, 0, 0, 0, 0, 0, 0, "+-", "q", 10, 0, 1,
            "t", 10, 0, 1, 1, "1,", "0,", "9,"), collapse = "\t")
  ), path)

  coarse <- read_pairwise_psl(path)
  detail <- read_pairwise_psl(path, more = TRUE)

  expect_identical(coarse$qstart, c(1L, 1L))
  expect_identical(coarse$qend, c(1L, 1L))
  expect_identical(coarse$tstart, c(1L, 1L))
  expect_identical(coarse$tend, c(1L, 1L))
  expect_identical(detail$qstart, c(1L, 1L))
  expect_identical(detail$tstart, c(1L, 1L))
  expect_identical(detail$qstart_raw, c(0L, 0L))
  expect_identical(detail$tstart_raw, c(0L, 9L))
})

test_that("pre-normalized cached pairwise data is not converted again", {
  cached <- data.frame(
    qchr = "q", qlen = 10L, qstart = 1L, qend = 1L, strand = "+",
    tchr = "t", tlen = 10L, tstart = 1L, tend = 1L,
    nmatch = 1L, alen = 1L, mapq = 60L,
    stringsAsFactors = FALSE
  )
  pair <- SynPairAlignment(
    "cached",
    query_individual = "q_species",
    target_individual = "t_species",
    file = "unused.paf",
    format = "paf",
    data = cached
  )

  result <- pairwise_alignment_data(pair)
  expect_identical(result$qstart, 1L)
  expect_identical(result$qend, 1L)
  expect_identical(result$tstart, 1L)
  expect_identical(result$tend, 1L)
})
```

- [ ] **Step 2: Update existing expected public starts, leaving ends/raw values unchanged.**

In `tests/testthat/test-paf-detail.R`, change the public expectations to:

```r
expect_identical(paf$qstart, c(101L, 108L, 111L, 107L, 102L, 101L))
expect_identical(paf$qend, c(105L, 110L, 114L, 110L, 104L, 101L))
expect_identical(paf$tstart, c(501L, 506L, 511L, 501L, 505L, 509L))
expect_identical(paf$tend, c(505L, 508L, 514L, 504L, 507L, 509L))
```

In `tests/testthat/test-psl.R`, increment only expected public starts for coarse and expanded rows; keep the source-derived ends unchanged. Delete the author-machine-only test block named `"read_pairwise_psl handles the external liftover PSL fixture shape"`; the preceding portable 22-column test already covers that parser shape.

- [ ] **Step 3: Run the coordinate tests red.**

Run:

```sh
Rscript -e 'devtools::test(filter = "pairwise-coordinate-contract|paf-detail|psl|synspecies", stop_on_failure = TRUE)'
```

Expected RED: imported starts remain zero-based, a one-base boundary fails the strict overlap predicate, and overlap length omits one base.

- [ ] **Step 4: Convert public PAF starts at the import boundary.**

In `.expand_paf_matches()`, retain `q_interval`/`t_interval` for cursor and raw fields, but write:

```r
qstart = as.integer(q_interval[[1L]] + 1L)
qend = as.integer(q_interval[[2L]])
tstart = as.integer(t_interval[[1L]] + 1L)
tend = as.integer(t_interval[[2L]])
```

In `.read_pairwise_paf()`, after parsing the raw integer columns and before returning coarse rows, apply:

```r
paf$qstart <- paf$qstart + 1L
paf$tstart <- paf$tstart + 1L
```

Do this only after detailed expansion has consumed the raw cursor values, so `cigar = TRUE` is not converted twice.

- [ ] **Step 5: Convert public PSL starts at the import boundary.**

In `.expand_psl_blocks()`, write oriented interval starts plus one while retaining the raw block-start fields:

```r
qstart = as.integer(q_intervals[, 1L] + 1L)
qend = as.integer(q_intervals[, 2L])
tstart = as.integer(t_intervals[, 1L] + 1L)
tend = as.integer(t_intervals[, 2L])
```

In the coarse `read_pairwise_psl()` output, write `psl$qStart + 1L` and `psl$tStart + 1L`, retaining `qEnd` and `tEnd` unchanged.

- [ ] **Step 6: Make all interval math closed.**

Apply these exact predicates/formulas in `R/SynSpecies-class.R`:

```r
record_start <= requested_end & record_end >= requested_start
```

for `pairwise_alignment_data()` subset filters and `subset_synspecies_window()` reference/partner filters;

```r
pmax(0L, pmin(record_end, requested_end) - pmax(record_start, requested_start) + 1L)
```

for dominant-cluster overlap bases; and:

```r
gap_bp <- next_start - previous_end - 1L
```

for partner-cluster gaps. Update the existing `test-synspecies.R` overlap assertions to use `<=`/`>=` at the same boundaries.

Add this sentence to the roxygen for `SynPairAlignment`, `pairwise_alignment_data()`, and `read_pairwise_psl()`:

> Returned `qstart`, `qend`, `tstart`, and `tend` values use one-based closed genomic intervals; PAF and PSL source starts are normalized during import.

- [ ] **Step 7: Run the coordinate slice green.**

Run:

```sh
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test(filter = "pairwise-coordinate-contract|paf-detail|psl|synspecies", stop_on_failure = TRUE)'
git diff --check -- R/SynSpecies-class.R tests/testthat/test-pairwise-coordinate-contract.R tests/testthat/test-paf-detail.R tests/testthat/test-psl.R tests/testthat/test-synspecies.R man NAMESPACE
```

Expected GREEN: first and last bases are included, reverse rows normalize correctly, source raw fields remain untouched, and cached caller-supplied tables are not converted again.

---

### Task 5: Fix BLAST Metadata and Export the Documented Accessors

**Files:**

- Modify: `tests/testthat/test-homology-annotation.R`
- Modify: `tests/testthat/test-synindividual.R`
- Modify: `R/homology-annotation.R:316-352`
- Modify: `R/SynIndividual-class.R:2299-2373`
- Modify: `_pkgdown.yml:141-169`
- Generate: `NAMESPACE`, `man/syn_id.Rd`, `man/protein_seq.Rd`, `man/projected_domains.Rd`

**Interfaces:**

- Produces: `metadata$n_input_rows` before BLAST best-hit deduplication and `metadata$n_unique_query` afterward.
- Produces: exported getters `syn_id()`, `protein_seq()`, and `projected_domains()`; replacement forms stay internal.

- [ ] **Step 1: Add the end-to-end BLAST importer test.**

Add to `tests/testthat/test-homology-annotation.R`:

```r
test_that("import_blast_homology ranks hits before deduplication and records input metadata", {
  blast_path <- tempfile(fileext = ".blast6")
  writeLines(c(
    "transcript:q1.t1\tref_low\t80\t100\t0\t0\t1\t100\t1\t100\t1e-20\t100",
    "transcript:q1.t1\tref_best\t90\t100\t0\t0\t1\t100\t1\t100\t1e-30\t250",
    "gene:q2-T1\tref_two\t85\t100\t0\t0\t1\t100\t1\t100\t1e-25\t200"
  ), blast_path)

  imported <- import_blast_homology(
    blast_path,
    reference_species = "reference",
    query_species = "query",
    rank_by = c("bitscore", "evalue"),
    metadata = list(fixture = "multi-hit")
  )
  expect_identical(homology_table(imported)$query_gene, c("q1", "q2"))
  expect_identical(homology_table(imported)$reference_gene, c("ref_best", "ref_two"))
  metadata <- annotation_metadata(imported)
  expect_identical(metadata$n_input_rows, 3L)
  expect_identical(metadata$n_unique_query, 2L)
  expect_identical(metadata$rank_by, c("bitscore", "evalue"))
  expect_identical(metadata$fixture, "multi-hit")
})
```

- [ ] **Step 2: Add the consumer-facing export test.**

Add to `tests/testthat/test-synindividual.R`:

```r
test_that("documented SynIndividual accessors are exported", {
  x <- SynIndividual(id = "sample")
  public <- lapply(
    c("syn_id", "protein_seq", "projected_domains"),
    function(name) getExportedValue("ggexon", name)
  )
  names(public) <- c("syn_id", "protein_seq", "projected_domains")

  expect_identical(public$syn_id(x), "sample")
  expect_null(public$protein_seq(x))
  expect_identical(public$projected_domains(x), list())
})
```

- [ ] **Step 3: Run both slices red.**

Run:

```sh
Rscript -e 'devtools::test(filter = "homology-annotation|synindividual", stop_on_failure = TRUE)'
```

Expected RED: `n_input_rows` is `2L`, and at least one getter is absent from the namespace exports.

- [ ] **Step 4: Capture the BLAST input count before ranking and deduplication.**

Immediately after reading the table, add:

```r
blast_df <- .read_blast_tabular(blast_file, col_names = col_names)
n_input_rows <- nrow(blast_df)
```

Then store `n_input_rows = n_input_rows` in the metadata list. Do not change rank ordering or query-ID normalization.

- [ ] **Step 5: Add roxygen-owned public accessor documentation.**

Give each generic a title, description, `@param x`, `@return`, executable example, and `@export`. Use `@rdname protein_seq` on both `SynIndividual` and `SynFeatureAnnotation` methods. Export getters only. Add `syn_id` to the object/accessor group in `_pkgdown.yml`, add `protein_seq` beside `annotation_data`, and keep the existing `projected_domains` entry.

Remove the hand-authored `man/projected_domains.Rd` before `devtools::document()` so roxygen creates the single authoritative file.

- [ ] **Step 6: Generate docs and run both slices green.**

Run:

```sh
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test(filter = "homology-annotation|synindividual", stop_on_failure = TRUE)'
Rscript -e 'stopifnot(all(c("syn_id", "protein_seq", "projected_domains") %in% getNamespaceExports("ggexon")))'
```

Expected GREEN: three input BLAST rows become two best-hit mappings, and all three getters work through the exported namespace.

---

### Task 6: Make MAF Limitations and Tutorials Executable and Honest

**Files:**

- Modify: `tests/testthat/test-synspecies.R`
- Modify: `R/SynSpecies-class.R:102-131, 505-521, 3243-3363`
- Modify: `R/odgi.R:1195-1260`
- Modify: `README.md:85-107`
- Modify: `vignettes/ggexon-classes-and-verbs.Rmd:92-100, 138-210, 453-464, 654-672`
- Modify: `vignettes/ggexon-workflow.Rmd:57-88, 167-178, 204-295`
- Generate: `man/SynMultiAlignment-class.Rd`, `man/SynMultiAlignment.Rd`, `man/load_alignment.Rd`

**Interfaces:**

- Preserves: parsed MAF supplied via `data` is valid stored data.
- Produces: unloaded MAF files fail with `"MAF file parsing is not supported"`; ODGI file loading remains supported.
- Produces: vignette code backed by bundled fixtures that renders successfully; external-file recipes remain visibly labelled templates with `eval = FALSE`.

- [ ] **Step 1: Add the MAF storage/loading contract test.**

Add to `tests/testthat/test-synspecies.R`:

```r
test_that("SynMultiAlignment stores parsed MAF data but rejects MAF file loading", {
  parsed <- data.frame(block = 1L, sequence = c("AC", "AT"))
  stored <- SynMultiAlignment(
    "maf",
    individuals = c("a", "b"),
    file = "input.maf",
    format = "maf",
    data = parsed
  )
  expect_identical(multiple_alignment_data(stored), parsed)

  unloaded <- SynMultiAlignment(
    "maf",
    individuals = c("a", "b"),
    file = "input.maf",
    format = "maf"
  )
  expect_error(multiple_alignment_data(unloaded), "MAF file parsing is not supported")
  expect_error(load_alignment(unloaded), "MAF file parsing is not supported")
})
```

- [ ] **Step 2: Run the MAF test red.**

Run:

```sh
Rscript -e 'devtools::test(filter = "synspecies", stop_on_failure = TRUE)'
```

Expected RED: the current unloaded-MAF error mentions only ODGI and does not expose the stable MAF limitation contract.

- [ ] **Step 3: Implement the specific MAF error and align all prose.**

In both the unloaded `multiple_alignment_data("SynMultiAlignment")` method and the unloaded `load_alignment("SynMultiAlignment")` method, use:

```r
if (identical(alignment_format(x), "maf")) {
  stop(
    "MAF file parsing is not supported; supply already parsed MAF data through `data`.",
    call. = FALSE
  )
}
```

Use this sentence consistently in source roxygen, README, and vignettes:

> `SynMultiAlignment` can store already parsed MAF data supplied through `data`, but `load_alignment()` cannot parse a MAF file. ODGI-backed file loading is supported.

Change every MAF constructor example to a storage-only template that includes `format = "maf"` and `data = pre_parsed_maf`.

In the README class table and both vignettes' annotation sections, replace any combined VCF/BCF support claim with the exact Task 3 boundary:

> VCF querying is supported. BCF parsing is not implemented; BCF input raises an unsupported-format error.

- [ ] **Step 4: Correct the BLAST loop and label it as an external-file template.**

Keep the chunk at `eval = FALSE`, add the visible heading `Template requiring your BLAST files`, and use:

```r
query_species_names <- c("C_bovis", "C_afra")
for (sp_name in query_species_names) {
  ha <- import_blast_homology(
    blast_file = paste0(sp_name, ".blast6"),
    reference_species = "C. elegans",
    query_species = sp_name,
    gene_id_map = "c_elegans.PRJNA13758.WS285.geneIDs.txt"
  )
  sp <- add_homology_annotation(sp, ha)
}
```

This avoids collision with the `query_species()` generic; Task 5's importer test is its executable validation.

- [ ] **Step 5: Replace the broken lollipop example with executable data.**

Use this exact chunk with execution enabled:

```r
mutations <- data.frame(
  position = c(5, 8, 20),
  mutation = c("M5T", "K8R", "A20V")
)
domains <- data.frame(
  start = c(1, 15),
  end = c(10, 25),
  Domain = c("C2H2", "Coil")
)

ggplot2::ggplot() +
  geom_protein_lollipop(
    mutations = mutations,
    domains = domains,
    protein_length = 30
  )
```

- [ ] **Step 6: Make the bundled two-species workflow self-contained.**

Before the two-species plot in `vignettes/ggexon-workflow.Rmd`, add the N2 object and pairwise alignment:

```r
n2 <- SynIndividual(
  genome_file = system.file(
    "extdata", "compact_synspecies",
    "c_elegans.PRJNA13758.WS285.genomic.fa",
    package = "ggexon"
  ),
  annotation_file = system.file(
    "extdata", "compact_synspecies",
    "c_elegans.PRJNA13758.WS285.canonical_geneset.gtf",
    package = "ggexon"
  ),
  annotation_format = "gtf",
  id = "N2"
)
sp <- add_individual(sp, n2)
sp <- add_pairwise_alignment(
  sp,
  SynPairAlignment(
    "XZ1516_vs_N2",
    query_individual = "XZ1516",
    target_individual = "N2",
    file = system.file(
      "extdata", "compact_synspecies", "V_alginment.paf",
      package = "ggexon"
    )
  )
)
```

Add `library(ggexon)` once in each vignette's hidden setup chunk. Give the executable chunks these labels and settings:

```text
ggexon-classes-and-verbs.Rmd: protein-lollipop-example, eval = TRUE
ggexon-workflow.Rmd: workflow-one-individual, eval = TRUE
ggexon-workflow.Rmd: workflow-genetag, eval = TRUE
ggexon-workflow.Rmd: workflow-pairwise-setup, eval = TRUE
ggexon-workflow.Rmd: workflow-pairwise-plot, eval = TRUE
```

The one-individual chunk is the existing block that creates `x`, creates `sp`, adds `x`, and draws `geom_exon()`. The `workflow-genetag` chunk is the immediately following `geom_genetag()` plot. Split the new N2/alignment setup from the two-species plot so both have stable labels. Leave every chunk containing `other_genome_file`, a user BLAST filename, `geneIDs.txt`, or `worms.maf` at `eval = FALSE`, and add the visible prefix `Template requiring user-provided files:` before its explanatory paragraph.

- [ ] **Step 7: Remove `add_genetag()` from public prose and render both guides.**

Remove `add_genetag()` from the graph-building function list; `geom_genetag()` remains public plotting API.

Run:

```sh
Rscript -e 'devtools::document()'
Rscript -e 'rmarkdown::render("vignettes/ggexon-classes-and-verbs.Rmd", output_dir = tempdir(), quiet = TRUE, clean = TRUE)'
Rscript -e 'rmarkdown::render("vignettes/ggexon-workflow.Rmd", output_dir = tempdir(), quiet = TRUE, clean = TRUE)'
Rscript -e 'devtools::test(filter = "synspecies|homology-annotation|protein-lollipop", stop_on_failure = TRUE)'
```

Expected GREEN: both HTML guides render, the pairwise example resolves both species, the lollipop call matches the real API, and MAF storage/loading behavior is explicit.

---

### Task 7: Correct Package Metadata, Dependency Attachment, and Manual Portability

**Files:**

- Create: `tests/testthat/test-package-contract.R`
- Modify: `DESCRIPTION`
- Inspect and modify only if `R CMD check` identifies a missing narrow import: `R/ggexon-package.R`
- Modify: `R/geom_genelabel.R:639`
- Generate: `NAMESPACE`, `man/geom_genelabel.Rd`, `man/ggexon-package.Rd`

**Interfaces:**

- Produces: `Depends: R (>= 4.1)` only; runtime packages live in `Imports`.
- Produces: `ggplot2 (>= 4.0.0)`; the final installed-package gate proves a clean `library(ggexon)` search path.
- Produces: portable Rd markup with no raw approximation sign.

- [ ] **Step 1: Add metadata and clean-session attachment tests.**

Create `tests/testthat/test-package-contract.R`:

```r
test_that("package version floors and dependency fields match the public contract", {
  description_path <- system.file("DESCRIPTION", package = "ggexon")
  expect_true(nzchar(description_path))
  description <- read.dcf(description_path)[1L, ]
  depends <- trimws(strsplit(description[["Depends"]], ",")[[1L]])
  imports <- trimws(strsplit(description[["Imports"]], ",")[[1L]])

  expect_identical(depends, "R (>= 4.1)")
  expect_true("ggplot2 (>= 4.0.0)" %in% imports)
  expect_true(all(c(
    "methods", "gtable", "ggforce", "dplyr", "rtracklayer", "grid",
    "tidyr", "scales", "reshape2", "rlang", "vctrs", "S7"
  ) %in% imports))
})
```

Do not launch `library(ggexon)` from this source-tree unit test: a fresh subprocess would load whichever older ggexon happens to be installed. Task 10 installs the exact built archive into a temporary library and performs the clean-search-path assertion there.

- [ ] **Step 2: Run the package-contract test red and confirm the raw Unicode source.**

Run:

```sh
Rscript -e 'devtools::test(filter = "package-contract", stop_on_failure = TRUE)'
rg -n '≈' R man
```

Expected RED: the dependency field assertions fail; `rg` reports `R/geom_genelabel.R` and generated `man/geom_genelabel.Rd`.

- [ ] **Step 3: Rewrite `DESCRIPTION` metadata.**

Set:

```text
Title: Harmonizing Visualization of Genomic Synteny Across Genomes
Description: Provides a grammar of graphics for visualizing genomic annotations,
    synteny, alignments, protein features, mutation summaries, and associated
    track data across genomes.
Depends:
    R (>= 4.1)
Imports:
    BiocGenerics,
    Biostrings,
    cli,
    dplyr,
    farver,
    fuzzyjoin,
    GenomeInfoDb,
    GenomicRanges,
    ggforce,
    ggplot2 (>= 4.0.0),
    glue,
    grid,
    gtable,
    IRanges,
    lifecycle,
    methods,
    readr,
    reshape2,
    rlang,
    Rsamtools,
    rtracklayer,
    S4Vectors,
    S7,
    scales,
    stats,
    stringr,
    tibble,
    tidyr,
    utils,
    vctrs,
    withr
```

Remove the invalid ORCID comment because no valid repository source exists; do not guess a replacement. Retain the current author, email, URL, license, encoding, and system requirements.

- [ ] **Step 4: Replace raw Unicode in the roxygen source and keep imports narrow.**

Change the parameter sentence to:

```r
#'   Used to convert text size into data-coordinate units for label placement
#'   and collision avoidance. Default `300` (\eqn{\approx} A4/US-letter panel width).
```

Run `devtools::document()`. If `R CMD check` later reports an unqualified missing symbol, add a single `@importFrom package symbol` declaration to `R/ggexon-package.R`; never restore a package to `Depends` to hide an import defect.

- [ ] **Step 5: Verify metadata, namespace, and the PDF manual.**

Run:

```sh
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test(filter = "package-contract", stop_on_failure = TRUE)'
rg -n '≈' R man
audit_manual_dir=$(mktemp -d)
R CMD Rd2pdf --no-preview --force --output="$audit_manual_dir/ggexon.pdf" .
```

Expected GREEN: package-contract tests pass, `rg` has no matches, and the PDF manual command exits zero.

---

### Task 8: Make Provenance Portable and Remove Repository/Test Hygiene Defects

**Files:**

- Create: `tests/testthat/test-provenance-portability.R`
- Modify: `tests/testthat/test-cd44-demo-data.R`
- Modify: `tests/testthat/test-peel1-coverage-data.R`
- Modify: `tests/testthat/test-synspecies-plotting.R`
- Modify: `data-raw/cd44_pairwise_ensembl116/build-cd44-demo.R`
- Modify: `data-raw/peel1_coverage/build-peel1-coverage-fixtures.R`
- Modify: `inst/extdata/cd44_pairwise_ensembl116/cd44_provenance.tsv`
- Modify: `inst/extdata/cd44_pairwise_ensembl116/README.md`
- Modify: `inst/extdata/peel1_coverage/manifest.tsv`
- Modify: `inst/extdata/peel1_coverage/README.md`
- Modify: `.Rbuildignore`
- Modify: `.gitignore`
- Modify: `R/ggplot-functions.R`
- Modify: `R/grob-null.R`
- Delete: `.Rhistory`, `docs/.DS_Store`, `man/.DS_Store`, `tests/ldy_test.R`, `R/geom-direction.R`
- Modify: `DESCRIPTION` Collate entry for `R/geom-direction.R`

**Interfaces:**

- Produces: shipped provenance containing URLs, portable basenames/tool versions, MD5 values, and portable reproduction commands, but no maintainer-local absolute path.
- Produces: warning-free aesthetic mapping tests.
- Produces: source archives without ignored local assets or tracked editor/session debris.

- [ ] **Step 1: Add a shared shipped-provenance portability test.**

Create `tests/testthat/test-provenance-portability.R`:

```r
test_that("shipped manifests and provenance contain no local absolute paths", {
  root <- system.file("extdata", package = "ggexon")
  expect_true(nzchar(root))
  files <- list.files(
    root,
    pattern = "(manifest|provenance).*\\.tsv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  expect_gt(length(files), 0L)

  for (path in files) {
    text <- paste(readLines(path, warn = FALSE), collapse = "\n")
    posix_absolute <- grepl(
      "(^|[[:space:]\\\"'=])/(?!/)", text, perl = TRUE
    )
    windows_absolute <- grepl(
      "(^|[[:space:]\\\"'=])[A-Za-z]:[/\\\\]", text, perl = TRUE
    )
    expect_false(posix_absolute, info = path)
    expect_false(windows_absolute, info = path)
  }
})
```

- [ ] **Step 2: Strengthen dataset-specific provenance assertions and run red.**

In the CD44 test, require keys `lastz_binary`, `lastz_version`, `lastz_command`, `rscript`, `human_fasta_md5`, `mouse_fasta_md5`, and `paf_md5`; require MD5 values to match `^[0-9a-f]{32}$`; require `lastz_binary == basename(lastz_binary)`.

In the PEEL1 test, require every `source_bam` to equal `basename(source_bam)` while keeping the existing BigWig checksum and bamCoverage version assertions.

Run:

```sh
Rscript -e 'devtools::test(filter = "provenance-portability|cd44-demo-data|peel1-coverage-data", stop_on_failure = TRUE)'
```

Expected RED: CD44 contains `/opt`, `/Users`, and `/Library`; PEEL1 contains `/Users` paths and lacks basename-only provenance.

- [ ] **Step 3: Make both fixture builders emit portable provenance.**

In the PEEL1 builder, replace `source_bam = normalizePath(bam)` with:

```r
source_bam = basename(bam)
```

In `run_lastz()`, construct both the executable arguments and a separate portable provenance command:

```r
portable_args <- c(
  basename(target_fa),
  basename(query_fa),
  "--strand=both --chain --format=paf:minimap2",
  paste0("--output=", basename(paf_path))
)
portable_command <- paste(c(basename(lastz_bin), portable_args), collapse = " ")
```

Return `command = portable_command` from both `run_lastz()` branches. Before building the provenance table, define and store the exact values:

```r
human_fasta <- file.path(
  out_dir,
  windows$fasta_file[windows$species == "human"]
)
mouse_fasta <- file.path(
  out_dir,
  windows$fasta_file[windows$species == "mouse"]
)
lastz_binary <- basename(lastz_bin)
lastz_version <- paste(system2(lastz_bin, "--version", stdout = TRUE), collapse = " ")
lastz_command <- alignment$command
rscript <- paste("R", getRversion())
human_fasta_md5 <- unname(tools::md5sum(human_fasta))
mouse_fasta_md5 <- unname(tools::md5sum(mouse_fasta))
paf_md5 <- unname(tools::md5sum(alignment$paf_path))
```

Apply the same representation to the shipped TSV files. Use the existing fixture hashes:

```text
human_fasta_md5  831e0af2005f105ee109d8a44850ac26
mouse_fasta_md5  ac17f56ce45b257a7030b453078b43cc
paf_md5          a8e51d0a5dc44d03dc6f64e101ec6527
```

Retain `lastz` version `1.04.52` and represent R as `R 4.4.1` in the shipped snapshot. Update the two README files so they call these fields basenames and identify the exact checksum targets.

- [ ] **Step 4: Turn discrete-alpha warnings into meaningful numeric mapping tests.**

Wrap the three relevant `ggexon_build()` calls in `expect_no_warning()`. Replace each `alpha = strand` mapping with:

```r
alpha = ifelse(strand == "+", 0.4, 0.8)
```

and add `ggplot2::scale_alpha_identity()` to the corresponding plot. Strengthen the assertions to:

```r
expect_true(all(exon_data$alpha %in% c(0.4, 0.8)))
expect_true(all(gene_data$alpha %in% c(0.4, 0.8)))
expect_true(all(label_data$alpha %in% c(0.4, 0.8)))
```

Run:

```sh
Rscript -e 'devtools::test(filter = "synspecies-plotting", stop_on_failure = TRUE)'
```

Expected GREEN: the mapped and fixed alpha paths remain tested with no discrete-alpha warning.

- [ ] **Step 5: Exclude local-only extdata assets from source archives.**

Add these anchored entries to `.Rbuildignore`:

```text
(^|/)\.DS_Store$
^inst/extdata/V_alginment\.paf$
^inst/extdata/XZ1516\.fasta(\.fai)?$
^inst/extdata/XZ1516_chrV_21560000_21620000\.(node_table\.tsv|og)$
^inst/extdata/c_elegans\.PRJNA13758\.WS285\.canonical_geneset\.gtf$
^inst/extdata/c_elegans\.PRJNA13758\.WS285\.genomic\.fa(\.fai)?$
^inst/extdata/caenorhabditis_XZ1516\.gff3$
```

These patterns target only top-level local copies; they do not remove the tracked `inst/extdata/compact_synspecies/` fixtures.

- [ ] **Step 6: Remove only verified tracked debris and one empty source file.**

First verify scope:

```sh
git grep -n -e 'geom-direction' -e 'ldy_test' -e 'create_layout' -e 'is\.zero' -- ':!docs/**'
git log --oneline -- .Rhistory docs/.DS_Store man/.DS_Store tests/ldy_test.R R/geom-direction.R R/ggplot-functions.R R/grob-null.R
wc -l R/geom-direction.R
```

Delete `.Rhistory`, both tracked `.DS_Store` files, `tests/ldy_test.R`, and the one-line empty `R/geom-direction.R`; remove only `'geom-direction.R'` from `DESCRIPTION`'s `Collate`. Remove the uncalled `create_layout()` body from `R/ggplot-functions.R` while retaining its roxygen import declarations, and remove the uncalled `is.zero()` line from `R/grob-null.R` while retaining the public `zeroGrob` re-export. Add `.Rhistory` to `.gitignore`. Do not remove `R/geom-coverageMUT.R`, which is referenced by `_pkgdown.yml`.

- [ ] **Step 7: Verify provenance, hygiene, and archive exclusions green.**

Run:

```sh
Rscript -e 'devtools::test(filter = "provenance-portability|cd44-demo-data|peel1-coverage-data|synspecies-plotting", stop_on_failure = TRUE)'
git ls-files -ci --exclude-standard
audit_build_dir=$(mktemp -d)
(cd "$audit_build_dir" && R CMD build --no-build-vignettes --no-manual /Users/liudongyao/Downloads/repository/ggexon)
if tar -tzf "$audit_build_dir/ggexon_0.1.tar.gz" | rg '(^|/)(\.Rhistory|\.DS_Store|ldy_test\.R|V_alginment\.paf|XZ1516_chrV_21560000_21620000|notch2nl_primate|smn_splice_candidate)'; then exit 1; fi
```

Expected GREEN: focused tests pass, `git ls-files -ci` prints nothing, and the archive exclusion probe exits zero.

---

### Task 9: Separate R-CMD-Check Validation from Pkgdown Publication

**Files:**

- Create: `.github/workflows/R-CMD-check.yaml`
- Modify: `.github/workflows/pkgdown.yaml`

**Interfaces:**

- Produces: read-only push/PR/workflow-dispatch package checks with PDF manual generation.
- Produces: a read-only pkgdown build artifact and a single write-enabled publish job.

- [ ] **Step 1: Record the current workflow-policy failure.**

Run:

```sh
rg -n 'permissions:|contents: write|git push|R-CMD-check' .github/workflows
```

Expected RED: there is no R-CMD-check workflow and the entire current pkgdown job has repository write permission.

- [ ] **Step 2: Add the package-check workflow with immutable action SHAs.**

Create `.github/workflows/R-CMD-check.yaml` with this complete content:

```yaml
name: R-CMD-check

on:
  push:
    branches: [main]
  pull_request:
  workflow_dispatch:

permissions:
  contents: read

jobs:
  R-CMD-check:
    runs-on: ${{ matrix.config.os }}
    strategy:
      fail-fast: false
      matrix:
        config:
          - {os: macos-latest, r: release}
          - {os: windows-latest, r: release}
          - {os: ubuntu-latest, r: devel, http-user-agent: release}
          - {os: ubuntu-latest, r: release}
          - {os: ubuntu-latest, r: oldrel-1}
    steps:
      - uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1
        with:
          persist-credentials: false
      - uses: r-lib/actions/setup-pandoc@6f6e5bc62fba3a704f74e7ad7ef7676c5c6a2590
      - uses: r-lib/actions/setup-r@6f6e5bc62fba3a704f74e7ad7ef7676c5c6a2590
        with:
          r-version: ${{ matrix.config.r }}
          http-user-agent: ${{ matrix.config.http-user-agent }}
          use-public-rspm: true
      - uses: r-lib/actions/setup-tinytex@6f6e5bc62fba3a704f74e7ad7ef7676c5c6a2590
      - uses: r-lib/actions/setup-r-dependencies@6f6e5bc62fba3a704f74e7ad7ef7676c5c6a2590
        with:
          extra-packages: any::rcmdcheck
          needs: check
      - uses: r-lib/actions/check-r-package@6f6e5bc62fba3a704f74e7ad7ef7676c5c6a2590
        with:
          args: 'c("--as-cran")'
          build_args: 'character()'
          error-on: '"warning"'
```

- [ ] **Step 3: Replace pkgdown with isolated build and publish jobs.**

Replace `.github/workflows/pkgdown.yaml` with this complete workflow:

```yaml
name: pkgdown

on:
  push:
    branches: [main]
    paths-ignore:
      - "docs/**"
  workflow_dispatch:

permissions:
  contents: read

concurrency:
  group: pkgdown-${{ github.ref }}
  cancel-in-progress: true

jobs:
  build:
    runs-on: ubuntu-latest
    permissions:
      contents: read
    steps:
      - uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1
        with:
          persist-credentials: false
      - uses: r-lib/actions/setup-pandoc@6f6e5bc62fba3a704f74e7ad7ef7676c5c6a2590
      - uses: r-lib/actions/setup-r@6f6e5bc62fba3a704f74e7ad7ef7676c5c6a2590
        with:
          use-public-rspm: true
      - uses: r-lib/actions/setup-r-dependencies@6f6e5bc62fba3a704f74e7ad7ef7676c5c6a2590
        with:
          extra-packages: any::pkgdown, local::.
          needs: website
      - name: Remove checked-out site in the runner
        run: rm -rf docs
      - name: Build pkgdown site
        run: pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
        shell: Rscript {0}
      - uses: actions/upload-artifact@043fb46d1a93c77aae656e7c1c64a875d1fc6a0a
        with:
          name: pkgdown-site
          path: docs
          if-no-files-found: error
          include-hidden-files: true

  publish:
    needs: build
    runs-on: ubuntu-latest
    permissions:
      contents: write
    steps:
      - uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1
      - run: git rm -r --ignore-unmatch docs
      - uses: actions/download-artifact@3e5f45b2cfb9172054b4087a40e8e0b5a5461e7c
        with:
          name: pkgdown-site
          path: docs
      - name: Publish pkgdown site
        run: |
          git config user.name "github-actions[bot]"
          git config user.email "41898282+github-actions[bot]@users.noreply.github.com"
          git add -A docs
          if git diff --cached --quiet; then
            echo "No pkgdown site changes to commit."
          else
            git commit -m "Update pkgdown site"
            git push
          fi
```

The build job contains no `GITHUB_PAT`, `contents: write`, Git configuration, commit, or push. The only destructive command targets the disposable runner's checked-out `docs` directory; the publish job recreates that exact directory from the immutable artifact before committing.

- [ ] **Step 4: Validate workflow policy and syntax.**

Run:

```sh
test "$(rg -n '^\s+contents: write$' .github/workflows/pkgdown.yaml | wc -l | tr -d ' ')" = "1"
test "$(rg -n 'git push' .github/workflows/pkgdown.yaml | wc -l | tr -d ' ')" = "1"
rg -n 'contents: read|persist-credentials: false|check-r-package|setup-tinytex' .github/workflows
if command -v actionlint >/dev/null 2>&1; then actionlint .github/workflows/*.yaml; fi
```

Expected GREEN: exactly one pkgdown write permission and one push exist, both in `publish`; R-CMD-check is read-only and retains the manual build.

---

### Task 10: Regenerate, Review, and Run the Complete Release Gate

**Files:**

- Generate: `NAMESPACE`, affected `man/*.Rd`, and `docs/`
- Inspect: every changed file and the built source archive

**Interfaces:**

- Consumes: all preceding task outputs.
- Produces: one verified, uncommitted working tree that satisfies the approved Approach C scope.

- [ ] **Step 1: Run focused suites once in dependency order.**

Run:

```sh
Rscript -e 'devtools::test(filter = "odgi|synannotation|synindividual|protein-mutation-annotation", stop_on_failure = TRUE)'
Rscript -e 'devtools::test(filter = "pairwise-coordinate-contract|paf-detail|psl|synspecies", stop_on_failure = TRUE)'
Rscript -e 'devtools::test(filter = "homology-annotation|protein-lollipop|package-contract", stop_on_failure = TRUE)'
Rscript -e 'devtools::test(filter = "provenance-portability|cd44-demo-data|peel1-coverage-data|synspecies-plotting", stop_on_failure = TRUE)'
```

- [ ] **Step 2: Regenerate documentation and prove idempotence.**

Run:

```sh
Rscript -e 'devtools::document()'
git diff --check
audit_doc_hash_before=$(mktemp)
find NAMESPACE man -type f -print | LC_ALL=C sort | xargs shasum -a 256 > "$audit_doc_hash_before"
Rscript -e 'devtools::document()'
audit_doc_hash_after=$(mktemp)
find NAMESPACE man -type f -print | LC_ALL=C sort | xargs shasum -a 256 > "$audit_doc_hash_after"
cmp "$audit_doc_hash_before" "$audit_doc_hash_after"
if rg -n '≈' R man; then exit 1; fi
if rg -n 'VCF/BCF.*supported|MAF.*file.*supported' R man README.md vignettes; then exit 1; fi
if rg -n 'add_genetag\(' README.md vignettes; then exit 1; fi
```

Expected GREEN: the two generated hash manifests are identical, and all three `rg` commands have no matches.

- [ ] **Step 3: Render the corrected guides and run the complete test suite.**

Run:

```sh
Rscript -e 'rmarkdown::render("vignettes/ggexon-classes-and-verbs.Rmd", output_dir = tempdir(), quiet = TRUE, clean = TRUE)'
Rscript -e 'rmarkdown::render("vignettes/ggexon-workflow.Rmd", output_dir = tempdir(), quiet = TRUE, clean = TRUE)'
Rscript -e 'devtools::test(reporter = "summary", stop_on_failure = TRUE)'
```

Expected GREEN: both vignettes render and the full suite completes with zero failures, errors, and unexpected warnings.

- [ ] **Step 4: Create an isolated source copy containing only tracked files plus owned changes.**

From the repository root, create an explicit file manifest from every still-existing tracked path, then append only the new files owned by this remediation:

```sh
audit_verify_root=$(mktemp -d)
audit_source_copy="$audit_verify_root/ggexon"
audit_file_list="$audit_verify_root/source-files.txt"
mkdir -p "$audit_source_copy"
git ls-files | while IFS= read -r audit_source_path; do
  if test -f "$audit_source_path"; then
    printf '%s\n' "$audit_source_path"
  fi
done > "$audit_file_list"
for audit_owned_path in \
  .github/workflows/R-CMD-check.yaml \
  tests/testthat/test-pairwise-coordinate-contract.R \
  tests/testthat/test-package-contract.R \
  tests/testthat/test-provenance-portability.R \
  man/syn_id.Rd \
  man/protein_seq.Rd \
  docs/superpowers/specs/2026-09-14-repository-audit-remediation-design.md \
  docs/superpowers/plans/2026-09-14-repository-audit-remediation.md
do
  if test -f "$audit_owned_path"; then
    printf '%s\n' "$audit_owned_path" >> "$audit_file_list"
  fi
done
sort -u "$audit_file_list" -o "$audit_file_list"
rsync -a --files-from="$audit_file_list" ./ "$audit_source_copy/"
export AUDIT_SOURCE_COPY="$audit_source_copy"
git status --short
```

Inspect `git status --short` and confirm that each path outside the two preserved research directories belongs to Tasks 1-9. Do not add other untracked paths to the manifest.

Run the source-tree sanity checks in the copy:

```sh
test -z "$(find "$audit_source_copy" \( -name '.DS_Store' -o -name '.Rhistory' \) -print)"
Rscript -e 'devtools::test(Sys.getenv("AUDIT_SOURCE_COPY"), filter = "provenance-portability", stop_on_failure = TRUE)'
```

Expected GREEN: neither command reports a match.

- [ ] **Step 5: Build and check the package archive.**

Use a separate temporary output directory and absolute archive path:

```sh
audit_build_dir="$audit_verify_root/build"
mkdir -p "$audit_build_dir"
(cd "$audit_build_dir" && R CMD build "$audit_source_copy")
(cd "$audit_build_dir" && R CMD check --as-cran ggexon_0.1.tar.gz)
```

Expected GREEN: build succeeds; check reports `0 errors | 0 warnings | 0 notes`. If CRAN incoming feasibility performs a network check that fails while all package-owned checks are green, record that section and rerun `(cd "$audit_build_dir" && env _R_CHECK_CRAN_INCOMING_REMOTE_=false R CMD check --as-cran ggexon_0.1.tar.gz)` to separate network availability from package state.

- [ ] **Step 6: Build pkgdown from the isolated copy.**

Run:

```sh
Rscript -e 'pkgdown::build_site(pkg = Sys.getenv("AUDIT_SOURCE_COPY"), new_process = FALSE, install = TRUE)'
```

Set `AUDIT_SOURCE_COPY` to the absolute temporary-copy path for this command. Expected GREEN: site generation completes without errors. This gate is mandatory before any later commit request.

- [ ] **Step 7: Audit the exact archive and installed search path.**

Run:

```sh
if tar -tzf "$audit_build_dir/ggexon_0.1.tar.gz" | rg '(^|/)(\.Rhistory|\.DS_Store|ldy_test\.R|notch2nl_primate|smn_splice_candidate|XZ1516_chrV_21560000_21620000)'; then exit 1; fi
audit_install_lib=$(mktemp -d)
R CMD INSTALL --library="$audit_install_lib" "$audit_build_dir/ggexon_0.1.tar.gz"
Rscript --vanilla -e '.libPaths(c(commandArgs(TRUE)[1], .libPaths())); library(ggexon); forbidden <- paste0("package:", c("gtable","ggforce","dplyr","rtracklayer","grid","tidyr","scales","reshape2","rlang","vctrs","S7")); stopifnot(!any(forbidden %in% search()))' "$audit_install_lib"
```

Expected GREEN: archive scan has no matches and the clean installed-package session does not attach runtime Imports.

- [ ] **Step 8: Request an independent code review, resolve findings, and rerun affected gates.**

Use `superpowers:requesting-code-review` over the complete owned diff. Verify every finding against the current code before editing. Rerun the focused test for each accepted correction, then repeat Steps 2, 3, 5, 6, and 7.

- [ ] **Step 9: Deliver the uncommitted result.**

Report changed behavior, deferred BCF/MAF parsers, exact verification results, environmental exceptions, and the preserved untracked directories. Include `git status --short` and state explicitly that no commit was created.
