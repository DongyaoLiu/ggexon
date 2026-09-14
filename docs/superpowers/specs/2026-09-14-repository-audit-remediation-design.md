# Repository Audit Remediation Design

## Status and scope

This design implements the approved staged approach (Approach C): repair the
confirmed security, correctness, package, documentation, and CI defects while
leaving full BCF and MAF parsing as explicitly documented future work.

The work applies to the complete tracked `HEAD` of ggexon. Existing untracked
research files and outputs are outside scope and must remain untouched. No
commit will be created unless the maintainer asks for one.

## Compatibility decisions

- Public genomic coordinates will use R/Bioconductor conventions: one-based,
  closed intervals. PAF and PSL starts will be converted from zero-based,
  half-open source coordinates during import; source ends already equal the
  corresponding one-based closed ends and will not be incremented.
- VCF querying remains supported, including a non-default Tabix index path.
  BCF parsing will not be implemented in this pass. Documentation will state
  that BCF is future work, and BCF input will fail early with a specific error
  instead of being read as text.
- `SynMultiAlignment` may continue to hold pre-parsed MAF data, but loading a
  MAF file is future work. Class, constructor, README, and vignette text will
  distinguish storage from file parsing.
- Accessors intentionally taught in public documentation (`syn_id()`,
  `protein_seq()`, and `projected_domains()`) will become documented exports.
  The legacy `add_genetag()` helper will remain internal and be removed from
  public prose.
- Existing public plotting behavior remains in scope. The pass will declare
  the actual R and ggplot2 minimum versions and reduce accidental search-path
  attachment, but it will not attempt a wholesale ggplot2 compatibility
  rewrite.

## Remediation units

### 1. Safe external-process execution

The ODGI Python launcher will preserve every path/option as one process
argument. Tests will first demonstrate failure with spaces and shell
metacharacters, then verify that no appended shell fragment executes. Python
and ODGI executable resolution and current error reporting will remain intact.

### 2. Annotation state and query correctness

Feature-annotation replacement will establish the replacement value as the new
base annotation, clear all stored patches and label mappings, and invalidate
every derived value whose row identity or sequence content depends on the old
annotation: feature index, nucleotide sequence, protein sequence, and plot cache.
The label-setting path will update the active annotation without invoking these
authoritative-replacement semantics, so an explicitly installed label map is
preserved. Replacement
through a `SynIndividual` will also clear the individual's plot cache and
refresh its sequence metadata from the replacement annotation.

Data-returning query functions will not pretend to mutate caller-owned S4
objects. Dead local cache writes will be removed or routed through an explicit
object-returning load path. Slot documentation will accurately identify
compatibility/on-object caches versus on-demand query results.

VCF indexed querying will construct a `Rsamtools::TabixFile` with the supplied
index path. BCF input will produce a deliberate unsupported-format error.

### 3. Alignment coordinate contract

PAF and PSL coarse records and expanded block/CIGAR records will expose
one-based closed intervals consistently. All overlap and overlap-length
calculations will use closed-interval predicates and `+ 1` lengths where
appropriate. Tests will cover the first genomic base, the last aligned base,
reverse-strand records, coarse records, expanded PAF matches, and expanded PSL
blocks. Bundled fixtures that are already normalized will not be converted a
second time.

### 4. Package contract and release checks

`DESCRIPTION` will declare `R (>= 4.1)` and `ggplot2 (>= 4.0.0)`. Runtime
packages currently placed in `Depends` will move to `Imports` unless attachment
is demonstrably required. Namespace documentation will be made consistent with
the resulting policy.

The raw Unicode approximation sign that breaks the PDF manual will be replaced
with portable documentation markup. Package metadata spelling/grammar and the
invalid ORCID will be corrected only from verifiable repository information;
if no valid ORCID can be established locally, the invalid identifier will be
removed rather than guessed.

A pull-request/push R-CMD-check workflow will be added separately from the
pkgdown deployment workflow. Pkgdown will use a read-only build job that uploads
the generated site and a separate publishing job whose sole responsibility is
to download and push that site with `contents: write` permission.

### 5. Public documentation and executable examples

Broken tutorial snippets will be corrected. Sections backed by bundled small
fixtures will become executable; examples that genuinely require user-owned
external files will remain non-executed and be labelled as templates rather
than validation evidence. At minimum, the previously broken homology loop,
protein-lollipop call, and two-species workflow will receive executable or
directly tested equivalents.

The BLAST homology importer will get an end-to-end fixture test covering
multi-hit ranking/deduplication and metadata. `n_input_rows` will be captured
before deduplication.

Public API/reference configuration, README claims, generated Rd, and vignette
language will be synchronized. BCF and file-backed MAF parsing will be listed
as deferred limitations.

### 6. Fixture and repository hygiene

Bundled provenance will contain portable tool names, versions, checksums, and
relative/basename paths rather than maintainer-local absolute paths. A shared
test will reject `/Users`, `/opt`, `/Library`, Windows drive roots, and other
absolute source paths across shipped manifest/provenance files.

The author-machine-only PSL test will use a minimal sanitized fixture or be
removed if fully redundant. Discrete-alpha test warnings will be rewritten to
exercise the intended mapping without warning. Local-only large files currently
under `inst/extdata` will be explicitly excluded from `R CMD build` through
`.Rbuildignore`.

Tracked `.Rhistory`, `.DS_Store`, and obsolete scratch/dead artifacts will be
removed only when they have no package caller and are recoverable from Git.
Matching ignore rules will prevent recurrence.

## Testing strategy

Every behavioral repair follows red-green-refactor:

1. Add a focused regression test and run it to observe the expected failure.
2. Apply the minimal implementation change.
3. Run the focused test to green before integrating the next repair.

Final verification will run, from a clean tracked-source copy so local ignored
data cannot contaminate results:

- the complete `testthat` suite with zero failures and zero unexpected warnings;
- roxygen regeneration followed by a generated-file diff check;
- `R CMD build`;
- `R CMD check --as-cran`;
- `pkgdown::build_site()`;
- archive inspection for absolute paths and unintended large local files.

Network-, CRAN-, or host-tool limitations will be reported separately from
package-owned failures. Completion requires all package-owned failures to be
resolved; environmental checks may be explicitly marked unverified with their
exact error output.

## Non-goals for this pass

- Implementing a BCF parser/query adapter.
- Implementing a file-backed MAF parser.
- Rewriting all ggplot2 internals or redesigning the complete plotting engine.
- Reorganizing the large core files solely to reduce line counts.
- Modifying or deleting untracked research data and plots.
