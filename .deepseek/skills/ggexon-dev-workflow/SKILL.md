---
name: ggexon-dev-workflow
description: Development workflow for the ggexon R package. Regenerate documentation, test pkgdown locally, commit, and push. Use when making changes to R source files, vignettes, or _pkgdown.yml in the ggexon repository.
---

# ggexon Development Workflow

## Prerequisites

- [pandoc](https://pandoc.org) must be installed (`brew install pandoc` on macOS)
- Working directory is the repo root

## Workflow

After making changes to any of:
- `R/*.R` (source code)
- `vignettes/*.Rmd` (documentation)
- `_pkgdown.yml` (site config)

run these steps in order:

### 1. Regenerate Rd files

```r
roxygen2::roxygenise()
```

This picks up new `@param`, `@export`, and `@section` tags from R source files
and writes fresh `.Rd` files into `man/`.

### 2. Build pkgdown site locally

```r
pkgdown::build_site()
```

This does a full site build: sitrep check → home page → function reference →
articles (vignettes) → sitemap → search index.

**If it fails**, read the error carefully. Common causes:

| Symptom | Fix |
|---|---|
| `Reference metadata not ok` + a topic name | Remove or fix the topic reference in `_pkgdown.yml` |
| `X topics missing from index` | Add the missing topic to `_pkgdown.yml` or mark it `@keywords internal` |
| `Pandoc not available` | Install pandoc |

After fixing, re-run `pkgdown::build_site()` until it succeeds.

### 3. Stage and commit all changes

```bash
git add -A
git status          # review what's staged
git commit -m "type: description"
```

Include the regenerated `man/*.Rd` files and any `docs/` changes in the
commit. Use [conventional commits](https://www.conventionalcommits.org):
`feat:`, `fix:`, `chore:`, `docs:`.

### 4. Pull and push

```bash
git pull --rebase
git push
```

If rebase conflicts occur (common when CI pkgdown committed `docs/`),
resolve by removing local untracked `docs/` files that CI already created:

```bash
rm -rf docs/reference/<conflicting-files>
git pull --rebase
```

### 5. Verify CI

Check the pkgdown workflow at:
https://github.com/DongyaoLiu/ggexon/actions/workflows/pkgdown.yaml

Wait for the run to complete. If it fails, pull the CI-committed `docs/`
changes and investigate locally.

## Notes

- **Never skip step 1.** Stale Rd files are the #1 cause of CI pkgdown
  failures because `pkgdown::build_site_github_pages()` on CI reads
  pre-generated Rd files, not roxygen2 comments.
- `docs/` is auto-committed by CI after a successful pkgdown run. After
  pulling CI changes, your local `docs/` will be synced.
- The CI workflow ignores pushes that only touch `docs/**`, so a docs-only
  push won't trigger a redundant rebuild.
