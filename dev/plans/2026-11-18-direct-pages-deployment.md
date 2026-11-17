# Direct GitHub Pages deployment

Approved scope: publish the generated website as a GitHub Pages artifact and
remove generated site output from the source branch. Preserve the website,
source-audit records, authored design notes, and existing local drafts.

- [x] Copy the four current README previews to `man/figures` and update both
  README and homepage image links to these source-controlled assets.
- [x] Replace the workflow's site commit/push job with pinned official
  `upload-pages-artifact` and `deploy-pages` actions. The build job keeps read
  permission; only the deployment job receives Pages/OIDC write permissions.
- [x] Untrack generated `docs/` output while retaining `dev/` source
  notes and local files. Ignore regenerated site output.
- [x] Document the distinction between R/Markdown sources, preview assets,
  generated website output, and historical HTML source-audit records.
- [x] Verify YAML and action pins, then build the site from a clean staged
  snapshot without tracked HTML. Check homepage image links, required pages,
  artifact contents, and absence of agent-instruction pages.
- [x] Review the workflow change before publishing it.
- [ ] Change Pages publishing mode from `main/docs` to GitHub Actions, publish
  the tested source commit, and monitor artifact deployment to completion.
- [ ] Verify the public website and source branch, and preserve all unrelated
  local drafts.

The 21 HTML records under `data-raw/ape_paper/source_metadata` are provenance
snapshots, not generated package documentation. They remain in place.

Prepublication verification: fresh installed-package `build_site_github_pages()`
passed from a source-only index snapshot with no docs directory. The resulting
artifact has 789 files (18 MiB), all homepage links and four previews resolve,
extra.css loads, and no agent instructions or symlinks are included. Scoped
workflow/asset review found no issues. Live deployment follows this commit.
