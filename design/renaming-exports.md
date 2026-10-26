# Renaming an exported function

`_pkgdown.yml`'s `reference:` index lists topics by name, and a stale entry
fails the site build with "must be a known topic name or alias". Nothing
before the merge sees it: tests, lint and `R CMD check --as-cran` all pass,
the pre-push gate does not run pkgdown, and the `pages` job runs only on
`main`, so the first red result arrives after the merge. The
`resolve_urls()` -> `resolve_redirect_urls()` rename (PR #119) cost a round
trip exactly this way.

On any export rename:

1. Sweep for the old name with **no file-extension filter**:
   `git grep -nwE 'old_name'`. A sweep limited to `.R`/`.Rmd`/`.md`/`.Rd`
   skips `.yml`.
2. Run `pkgdown::check_pkgdown()`. It validates the whole reference index in
   seconds without building the site.
3. Also update `NAMESPACE` (via roxygen), the `R/` and `tests/testthat/` file
   names, `README.Rmd` (then `devtools::build_readme()`), vignette reference
   tables, and a `NEWS.md` entry.
