# Lint (gate 5): the linter set

`.lintr` is intentionally aligned with the linter set `goodpractice::gp()` runs
(`goodpractice:::linters_to_lint()`), so the local and CI `lintr::lint_package()`
gate surfaces the same findings as the goodpractice report reviewers run. Without
that alignment a package passes its own lint gate and then trips a pile of
goodpractice findings later. Regenerate the list after a goodpractice upgrade,
comparing against `names(goodpractice:::linters_to_lint())`.

**Keep `.lintr` free of `#` comments.** It is parsed with `read.dcf()`, which
only learned to skip comment lines in R 4.6. On R 4.5 and older a single comment
makes `lint_package()` abort with `Invalid DCF format`, so the rationale lives
here instead. Keep it ASCII too: a non-ASCII byte in `.lintr` comes back
`bytes`-encoded on older R and makes any config error surface as a confusing
`sprintf()` failure instead of the real message.

Documented deviations from the goodpractice set — test-idiom and public-API
reasons a real package hits as it grows:

- `object_name_linter` / `object_usage_linter`: not part of the goodpractice set
  and deliberately NOT added. The cucumber DSL (`when`/`then`/`context`) and the
  testthat helpers read as undefined globals to `object_usage_linter`, and
  packages commonly expose mixed-case or dotted public parameters plus
  `._`-prefixed internal helpers that `object_name_linter` would flag.
- `expect_identical_linter`: off. Suites routinely rely on `expect_equal()`'s
  numeric tolerance (`expect_equal(nrow(x), 2)` compares integer vs double) and
  its string-encoding normalization, both of which `identical()` rejects; a
  wholesale swap means retyping literals for no behavioral gain.
- `implicit_assignment_linter`: off. Tests use the standard
  `expect_warning(res <- f(), "msg")` idiom to capture both the warning and the
  return value (`expect_warning()` returns the condition, not the value).
- `library_require_linter`: off. `tests/testthat.R` and vignette setup chunks
  legitimately call `library()`.
- `undesirable_operator_linter`: configured to keep flagging `<<-`/`->>` but
  allow `:::`, which tests use to reach internal (unexported) functions.

`strings_as_factors_linter` is off, as in goodpractice, which dropped it in 1.2.0
(ropensci-review-tools/goodpractice#321). It only guarded the pre-R-4.0
`data.frame()` default, and this package Depends on R >= 4.1.0. The fleet
turned it off on 2026-07-18, before goodpractice did (`PAGE-iiqjlfxl`).
Its absence does not make `stringsAsFactors = FALSE` removable everywhere:
`expand.grid()` kept `TRUE` as its default through R 4.0, so the argument in
`R/auto_grid.R` is load-bearing.

**Measuring cyclomatic complexity.** `cyclocomp::cyclocomp_package_dir()` does
not see `.`-prefixed functions, and those are most of this package: it reports
83 functions against the 447 in the namespace, 364 of them dot-prefixed
(measured 2026-09-29). Score the loaded namespace instead:

```r
pkgload::load_all(".")
ns <- asNamespace("pagerankr")
fns <- Filter(function(n) is.function(get(n, envir = ns)), ls(ns, all.names = TRUE))
sort(vapply(fns, function(n) cyclocomp::cyclocomp(get(n, envir = ns)), 0), decreasing = TRUE)
```

The cheapest reduction is usually splitting one `&&`/`||` guard chain into
sequential single-condition `if`s: a four-way `||` guard scores 11, the same
four checks as separate `if`s score 5.
