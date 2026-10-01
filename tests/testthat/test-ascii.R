# ASCII-only case folding (PAGE-vzetnwuu, fleet sweep SEOR-rxxuzhmc).

test_that("only ASCII letters change case", {
  expect_identical(
    .pr_ascii_lower("ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
    "abcdefghijklmnopqrstuvwxyz"
  )
  expect_identical(
    .pr_ascii_upper("abcdefghijklmnopqrstuvwxyz"),
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  )
  # Non-ASCII letters, the Turkish dotless and dotted I among them, pass
  # through unchanged in both directions.
  others <- intToUtf8(c(0xC4, 0x131, 0x130, 0xE4), multiple = TRUE)
  expect_identical(.pr_ascii_lower(others), others)
  expect_identical(.pr_ascii_upper(others), others)
  expect_identical(.pr_ascii_lower(c("NAV", NA)), c("nav", NA))
  expect_identical(.pr_ascii_upper(character()), character())
})

test_that("Screaming Frog values and hosts survive a Turkish locale", {
  # The hazard is glibc's: under tr_TR, tolower("I") is the dotless "ı". Run
  # where the platform has the locale; on macOS it exists but folds I to i, so
  # there the test only pins that nothing regresses.
  suppressWarnings(withr::local_locale(c(LC_CTYPE = "tr_TR.UTF-8")))
  skip_if_not(
    identical(Sys.getlocale("LC_CTYPE"), "tr_TR.UTF-8"),
    "tr_TR.UTF-8 locale not available"
  )
  expect_identical(.sf_header_key("Link Origin"), "linkorigin")
  expect_identical(
    .sf_parse_allowed(c("DISALLOWED", "ALLOWED")),
    c(FALSE, TRUE)
  )
  expect_identical(.sf_url_host("HTTPS://WWW.EXAMPLE.IO/x"), "www.example.io")
  expect_identical(
    .sf_region_from_one_path("/HTML/BODY/DIV[@id='x']/ASIDE/A"),
    .sf_region_from_one_path("/html/body/div[@id='x']/aside/a")
  )
})
