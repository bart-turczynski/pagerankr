# Locale-independent ASCII case folding (PAGE-vzetnwuu, fleet sweep
# SEOR-rxxuzhmc; modeled on pslr's R/ascii.R and sitemapr's).
#
# Base R's tolower() and toupper() follow the session's LC_CTYPE. Under a
# Turkish or Azeri locale on glibc, "I" lowercases to the dotless "ı" and "i"
# uppercases to the dotted "İ", so a fold followed by a comparison fails
# silently: "NAVIGATION" stops matching "navigation". Every string pagerankr
# folds (Screaming Frog header names and enumerated values, XPath steps,
# placement labels, URL hosts) is compared against ASCII constants or an
# ASCII-only pattern, so only A-Z and a-z are mapped and anything else passes
# through unchanged. .lintr bans tolower(), toupper() and casefold() so a new
# call site cannot reintroduce the locale dependence.
.pr_ascii_lower <- function(x) {
  chartr(paste(LETTERS, collapse = ""), paste(letters, collapse = ""), x)
}

.pr_ascii_upper <- function(x) {
  chartr(paste(letters, collapse = ""), paste(LETTERS, collapse = ""), x)
}
