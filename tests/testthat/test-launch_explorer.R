# Tests for launch_pagerank_explorer()
# This function is a thin Shiny launcher; we test the validation logic
# by creating copies of the function with overridden environments.

describe("launch_pagerank_explorer validation", {
  it("errors when shiny is not available", {
    fn <- launch_pagerank_explorer
    env <- new.env(parent = environment(fn))
    env$requireNamespace <- function(pkg, ...) {
      if (pkg == "shiny") return(FALSE)
      base::requireNamespace(pkg, ...)
    }
    environment(fn) <- env
    expect_error(fn(), "shiny.*required")
  })

  it("errors when DT is not available", {
    fn <- launch_pagerank_explorer
    env <- new.env(parent = environment(fn))
    env$requireNamespace <- function(pkg, ...) {
      if (pkg == "DT") return(FALSE)
      base::requireNamespace(pkg, ...)
    }
    environment(fn) <- env
    expect_error(fn(), "DT.*required")
  })

  it("errors when app directory is not found", {
    fn <- launch_pagerank_explorer
    env <- new.env(parent = environment(fn))
    env$system.file <- function(...) ""
    environment(fn) <- env
    expect_error(fn(), "Could not find the Shiny app")
  })

  it("messages about visNetwork when not installed", {
    fn <- launch_pagerank_explorer
    env <- new.env(parent = environment(fn))
    env$requireNamespace <- function(pkg, ...) {
      if (pkg == "visNetwork") return(FALSE)
      base::requireNamespace(pkg, ...)
    }
    env$system.file <- function(...) "/fake/app/path"
    # Create a mock shiny module so shiny::runApp resolves to our mock
    mock_shiny_ns <- new.env(parent = emptyenv())
    mock_shiny_ns$runApp <- function(...) invisible(NULL)
    env$shiny <- mock_shiny_ns
    environment(fn) <- env
    # The function calls shiny::runApp which resolves via the real namespace,
    # so we expect the visNetwork message before it fails or succeeds
    expect_message(
      tryCatch(fn(), error = function(e) NULL),
      "visNetwork"
    )
  })
})
