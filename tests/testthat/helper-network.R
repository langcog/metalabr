# Network tests run against a pinned release so new data releases can't
# silently change what the suite asserts; bump deliberately.
TEST_VERSION <- "2023.1"
TEST_VERSION_ROWS <- 2967L
TEST_VERSION_DATASETS <- 32L

skip_if_no_redivis <- function() {
  testthat::skip_on_cran()
  testthat::skip_if(Sys.getenv("REDIVIS_API_TOKEN") == "",
                    "REDIVIS_API_TOKEN not set")
  testthat::skip_if_not_installed("redivis")
}
