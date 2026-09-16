# Live-backend tests (skipped on CRAN and without credentials); the nightly
# CI canary runs these so backend drift surfaces as a red run, not a user
# bug report.

test_that("get_metalab_versions returns a usable registry", {
  skip_on_cran()
  versions <- suppressMessages(get_metalab_versions())
  expect_type(versions, "list")
  expect_true(is.character(versions$current) || is.character(versions$current[[1]]))
  expect_true(TEST_VERSION %in% names(versions$releases))
})

test_that("released effect sizes match the pinned release", {
  skip_if_no_redivis()
  dat <- suppressMessages(get_metalab_data(version = TEST_VERSION))
  skip_if(is.null(dat), "Redivis unreachable")
  expect_equal(nrow(dat), TEST_VERSION_ROWS)
  expect_equal(dplyr::n_distinct(dat$short_name), TEST_VERSION_DATASETS)
  expect_true(all(c("d_calc", "d_var_calc", "g_calc", "mean_age_months",
                    "same_infant_calc") %in% names(dat)))
  expect_true(all(is.finite(dat$d_calc)))
})

test_that("short_names filtering works on the released data", {
  skip_if_no_redivis()
  mutex <- suppressMessages(get_metalab_data(short_names = "mutex",
                                             version = TEST_VERSION))
  skip_if(is.null(mutex), "Redivis unreachable")
  expect_equal(unique(mutex$short_name), "mutex")
  expect_equal(nrow(mutex), 146L)
})

test_that("released metadata has registry shape and list-columns", {
  skip_if_no_redivis()
  metadata <- suppressMessages(get_metalab_metadata(version = TEST_VERSION))
  skip_if(is.null(metadata), "Redivis unreachable")
  expect_equal(nrow(metadata), TEST_VERSION_DATASETS)
  expect_true(is.list(metadata$moderators))
  expect_true("mutex" %in% metadata$short_name)
})

test_that("unknown release fails informatively", {
  skip_on_cran()
  # version resolution precedes any Redivis call, but the no-redivis guard
  # precedes both, so this needs the package (not credentials)
  skip_if_not_installed("redivis")
  expect_error(suppressMessages(get_metalab_data(version = "1999.1")),
               "Unknown MetaLab release")
})
