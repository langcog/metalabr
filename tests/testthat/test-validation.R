# Unit tests for the field-validation rules (validate_metalab_dataset_field.R)

make_spec <- function(...) list(...)

test_that("required field presence", {
  spec <- make_spec(field = "study_ID", type = "string", required = TRUE)
  expect_true(suppressMessages(
    metalabr:::validate_metalab_field("x", data.frame(study_ID = "a"), spec)))
  expect_false(suppressMessages(
    metalabr:::validate_metalab_field("x", data.frame(other = 1), spec)))
})

test_that("non-required fields always pass", {
  spec <- make_spec(field = "whatever", type = "numeric", required = FALSE)
  expect_true(suppressMessages(
    metalabr:::validate_metalab_field("x", data.frame(other = 1), spec)))
})

test_that("short_cite: no missing values, max 60 characters", {
  spec <- make_spec(field = "short_cite", type = "string", required = TRUE)
  expect_true(suppressMessages(
    metalabr:::validate_metalab_field("x", data.frame(short_cite = "Smith (2015)"), spec)))
  expect_false(suppressMessages(
    metalabr:::validate_metalab_field("x", data.frame(short_cite = c("ok", NA)), spec)))
  expect_false(suppressMessages(
    metalabr:::validate_metalab_field(
      "x", data.frame(short_cite = strrep("a", 61)), spec)))
})

test_that("options fields: values within allowed options; nullable excuses NA", {
  spec <- make_spec(field = "peer_reviewed", type = "options",
                    options = c("yes", "no"), required = TRUE, nullable = TRUE)
  expect_true(suppressMessages(
    metalabr:::validate_metalab_field(
      "x", data.frame(peer_reviewed = c("yes", "no", NA)), spec)))
  expect_false(suppressMessages(
    metalabr:::validate_metalab_field(
      "x", data.frame(peer_reviewed = c("yes", "maybe")), spec)))
  # non-nullable: NA is an invalid value
  spec2 <- make_spec(field = "peer_reviewed", type = "options",
                     options = c("yes", "no"), required = TRUE)
  expect_false(suppressMessages(
    metalabr:::validate_metalab_field(
      "x", data.frame(peer_reviewed = c("yes", NA)), spec2)))
})

test_that("options fields with keyed (fullname) option lists", {
  spec <- make_spec(field = "method", type = "options",
                    options = list(list(CF = list(fullname = "central fixation")),
                                   list(HPP = list(fullname = "head-turn"))),
                    required = TRUE)
  expect_true(suppressMessages(
    metalabr:::validate_metalab_field("x", data.frame(method = c("CF", "HPP")), spec)))
  expect_false(suppressMessages(
    metalabr:::validate_metalab_field("x", data.frame(method = "eyetracking"), spec)))
})

test_that("numeric fields: numeric or all-NA", {
  spec <- make_spec(field = "n_1", type = "numeric", required = TRUE)
  expect_true(suppressMessages(
    metalabr:::validate_metalab_field("x", data.frame(n_1 = c(1, 2)), spec)))
  expect_true(suppressMessages(
    metalabr:::validate_metalab_field("x", data.frame(n_1 = c(NA, NA)), spec)))
  expect_false(suppressMessages(
    metalabr:::validate_metalab_field("x", data.frame(n_1 = c("12", "abc")), spec)))
})

test_that("r and corr must lie in [-1, 1]", {
  for (f in c("r", "corr")) {
    spec <- make_spec(field = f, type = "numeric", required = TRUE)
    ok <- data.frame(a = c(-1, 0, 1, NA)); names(ok) <- f
    bad <- data.frame(a = c(0.5, 1.2)); names(bad) <- f
    expect_true(suppressMessages(metalabr:::validate_metalab_field("x", ok, spec)))
    expect_false(suppressMessages(metalabr:::validate_metalab_field("x", bad, spec)))
  }
})

test_that("whole-dataset validation aggregates per-field results", {
  specs <- list(
    make_spec(field = "study_ID", type = "string", required = TRUE),
    make_spec(field = "n_1", type = "numeric", required = TRUE))
  good <- data.frame(study_ID = "a", n_1 = 10)
  bad <- data.frame(study_ID = "a", n_1 = "ten")
  expect_true(suppressMessages(
    metalabr:::is_valid_metalab_data(list(name = "x"), good, specs)))
  expect_false(suppressMessages(
    metalabr:::is_valid_metalab_data(list(name = "x"), bad, specs)))
})
