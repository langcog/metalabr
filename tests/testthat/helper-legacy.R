# The characterization suite captures the behavior of the legacy pipeline
# (v0.9.1) before the Redivis rewrite. Numeric comparisons are exact to 1e-12.

expect_same_value <- function(got, want, label = "") {
  if (is.numeric(want) || is.numeric(got)) {
    got <- suppressWarnings(as.numeric(got))
    want <- suppressWarnings(as.numeric(want))
    both_na <- (is.na(got) & is.na(want)) |
      (is.infinite(got) & is.infinite(want) & sign(got) == sign(want))
    ok <- both_na | (!is.na(got) & !is.na(want) & abs(got - want) < 1e-12)
  } else {
    ok <- (is.na(got) & is.na(want)) | (!is.na(got) & !is.na(want) & got == want)
  }
  testthat::expect_true(all(ok, na.rm = FALSE),
    label = sprintf("%s: %d/%d values differ (first diff at index %s)",
                    label, sum(!ok, na.rm = TRUE), length(ok),
                    paste(head(which(!ok), 3), collapse = ",")))
}
