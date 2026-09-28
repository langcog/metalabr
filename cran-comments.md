## Resubmission

This is a resubmission addressing the review of 2026-09-27. Changes:

* **\dontrun{} usage**: examples that require no credentials are now
  executable and wrapped in `\donttest{}` (`get_metalab_versions()`,
  `get_current_metalab_data()`; both complete in well under 5 seconds and
  fail gracefully without network). The two remaining `\dontrun{}` examples
  (`get_metalab_data()`, `get_metalab_metadata()`) really cannot be
  executed without credentials: they read from the Redivis data repository,
  whose client requires an API token or an interactive OAuth login.
* **.GlobalEnv**: `get_current_metalab_data()` no longer writes to the
  global environment; it returns the loaded objects as a named list. (An
  optional `envir` argument lets the user explicitly request assignment
  into an environment they supply.)
* **set.seed() within a function**: the hardcoded seed in the correlation
  imputation (R/tidy_dataset.R) is gone. The seed is now a documented
  user-facing argument (`imputation_seed`, whose default reproduces the
  released MetaLab datasets exactly), and the user's RNG state
  (`.Random.seed`) is saved and restored around the imputation, so calling
  the function never alters the session's random number stream.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Internet resources

metalabr provides access to the MetaLab database, an online data resource.
Per the CRAN policy on internet resources:

* All functions that use internet resources fail gracefully: transient
  failures are retried with backoff, and persistent failures produce an
  informative `message()` and a `NULL` return, never an error. The version
  registry additionally falls back to a built-in copy when the site is
  unreachable.
* All tests that access internet resources use `skip_on_cran()` plus a
  credential gate; the remaining tests exercise the effect-size computation,
  data tidying, and validation code against bundled fixtures with no
  network access.
* Vignette chunks that access the network are only evaluated when
  `NOT_CRAN=true`.

## Suggested package not on CRAN

The `redivis` package (the client for the Redivis data repository, where
released MetaLab data are hosted) is not on CRAN. It is used only behind
`requireNamespace()` guards, is listed in `Suggests`, and is available from
the repository declared in `Additional_repositories`
(<https://langcog.r-universe.dev>). Users without it get an informative
message with the install command, and `get_current_metalab_data()` provides
dependency-free access to the current release.
