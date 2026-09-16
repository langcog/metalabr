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
* All examples that access internet resources are wrapped in `\dontrun{}`
  because they require network access (and, for the Redivis-backed
  functions, an account token), and can be long-running.
* All tests that access internet resources use `skip_on_cran()` plus a
  credential gate; the remaining tests exercise the effect-size computation,
  data tidying, and validation code against bundled fixtures with no
  network access.

## Suggested package not on CRAN

The `redivis` package (the client for the Redivis data repository, where
released MetaLab data are hosted) is not on CRAN. It is used only behind
`requireNamespace()` guards, is listed in `Suggests`, and is available from
the repository declared in `Additional_repositories`
(<https://langcog.r-universe.dev>). Users without it get an informative
message with the install command, and `get_current_metalab_data()` provides
dependency-free access to the current release.
