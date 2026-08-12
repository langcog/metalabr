#' Get MetaLab effect-size data
#'
#' By default, reads a released version of the MetaLab database from Redivis
#' (\url{https://redivis.com/datasets/81tq-9ewzpdvz0}) — one row per effect
#' size across all datasets, with all coded and derived columns. Releases are
#' versioned and citable; the release used is announced with a message.
#'
#' Passing `metalab_metadata` instead selects the legacy curator path: fetch
#' the live Google Sheets listed in the metadata, validate them against the
#' field spec, and compute effect sizes. This is how new data releases are
#' produced and is subject to change between calls as curators edit sheets.
#'
#' @param metalab_metadata Optional data frame of dataset metadata (from
#'   \code{\link{get_metalab_metadata}} with a `key` column) to fetch live
#'   from Google Sheets. If omitted, released data are read from Redivis.
#' @param short_names Optional character vector of dataset short names to
#'   include.
#' @param domains Optional character vector of domains to include (mutually
#'   exclusive with `short_names`).
#' @param specs Field specification (live-sheets path only); defaults to the
#'   current spec.
#' @param perform_validation Whether to validate sheets before inclusion
#'   (live-sheets path only).
#' @param version A MetaLab release name (e.g. `"2026.1"`), or `"current"`
#'   (default) for the latest release (Redivis path only).
#' @return A data frame (tibble) of effect sizes, or `NULL` (with a message)
#'   if released data could not be fetched.
#' @export
#' @examples
#' \dontrun{
#'   # released data (recommended)
#'   metalab_data <- get_metalab_data()
#'   metalab_data <- get_metalab_data(version = "2023.1")
#'   mutex <- get_metalab_data(short_names = "mutex")
#'
#'   # live curator sheets (legacy path)
#'   metadata <- get_metalab_metadata(source = "sheets")
#'   metalab_data <- get_metalab_data(metadata)
#' }
get_metalab_data <- function(metalab_metadata = NULL, short_names = NULL,
                             domains = NULL, specs = NULL,
                             perform_validation = TRUE, version = "current") {
  if (!is.null(short_names) && !is.null(domains)) {
    stop("Only provide one of short_names or domains")
  }

  if (is.null(metalab_metadata)) {
    dat <- read_metalab_table("effect_sizes", version = version)
    if (is.null(dat)) return(NULL)
    if (!is.null(short_names)) {
      dat <- dat %>% filter(short_name %in% short_names)
    }
    if (!is.null(domains)) {
      dat <- dat %>% filter(domain %in% domains)
    }
    return(dat)
  }

  ## legacy live-sheets path
  if (is.null(specs)) {
    specs <- get_metalab_specs()
  }

  if (!is.null(short_names)) {
    metalab_metadata <- metalab_metadata %>% filter(short_name %in% short_names)
  }

  if (!is.null(domains)) {
    metalab_metadata <- metalab_metadata %>% filter(domain %in% domains)
  }

  metalab_metadata %>%
    purrr::pmap_dfr(function(...) {
        get_and_validate_sheets(list(...), specs, perform_validation = perform_validation)
    })
}
