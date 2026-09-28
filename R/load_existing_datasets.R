#' Get the current MetaLab data release
#'
#' Downloads the current release's data bundle from the MetaLab site (no
#' account or extra packages needed) and returns its contents:
#' `metalab_data` (one row per effect size), `dataset_info` (one row per
#' dataset), and `metalab_release` (the release name).
#'
#' For versioned, pinnable access use \code{\link{get_metalab_data}}.
#'
#' @param rdata_file URL or path of the data bundle; defaults to the bundle
#'   served by the MetaLab site.
#' @param envir Optional environment. If supplied (e.g. `globalenv()`), the
#'   objects are also assigned there, reproducing the behavior of metalabr
#'   0.x, which always loaded into the global environment.
#' @return Invisibly, a named list with elements `metalab_data`,
#'   `dataset_info`, and `metalab_release`, or `NULL` (with a message) if
#'   the bundle could not be downloaded.
#' @export
#' @examples
#' \donttest{
#'   ml <- get_current_metalab_data()
#'   if (!is.null(ml)) head(ml$dataset_info$name)
#' }
get_current_metalab_data <- function(rdata_file = get_current_data_url(),
                                     envir = NULL) {
  bundle <- new.env(parent = emptyenv())
  loaded <- tryCatch({
    con <- url(rdata_file)
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    suppressWarnings(load(con, envir = bundle))
  }, error = function(e) {
    message("Could not download the MetaLab data bundle: ", conditionMessage(e))
    NULL
  })
  if (is.null(loaded)) return(invisible(NULL))
  out <- mget(loaded, envir = bundle)
  if ("metalab_release" %in% loaded) {
    message("Loaded MetaLab data release ", out$metalab_release,
            " (objects: ", paste(loaded, collapse = ", "), ").")
  }
  if (!is.null(envir)) list2env(out, envir = envir)
  invisible(out)
}

get_cached_metalab_data <- function(rdata_file = get_cached_data_file()) {
  bundle <- new.env(parent = emptyenv())
  loaded <- load(rdata_file, envir = bundle)
  invisible(mget(loaded, envir = bundle))
}

get_current_data_url <- function() {
  paste0(metalab_site_url, "/resources/metalab.Rdata")
}

get_cached_data_file <- function() {
  file.path("shinyapps", "site_data", "Rdata", "metalab.Rdata")
}
