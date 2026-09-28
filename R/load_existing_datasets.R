#' Get the current MetaLab data release
#'
#' Downloads the current release's effect-size data from the MetaLab site
#' (no account or extra packages needed) and returns it as a data frame,
#' one row per effect size across all datasets. The release name is
#' announced with a message and attached as the `metalab_release`
#' attribute.
#'
#' For versioned, pinnable access use \code{\link{get_metalab_data}}; for
#' the dataset registry use \code{\link{get_metalab_metadata}}.
#'
#' @param rdata_file URL or path of the data bundle; defaults to the bundle
#'   served by the MetaLab site.
#' @return A data frame (tibble) of effect sizes, or `NULL` (with a
#'   message) if the bundle could not be downloaded.
#' @export
#' @examples
#' \donttest{
#'   metalab_data <- get_current_metalab_data()
#'   if (!is.null(metalab_data)) dim(metalab_data)
#' }
get_current_metalab_data <- function(rdata_file = get_current_data_url()) {
  bundle <- new.env(parent = emptyenv())
  loaded <- tryCatch({
    con <- url(rdata_file)
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    suppressWarnings(load(con, envir = bundle))
  }, error = function(e) {
    message("Could not download the MetaLab data bundle: ", conditionMessage(e))
    NULL
  })
  if (is.null(loaded) || !"metalab_data" %in% loaded) {
    if (!is.null(loaded)) message("Data bundle did not contain metalab_data.")
    return(invisible(NULL))
  }
  out <- get("metalab_data", envir = bundle)
  release <- if ("metalab_release" %in% loaded)
    get("metalab_release", envir = bundle) else NULL
  if (!is.null(release)) {
    message("Using MetaLab data release ", release, ".")
    attr(out, "metalab_release") <- release
  }
  out
}

get_current_data_url <- function() {
  paste0(metalab_site_url, "/resources/metalab.Rdata")
}
