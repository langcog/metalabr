#' Load the current MetaLab data release into the global environment
#'
#' Downloads the current release's data bundle from the MetaLab site (no
#' account or extra packages needed) and loads two objects into the global
#' environment: `metalab_data` (one row per effect size) and `dataset_info`
#' (one row per dataset), plus `metalab_release` (the release name).
#'
#' For versioned, pinnable access use \code{\link{get_metalab_data}}, which
#' returns the data as a value instead of loading it globally.
#'
#' @param rdata_file URL or path of the data bundle; defaults to the bundle
#'   served by the MetaLab site.
#' @return Invisibly, the names of the loaded objects. Called for its side
#'   effect.
#' @export
get_current_metalab_data <- function(rdata_file = get_current_data_url()) {
  loaded <- tryCatch({
    con <- url(rdata_file)
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    load(con, envir = .GlobalEnv)
  }, error = function(e) {
    message("Could not download the MetaLab data bundle: ", conditionMessage(e))
    NULL
  })
  if (!is.null(loaded) && "metalab_release" %in% loaded) {
    message("Loaded MetaLab data release ", get("metalab_release", .GlobalEnv),
            " into the global environment (objects: ",
            paste(setdiff(loaded, "metalab_release"), collapse = ", "), ").")
  }
  invisible(loaded)
}

get_cached_metalab_data <- function(rdata_file = get_cached_data_file()) {
  load(rdata_file, envir = .GlobalEnv)
}

get_current_data_url <- function() {
  paste0(metalab_site_url, "/resources/metalab.Rdata")
}

get_cached_data_file <- function() {
  file.path("shinyapps", "site_data", "Rdata", "metalab.Rdata")
}
