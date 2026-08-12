metalab_site_url <- "https://langcog.github.io/metalab"

## fallback used if the served versions.json is unreachable; keep in sync at
## each release
metalab_builtin_versions <- list(
  current = "2026.1",
  releases = list(
    "2023.1" = list(redivis_version = "v1.0"),
    "2026.1" = list(redivis_version = "v1.1")
  )
)

#' Get the mapping between MetaLab data releases and Redivis dataset versions
#'
#' Fetches the release registry served by the MetaLab site, falling back to a
#' built-in copy if the site is unreachable.
#'
#' @return A list with elements `current` (release name) and `releases`
#'   (named list mapping release names to metadata including
#'   `redivis_version`).
#' @export
get_metalab_versions <- function() {
  tryCatch({
    resp <- httr::GET(paste0(metalab_site_url, "/resources/versions.json"),
                      httr::timeout(10))
    httr::stop_for_status(resp)
    httr::content(resp, as = "parsed", type = "application/json")
  }, error = function(e) {
    message("Could not fetch the MetaLab version registry (",
            conditionMessage(e), "); using the built-in copy.")
    metalab_builtin_versions
  })
}

resolve_metalab_version <- function(version = "current") {
  versions <- get_metalab_versions()
  release <- if (identical(version, "current")) versions$current else version
  info <- versions$releases[[release]]
  if (is.null(info)) {
    stop("Unknown MetaLab release '", release, "'. Available: ",
         paste(names(versions$releases), collapse = ", "), call. = FALSE)
  }
  list(release = release, redivis_version = info$redivis_version)
}

announce_version <- function(v) {
  message("Using MetaLab data release ", v$release,
          " (Redivis datapages.metalab ", v$redivis_version, "). ",
          "Pass version = \"<release>\" to pin a release.")
}

redivis_available <- function() {
  requireNamespace("redivis", quietly = TRUE)
}

read_metalab_table <- function(table, version = "current") {
  if (!redivis_available()) {
    message("The 'redivis' package is required to read released MetaLab data.\n",
            "Install it with:\n",
            "  install.packages('redivis', repos = c('https://langcog.r-universe.dev', getOption('repos')))\n",
            "or use get_current_metalab_data() (no extra dependencies).")
    return(NULL)
  }
  v <- resolve_metalab_version(version)
  tryCatch({
    ds <- redivis::redivis$organization("datapages")$dataset(
      "metalab", version = v$redivis_version)
    out <- ds$table(table)$to_tibble()
    announce_version(v)
    attr(out, "metalab_release") <- v$release
    out
  }, error = function(e) {
    message("Could not read table '", table, "' from Redivis: ",
            conditionMessage(e))
    NULL
  })
}

restore_list_cols <- function(df, cols) {
  for (col in cols) {
    if (col %in% names(df) && is.character(df[[col]])) {
      df[[col]] <- lapply(df[[col]], function(j) {
        if (is.na(j) || !nzchar(j)) character(0)
        else as.character(jsonlite::fromJSON(j))
      })
    }
  }
  df
}
