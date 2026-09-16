fetch_metalab_data <- function(key, revision = NA) {
  if (is.na(revision)) {
    dataset_url <- sprintf(
      "https://docs.google.com/spreadsheets/d/%s/export?id=%s&format=csv",
      key, key)
  } else {
    dataset_url <- sprintf(
      "https://docs.google.com/spreadsheets/d/%s/export?id=%s&format=csv&revision=%s",
      key, key, revision)
  }

  resp <- http_get_with_retries(dataset_url)
  if (is.null(resp)) {
    message("Can't load dataset with key '", key, "'.")
    return(NULL)
  }

  ## a permission change or deleted sheet can return an HTML page; refuse to
  ## parse it as data rather than producing garbage rows
  ctype <- httr::headers(resp)[["content-type"]] %||% ""
  if (!grepl("text/csv", ctype, fixed = TRUE)) {
    message("Can't load dataset with key '", key,
            "': expected CSV but got content type '", ctype, "'.")
    return(NULL)
  }

  tryCatch(
    suppressMessages(
      httr::content(resp, col_names = TRUE, col_types = NULL, encoding = "UTF-8")
    ),
    error = function(e) {
      message("Can't parse dataset with key '", key, "': ", conditionMessage(e))
      NULL
    })
}
