fetch_dataset <- function(key, revision = NULL) {
  if (is.null(revision)) {
    message("Note: No version name for Google Sheet specified, reading current Sheet")
    dataset_url <- sprintf(
      "https://docs.google.com/spreadsheets/d/%s/export?id=%s&format=csv",
      key, key)
  } else {
    message(sprintf("Note: reading revision %s", revision))
    dataset_url <- sprintf(
      "https://docs.google.com/spreadsheets/d/%s/export?id=%s&format=csv&revision=%s",
      key, key, revision)
  }
  
  tryCatch({
    suppressMessages({
      dataset_url %>%
        httr::GET() %>%
        httr::content(col_names = TRUE, col_types = NULL, encoding = "UTF-8")
    })
  },
  error = function(e) {
    cat(sprintf("Can't load dataset with key '%s' and revision '%s'. Exception: %s.\n",
                key, revision, e))
  })
}
