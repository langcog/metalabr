fetch_ml_data <- function(key, revision = NA) {
  if (is.na(revision)) {
    dataset_url <- sprintf(
      "https://docs.google.com/spreadsheets/d/%s/export?id=%s&format=csv",
      key, key)
  } else {
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
