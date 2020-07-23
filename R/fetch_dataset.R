fetch_dataset <- function(key) {
  dataset_url <- sprintf(
    "https://docs.google.com/spreadsheets/d/%s/export?id=%s&format=csv",
    key, key)
  
  tryCatch({
    suppressMessages({
      dataset_url %>%
        httr::GET() %>%
        httr::content(col_names = TRUE, col_types = NULL, encoding = "UTF-8")
    })
  },
  error = function(e) {
    cat(sprintf("Can't load dataset with key '%s'. Exception: %s.\n", key, e))
  })
}
