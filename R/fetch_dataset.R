fetch_dataset <- function(dataset_meta) {

  if (dataset_meta$key == "") {
    cat(sprintf("Can't load dataset '%s', key missing.\n", dataset_meta$name))
    return()
  }

  dataset_url <- sprintf(
    "https://docs.google.com/spreadsheets/d/%s/export?id=%s&format=csv",
    dataset_meta$key, dataset_meta$key
  )

  tryCatch({
    dataset_url %>%
      httr::GET() %>%
      httr::content(col_names = TRUE, col_types = NULL, encoding = "UTF-8")
  },
  error = function(e) {
    cat(sprintf("Can't load dataset '%s' with key '%s'. Exception: %s.\n", dataset_meta$name,
                dataset_meta$key, e))
  })

}
