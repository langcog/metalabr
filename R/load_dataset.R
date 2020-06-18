load_dataset <- function(dataset_short_name) {

  dataset_meta <- datasets %>% filter(short_name == dataset_short_name)
  if (!nrow(dataset_meta)) {
    cat(sprintf("Dataset '%s' isn't in datasets metadata.\n", dataset_short_name))
    return()
  }

  dataset_contents <- fetch_dataset(dataset_meta)
  if (is.null(dataset_contents)) {
    return()
  }

  valid_dataset <- validate_dataset(dataset_meta, dataset_contents)
  if (!valid_dataset) {
    cat(sprintf(
      "Dataset '%s' had one or more validation issues, not being cached.\n",
      dataset_meta$name))
    return()
  }

  dataset_data <- tidy_dataset(dataset_meta, dataset_contents)
  save_dataset(dataset_meta, dataset_data)

}
