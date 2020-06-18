save_dataset <- function(dataset_meta, dataset_data) {
  write.csv(dataset_data, here("data", paste0(dataset_meta$filename, ".csv")), row.names = FALSE)
  cat(sprintf("Dataset '%s' saved successfully.\n", dataset_meta$name))
}
