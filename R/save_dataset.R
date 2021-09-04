save_metalab_data <- function(metadata, data) {
  write.csv(data, here("data", paste0(metadata$filename, ".csv")), row.names = FALSE)
  cat(sprintf("Dataset '%s' saved successfully.\n", metadata$name))
}
