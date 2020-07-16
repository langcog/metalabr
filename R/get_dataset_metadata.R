get_dataset_metadata <- function(file) {
  yaml::yaml.load_file(file)
}
