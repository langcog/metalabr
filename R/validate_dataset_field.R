validate_dataset_field <- function(dataset_name, dataset_contents, field) {
  if (field$required) {
    if (field$field %in% names(dataset_contents)) {
      if (field$type == "options") {
        if (class(field$options) == "list") {
          options <- names(unlist(field$options, recursive = FALSE))
        } else {
          options <- field$options
        }
        invalid_values <- unique(dataset_contents[[field$field]]) %>%
          setdiff(options)
        if (!is.null(field$nullable) && field$nullable) {
          invalid_values <- na.omit(invalid_values)
        }
        if (length(invalid_values)) {
          for (value in invalid_values) {
            cat(sprintf("Dataset '%s' has invalid value '%s' for field '%s'.\n",
                        dataset_name, value, field$field))
          }
          return(FALSE)
        }
      } else if (field$type == "numeric") {
        field_contents <- dataset_contents[[field$field]]
        if (!(is.numeric(field_contents) || all(is.na(field_contents)))) {
          cat(sprintf("Dataset '%s' has wrong type for numeric field '%s'.\n",
                      dataset_name, field$field))
          return(FALSE)
        }
      }
    } else {
      cat(sprintf("Dataset '%s' is missing required field: '%s'.\n",
                  dataset_name, field$field))
      return(FALSE)
    }
  }
  return(TRUE)
}