is_valid_required_field <- function(dataset_name, dataset_contents, field) {
  if (!field$field %in% names(dataset_contents)) {
    cat(sprintf("Dataset '%s' is missing required field: '%s'.\n",
                dataset_name, field$field))
    return(FALSE)
  } else {
    return(TRUE)
  }
}

is_valid_options_field <- function(dataset_name, dataset_contents, field) {
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
  } else {
    return(TRUE)
  }
}

is_valid_numeric_field <- function(dataset_name, dataset_contents, field) {
  field_contents <- dataset_contents[[field$field]]
  if (!(is.numeric(field_contents) || all(is.na(field_contents)))) {
    cat(sprintf("Dataset '%s' has wrong type for numeric field '%s'.\n",
                dataset_name, field$field))
    return(FALSE)
  } else {
    return(TRUE)
  }
}

is_valid_r_corr <- function(dataset_name, dataset_contents, field){
  field_contents <- dataset_contents[[field$field]]
  if ((any(field_contents > 1, na.rm = TRUE)) || any(field_contents < -1, na.rm = TRUE)){
      cat(sprintf("Dataset '%s' has '%s' out of range [-1,1]",
                  dataset_name, field$field))
      return(FALSE)
    } else {
      return(TRUE)
    }
}


validate_dataset_field <- function(dataset_name, dataset_contents, field) {
  if (field$required) {
    if (!is_valid_required_field(dataset_name, dataset_contents, field)) {
      return(FALSE)
    }
    if (field$type == "options") {
      if (!is_valid_options_field(dataset_name, dataset_contents, field)) {
        return(FALSE)
      }
    } else if (field$type == "numeric") {
      if (!is_valid_numeric_field(dataset_name, dataset_contents, field)) {
        return(FALSE)
      }
      if (field$field == "r" || field$field == "corr"){
        if(!is_valid_r_corr(dataset_name, dataset_contents, field)){
          return(FALSE)
        }
      }
  
    }
  }
  return(TRUE)
}
