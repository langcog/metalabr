tidy_dataset <- function(dataset_meta, dataset_contents) {

  # Coerce each field's values to the field's type, discard any columns not in
  # field spec, add NA columns for missing (optional) fields
  dataset_data <- data_frame(row = 1:nrow(dataset_contents))
  for (field in fields) {
    if (field$field %in% names(dataset_contents)) {
      if (field$type == "string") {
        field_fun <- as.character
      } else if (field$type == "numeric") {
        field_fun <- as.numeric
      } else {
        field_fun <- function(x) x
      }
      dataset_data[,field$field] <- field_fun(dataset_contents[[field$field]])
    } else {
      dataset_data[,field$field] <- NA
    }
  }

  # Impute values for missing correlations
  set.seed(111)
  # First we replace corr values outside the range (.01,.99) with NA
  dataset_data = dataset_data %>%
    mutate(corr = abs(corr)) %>%
    mutate(corr = ifelse(corr > .99 | corr < .01, NA, corr))
  # Then impute NA values
  if (all(is.na(dataset_data$corr))) {
    dataset_data$corr_imputed <- NA
  } else {
    dataset_data$corr_imputed <- dataset_data$corr %>%
      Hmisc::impute(fun = "random") %>%
      as.numeric()
  }

  # Compute effect sizes and variances
  dataset_data_calc <- dataset_data %>%
    mutate(dataset = dataset_meta[["name"]],
           short_name = dataset_meta[["short_name"]],
           domain = dataset_meta[["domain"]]) %>%
    split(.$row) %>%
    map_df(~bind_cols(
      .x, compute_es(
        .x$participant_design, .x$x_1, .x$x_2, .x$x_dif, .x$SD_1, .x$SD_2,
        .x$SD_dif, .x$n_1, .x$n_2, .x$t, .x$F, .x$d, .x$d_var, .x$corr,
        .x$corr_imputed, .x$r, .x$r_var, .x$study_ID, .x$expt_num,
        .x$special_cases_measures, .x$contrast_sampa, .x$short_name
      ))) %>%
    select(-row)

  # Add any other derived values
  method_options <- keep(fields, ~.x$field == "method")[[1]]$options
  method_names <- unlist(map(method_options, ~.x[[names(.x)]]$fullname))
  names(method_names) <- unlist(map(method_options, names))

  dataset_data_calc %>%
    # mutate(dataset = dataset_meta[["name"]],
    #        short_name = dataset_meta[["short_name"]],
    #        method = unlist(method_names[method])) %>%
    mutate(method = unlist(method_names[method])) %>%
    rowwise() %>%
    mutate(mean_age = weighted.mean(c(mean_age_1, mean_age_2), c(n_1, n_2),
                                    na.rm = TRUE),
           n = mean(c(n_1, n_2), na.rm = TRUE),
           same_infant_calc = paste(study_ID,same_infant)) %>%
    add_rownames("unique_row") %>%
    ungroup()
}
