get_and_validate_sheets <- function(metalab_metadata, specs, perform_validation = TRUE) {
  cat("Getting raw MetaLab data from Google Sheets for dataset:", metalab_metadata$short_name, "\n")

  ## current_version <- get_current_version(metalab_metadata$short_name)

  ## if ((length(current_version) > 0) && (current_version == metalab_metadata$current_version)) {
  ##   cat(paste0("Version specified for ",
  ##              metalab_metadata$short_name ," (",
  ##              current_version ,
  ##              ") is already the current version, skipping update.\n"))
  ##   return()
  ## }
    
  ## googledrive::drive_share(file = googledrive::as_id(metalab_metadata$key), role = "writer", type = "anyone")
  ## metalab_dataset <- fetch_metalab_data(metalab_metadata$key, metalab_metadata$current_version)
  metalab_dataset <- fetch_metalab_data(metalab_metadata$key)  
  ## googledrive::drive_share(file = googledrive::as_id(metalab_metadata$key), role = "reader", type = "anyone")
  
  if (is.null(metalab_dataset)) {
    return()
  }

  is_valid_dataset <- is_valid_metalab_data(metalab_metadata,
                                            metalab_dataset,
                                            specs)

  if (perform_validation && !is_valid_dataset) {
    return()
  }

  ## update version in metadata
  ## update_current_version(metalab_metadata)
  
  avg_month <- 365.2425 / 12.0
  ## NB: do we need all_mod here? what is the d_calc filter?
  tidy_dataset(metalab_metadata, metalab_dataset, specs) %>%
    mutate(all_mod = "",
           mean_age_months = mean_age / avg_month) %>%
    filter(!is.na(d_calc)) %>%
    mutate(
      year = ifelse(
        test = grepl("submitted", study_ID),
        yes = Inf,
        no = stringr::str_extract(study_ID, "([:digit:]{4})")),
      study_ID = as.character(study_ID),
      same_infant = as.character(same_infant),
      expt_condition = as.character(expt_condition))
}

get_current_version <- function(sn) {
  versions_file <- here::here("shinyapps", "site_data", "versions", "dataset-versions.csv")
  versions_df <- read.csv(versions_file, header = TRUE, colClasses = c("character", "integer"))
  versions_df %>% filter(short_name == sn) %>% pull(version)
}

update_current_version <- function(metalab_metadata) {
  versions_file <- here::here("shinyapps", "site_data", "versions", "dataset-versions.csv")
  versions_df <- read.csv(versions_file, header = TRUE, colClasses = c("character", "integer"))

  versions_df <- dplyr::bind_rows(versions_df,
                                  data.frame(short_name = metalab_metadata$short_name,
                                             version = metalab_metadata$current_version))

  write.csv(versions_df, file = versions_file, quote = FALSE, row.names = FALSE)
}
