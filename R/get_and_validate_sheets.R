get_and_validate_sheets <- function(metalab_metadata, specs) {
  cat("Getting raw MetaLab data from Google Sheets for dataset:", metalab_metadata$short_name, "\n")

  published_version <- get_published_version(metalab_metadata$short_name)

  if ((length(published_version) > 0) && (published_version == metalab_metadata$published_version)) {
    cat(paste0("Version specified for ",
               metalab_metadata$short_name ," (",
               published_version ,
               ") is already published, skipping update.\n"))
    return()
  }
    
  drive_share(file = as_id(metalab_metadata$key), role = "writer", type = "anyone")
  metalab_dataset <- fetch_metalab_data(metalab_metadata$key, metalab_metadata$published_version)
  drive_share(file = as_id(metalab_metadata$key), role = "reader", type = "anyone")
  
  if (is.null(metalab_dataset)) {
    return()
  }

  is_valid_dataset <- is_valid_metalab_data(metalab_metadata,
                                            metalab_dataset,
                                            specs)

  if (!is_valid_dataset) {
    return()
  }

  ## update version in metadata
  update_published_version(metalab_metadata)
  
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

get_published_version <- function(sn) {
  versions_file <- here::here("shinyapps", "site_data", "versions", "dataset-versions.csv")
  versions_df <- read.csv(versions_file, header = TRUE, colClasses = c("character", "integer"))
  versions_df %>% filter(short_name == sn) %>% pull(version)
}

update_published_version <- function(metalab_metadata) {
  versions_file <- here::here("shinyapps", "site_data", "versions", "dataset-versions.csv")
  versions_df <- read.csv(versions_file, header = TRUE, colClasses = c("character", "integer"))

  versions_df <- dplyr::bind_rows(versions_df,
                                  data.frame(short_name = metalab_metadata$short_name,
                                             version = metalab_metadata$published_version))

  write.csv(versions_df, file = versions_file, quote = FALSE, row.names = FALSE)
}
