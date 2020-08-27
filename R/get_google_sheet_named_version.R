get_google_sheet_named_versions <- function(keys) {
  message(paste("calling get_google_sheet_named_versions"))
  ret_list <- lapply(keys, function(key) {
    sheet_url <- paste0("https://docs.google.com/spreadsheets/d/",
                        key,
                        "/revisions/tiles?id=",
                        key,
                        "&start=1&showDetailedRevisions=false&filterNamed=true")

    revision_json <- httr::GET(sheet_url) %>% content("text")
    ## response body JSON starts with )]}'\n, so need to extract from position 6
    revision_list <- revision_json %>% str_sub(6) %>% fromJSON()
    revision_list$tileInfo 
  })

  names(ret_list) <- keys
  ret_list
}
