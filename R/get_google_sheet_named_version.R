get_google_sheet_named_version <- function(sheet_id) {

  sheet_url <- paste0("https://docs.google.com/spreadsheets/d/",
                      sheet_id,
                      "/revisions/tiles?id=",
                      sheet_id,
                      "&start=1&showDetailedRevisions=false&filterNamed=true")

  httr::GET(sheet_url) %>% content("text")
}

