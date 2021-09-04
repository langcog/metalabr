get_revs <- function(file_id) {
  request <- request_generate(
    endpoint = "drive.revisions.list",
    params = list(
      fileId = file_id 
    ),
    token = drive_token()
  )
  
  res <- request_make(request)
  revision <- gargle::response_process(res)
  revs <- bind_rows(revision$revisions)
  revs %>%
    mutate(formatted_ts =
             as.POSIXct(modifiedTime, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC") %>%
             format(format = "%B %d, %Y, %I:%M %p", tz = Sys.timezone()) %>%
             gsub(pattern = " 0", replacement = " ")) %>% arrange(desc(modifiedTime))
}

get_rev_from_ts <- function(revs, timestamp) {
  ## trim ts
  ## str(revs %>% arrange(desc(formatted_ts)))
  revs %>% filter(formatted_ts == timestamp) %>% pull(id)
}
