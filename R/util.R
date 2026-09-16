complete <- function(...) {
  args <- list(...)
  !any(unlist(purrr::map(args, ~(is.null(.x) || is.na(.x)))))
}

## GET a URL, retrying transient failures briefly, per the CRAN internet
## resources policy; returns the response, or NULL after a message
http_get_with_retries <- function(url, tries = 3) {
  resp <- NULL
  for (i in seq_len(tries)) {
    resp <- tryCatch(httr::GET(url, httr::timeout(30)), error = function(e) e)
    if (!inherits(resp, "error") && !httr::http_error(resp)) return(resp)
    if (i < tries) Sys.sleep(2 ^ (i - 1))
  }
  reason <- if (inherits(resp, "error")) conditionMessage(resp) else
    paste("HTTP status", httr::status_code(resp))
  message("Could not fetch ", url, " (", reason, ")")
  NULL
}

## load YAML from a local path or URL; NULL (with a message) on failure
## rather than an error, so network problems never abort a caller
load_yaml_gracefully <- function(path, what = "file") {
  if (grepl("^https?://", path)) {
    resp <- http_get_with_retries(path)
    if (is.null(resp)) return(NULL)
    tryCatch(yaml::yaml.load(httr::content(resp, as = "text", encoding = "UTF-8")),
             error = function(e) {
               message("Could not parse ", what, " from ", path, " (",
                       conditionMessage(e), ")")
               NULL
             })
  } else {
    tryCatch(yaml::yaml.load_file(path), error = function(e) {
      message("Could not load ", what, " from ", path, " (",
              conditionMessage(e), ")")
      NULL
    })
  }
}
