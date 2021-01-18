complete <- function(...) {
  args <- list(...)
  !any(unlist(purrr::map(args, ~(is.null(.x) || is.na(.x)))))
}


complete <- function(...) {
  args <- list(...)
  !any(unlist(purrr::map(args, ~(is.null(.x) || is.na(.x)))))
}

complete(1,2,3,"")

dplyr::coalesce(1,2,3,NA)
