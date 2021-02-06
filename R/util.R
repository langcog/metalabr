complete <- function(...) {
  args <- list(...)
  !any(unlist(purrr::map(args, ~(is.null(.x) || is.na(.x)))))
}
