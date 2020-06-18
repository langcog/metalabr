complete <- function(...) {
  args = list(...)
  !any(unlist(map(args, ~(is.null(.x) || is.na(.x)))))
}
