#' Save and open plot
#'
#' Wrapper around `ggplot2::ggsave()` that opens rendered output using the system's default app for the graphic type.
#'
#' @param ... Passed to `ggplot2::ggsave()`
#'
#' @export
ggsave2 <- function(...) {
  system2("open", base::withVisible(ggplot2::ggsave(...))$value)
}
