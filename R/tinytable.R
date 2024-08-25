#' Quick table of regression model output
#'
#' @param x A model with a `broom::tidy()` method defined, or pre-tidied data.
#' @param beta_digits Digits to approximate on the estimate. This also controls
#'   digits printed for the S.E. and t-statistic columns. Defaults to `2`.
#' @param write Whether to write to a temporary file and open it in Viewer.
#'   Defaults to `FALSE`. Can specify a format for `tinytable::save_tt()`.
#'
#' @return A `tinytable `table
#' @export
#'
#' @examples
#' x <- lm(mpg ~ hp, mtcars)
#' table_lm(x) # `write = ".docx"` to also write out to a Word file
table_lm <- function(x, beta_digits = 2, write = FALSE) {

  if (!is.data.frame(x)) {
    requireNamespace("broom", quietly = TRUE)
    requireNamespace("broom.mixed", quietly = TRUE)
    x <- tidy(x, effects = "fixed")
  }

  digits <- pmax(1, beta_digits + c(0, -1, 1))

  tbl <- x %>%
    `colnames<-`(c("", "estimate", "S.E.", "t-statistic", "p-value")) %>%
    tinytable::tt(theme = "striped") %>%
    tinytable::format_tt(j = 2, digits = digits[1], num_zero = TRUE) %>%
    tinytable::format_tt(j = 3, digits = digits[2], num_zero = TRUE) %>%
    tinytable::format_tt(j = 4, digits = digits[3], num_zero = TRUE) %>%
    tinytable::format_tt(j = 5, fn = \(x) ifelse(x < 0.001, "<0.001", round(x, 3))) %>%
    tinytable::style_tt(j = 2:5, align = "r")

  if (!isFALSE(write) && is.character(write)) {
    path <- tempfile(fileext = write)
    tinytable::save_tt(tbl, path)
    cli_inform_write(path)
    utils::browseURL(path)
  }

  return(tbl)

}
