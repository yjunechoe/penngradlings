#' Quick table of regression model output
#'
#' @param x A linear model with a `broom::tidy()` method defined
#'
#' @return A `tinytable `table
#' @export
#'
#' @examples
#' x <- lm(mpg ~ hp, mtcars)
#' table_lm(x) # %>% tinytable::save_tt("ex.docx") %>% browseURL()
table_lm <- function(x) {

  requireNamespace("broom", quietly = TRUE)
  requireNamespace("broom.mixed", quietly = TRUE)

  tidy(x, effects = "fixed") %>%
    `colnames<-`(c("", "estimate", "S.E.", "t-statistic", "p-value")) %>%
    tinytable::tt(theme = "striped") %>%
    tinytable::format_tt(j = c(2, 3), digits = 2, num_zero = TRUE) %>%
    tinytable::format_tt(j = 4, digits = 3, num_zero = TRUE) %>%
    tinytable::format_tt(j = 5, fn = \(x) ifelse(x < 0.001, "<0.001", round(x, 3))) %>%
    tinytable::style_tt(j = 2:5, align = "r")

}
