#' Construct a list that names itself
#'
#' @param list_expr A expression using a call to `list`, `c`, or the like, where all arguments
#'   are name-value pairs and the expression evaluates to a list.
#' @param .fn A function passed to `purrr::imap_chr`, called on the character vector returned
#'   after coercing the element symbols in `list_expr` to name. Defaults to the identity function.
#'
#' @return A named list
#' @export
#'
#' @examples
#' name_self(c(min, mean, max))
#' name_self(c(min, mean, max), .fn = ~ toupper(.x))
#' name_self(c(min, mean, max), .fn = function(x, y) {
#'   paste0(x, y)
#' })
#'
#' # Helps when naming produced by from `across()`
#' library(dplyr)
#'
#' ## Without `name_self()`
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarize(across(where(~ max(.x) > 100), list(min, mean, max)))
#'
#' ## With `name_self()`
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarize(across(where(~ max(.x) > 100), name_self(list(min, mean, max))))
name_self <- function(list_expr, .fn = function(x, y) {
                        x
                      }) {
  list_quo <- rlang::enquo(list_expr)
  if (rlang::quo_is_call(list_quo) | is.vector(list_expr)) {
    args <- rlang::call_args(list_quo)
    args_names <- unname(sapply(args, rlang::as_name))
    list_names <- purrr::imap_chr(args_names, .fn)
    stats::setNames(object = list_expr, nm = list_names)
  } else {
    cli::cli_abort("Please pass a list-constructing expression to the {.code list_expr} argument.")
  }
}
