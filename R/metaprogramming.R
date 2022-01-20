#' Construct a list that names itself
#'
#' @param list_expr A expression using a call to `list`, `c`, or the like, where all arguments
#'   are name-value pairs and the expression evaluates to a list.
#' @param .fn A function passed to `purrr::imap_chr`, called on the character vector returned
#'   after coercing the element symbols in `list_expr` to name. Defaults to the identity function.
#'
#' @note Names of named elements are preserved.
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
#' name_self(c(min, mean, custom_name = max))
#'
#' # Helps when naming produced by from `across()`
#' suppressPackageStartupMessages(library(dplyr))
#'
#' ## With `name_self()`, columns are named after the functions
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarize(across(where(~ max(.x) > 100), name_self(list(min, mean, max))))
#'
#' ## More specifically, it allows `"{.fn"}` inside `.names` to reference the function names
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarize(across(disp, name_self(list(min, avg = mean, max)), .names = "{.col}.{toupper(.fn)}"))
#'
#' ## Without `name_self()`, column names are suffixed with position indices
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarize(across(where(~ max(.x) > 100), list(min, mean, max)))
name_self <- function(list_expr, .fn = ~.x) {
  list_quo <- rlang::enquo(list_expr)
  if (rlang::quo_is_call(list_quo) | is.vector(list_expr)) {
    args <- rlang::call_args(list_quo)
    args_names <- unname(sapply(args, rlang::as_name))
    list_names <- purrr::imap_chr(args_names, .fn)
    if (!is.null(names(list_expr))) {
      list_names <- dplyr::coalesce(purrr::modify_if(names(list_expr), ~ .x == "", ~NA_character_), list_names)
    }
    stats::setNames(object = list_expr, nm = list_names)
  } else {
    cli::cli_abort("Please pass a list-constructing expression to the {.code list_expr} argument.")
  }
}
