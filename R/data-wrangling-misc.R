#' Discretize a continuous variable into quantiles
#'
#' @param x Numeric
#' @param q Probabilities. Should be 1 less than the number of discrete bins to be returned.
#'   `x` is cut from left-to-right, filling up the smallest bin first. For example, `c(.25, .50, .75)`
#'   returns a factor of 4 levels - 0%-25%, 25%-50%, 50%-75%, 75%-100%. Bins are left-inclusive and right-exclusive.
#' @param ... Passed to `stats::quantile`
#'
#' @return
#' @export
#'
#' @examples
#' qcut(1:100, c(.2, .4, .6, .8))
qcut <- function(x, q = c(.25, .50, .75), ...) {
  vals <- stats::quantile(x, probs = sort(unique(c(0, q))), ...)
  cuts <- vapply(x, function(.x) max(which(.x >= vals)), numeric(1))
  labs <- paste0("(", names(vals), c(names(vals)[-1], "100%"), "]")
  names(vals)[cuts]
}
