#' Corrected calculation of mean and standard error
#'
#' Adapted from \code{ggplot2::mean_se}. Corrects the denominator for the variance with \eqn{N - 1}.
#'
#' @param x Numeric vector.
#' @param mult Number of multiples of standard error.
#'
#' @return A dataframe with three columns:
#' \describe{
#'     \item{`y`}{ The mean.}
#'     \item{`ymin`}{ The mean minus the multiples of the standard error.}
#'     \item{`ymax`}{ The mean plus the multiples of the standard error.}
#' }
#' @export
#' @examples
#' \dontrun{
#' x <- rnorm(100)
#' mean_se_corrected(x)
#' }
mean_se_corrected <- function(x, mult = 1) {
  x <- stats::na.omit(x)
  se <- mult * sqrt(stats::var(x) / (length(x) - 1))
  mean <- mean(x)
  as.data.frame(list(y = mean, ymin = mean - se, ymax = mean + se), n = 1)
}
