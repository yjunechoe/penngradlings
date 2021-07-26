mean_se_corrected <- function(x, mult = 1) {
  x <- stats::na.omit(x)
  se <- mult * sqrt( stats::var(x) / (length(x) - 1) )
  mean <- mean(x)
  as.data.frame(list(y = mean, ymin = mean - se, ymax = mean + se), n = 1)
}
