#' Derive factor levels from another factor with unique pair-wise matches
#'
#' @param new A vector
#' @param old A factor
#'
#' @return A factor
#' @export
#'
#' @examples
#' old <- factor(LETTERS[1:5], LETTERS[c(3, 2, 1, 5, 4)])
#' old
#' new <- letters[1:5]
#' new
#' factor(new)
#' fct_match(new, old)
fct_match <- function(new, old) {
  if (!is.factor(old)) {
    stop("`old` must be a factor")
  }
  new <- as.character(new)
  if (length(unique(new)) != nlevels(old)) {
    stop("Non-equal number of discrete categories between `new` and `old`")
  }
  combos <- table(new, old)
  combo_matches <- which(combos != 0, arr.ind = TRUE)
  if (nrow(combo_matches) != nlevels(old)) {
    stop("Non-unique pairwise matches between `new` and `old`")
  }
  factor(new, levels = names(sort(combo_matches[,2])))
}
