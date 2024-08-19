#' (Randomized) even shuffling of categories
#'
#' @param x Vector
#' @param mode Algorithm for even shuffling:
#'   - "squish" (default): Within each category, maximize the distance between observations, away from the ends of the vector.
#'   - "symmetric": Maximize distance around the category with the smallest number of observations.
#'   - "repel": Within each category, maximize the distance between observations, period.
#' @param shuffle_within Whether observations within each category should be shuffled.
#'   Defaults to `FALSE`, which keeps the ordering of observations within each category.
#'
#' @return A vector of shuffled indices of `x`
#' @export
#'
#' @examples
#'
#' ex <- c(rep("a", 10), rep("bb", 5), rep("ccc", 3), rep("a", 2), rep("bb", 3), rep("ccc", 3))
#' ex
#'
#' # Demos of algorithms
#' barplot(nchar(ex[rshuffle(ex)])) # mode = "squish"
#' barplot(nchar(ex[rshuffle(ex, mode = "symmetric")]))
#' barplot(nchar(ex[rshuffle(ex, mode = "repel")]))
#'
#' # Returns indices for ease of reordering rows
#' data.frame(id = LETTERS, category = ex)[rshuffle(ex), , drop = FALSE]
#'
rshuffle <- function(x, mode = c("squish", "symmetric", "repel"), shuffle_within = FALSE) {

  counts <- table(x)
  if (is.factor(x)) {
    counts <- counts[levels(x)]
  }

  max_n <- max(counts)
  mode <- match.arg(mode)

  tm <- switch(mode,
    "squish" = {
      m <- sapply(names(counts), function(lvl) {
        pos <- round(seq(from = 1, to = max_n + 1, length.out = counts[[lvl]] + 1))
        idx <- which(x == lvl)
        if (shuffle_within) { idx <- sample(idx) }
        replace(integer(max_n + 1), pos, c(idx, 0))
      })[-(max_n + 1),]
      t(m)
    },
    "repel" = {
      m <- sapply(names(counts), function(lvl) {
        pos <- round(seq(from = 1, to = max_n, length.out = counts[[lvl]]))
        idx <- which(x == lvl)
        if (shuffle_within) { idx <- sample(idx) }
        replace(integer(max_n), pos, idx)
      })
      t(m)
    },
    "symmetric" = {
      m <- sapply(names(counts), function(lvl) {
        pos <- round(seq(from = 1, to = max_n, length.out = counts[[lvl]]))
        idx <- which(x == lvl)
        if (shuffle_within) { idx <- sample(idx) }
        replace(integer(max_n), pos, idx)
      })
      .tm <- t(m)
      .tm[,c(FALSE, TRUE)] <- apply(.tm[,c(FALSE, TRUE)], 2, rev)
      .tm
    }
  )

  v <- as.vector(tm)
  v[v > 0]

}


#' Discretize a continuous variable into quantiles
#'
#' @param x Numeric
#' @param q Probabilities. Should be 1 less than the number of discrete bins to be returned.
#'   `x` is cut from left-to-right, filling up the smallest bin first. For example, `c(.25, .50, .75)`
#'   returns a factor of 4 levels - 0%-25%, 25%-50%, 50%-75%, 75%-100%. Bins are left-inclusive and right-exclusive.
#' @param ... Passed to `stats::quantile`
#'
#' @export
#' @examples
#' qcut(1:100, c(.2, .4, .6, .8))
qcut <- function(x, q = c(.25, .50, .75), ...) {
  vals <- stats::quantile(x, probs = sort(unique(c(0, q))), ...)
  cuts <- vapply(x, function(.x) max(which(.x >= vals)), numeric(1))
  labs <- paste0("(", names(vals), c(names(vals)[-1], "100%"), "]")
  names(vals)[cuts]
}
