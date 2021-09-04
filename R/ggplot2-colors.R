#' Select a PGL-themed color palette
#'
#' @param who The person associated with the palette.
#' @param what The object the palette was extracted from
#' @param n Number of colors to return
#'
#' @return A character vector of colors
#' @export
#'
#' @examples
#' pgl_pals("christine", "blueberry_boba", 4)
pgl_pals <- function(who, what, n) {
  pals <- list(
    christine = list(
      blueberry_boba = c("#5E4950", "#93604F", "#A2919B", "#77722E", "#9D7E52")
    )
  )

  pal <- pals[[who]][[what]]

  if (rlang::is_missing(n) || n > length(pal)) {
    pal
  } else {
    pal[seq_len(n)]
  }
}
