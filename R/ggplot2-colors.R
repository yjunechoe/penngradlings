#' Generate a PGL-themed color palette
#'
#' @param what The object the palette was extracted from. It's sufficient to just supply this argument.
#' @param who The person associated with the palette. This argument is optional.
#'
#' @return A unary palette generating function that takes the number of colors to return.
#' @export
#'
#' @examples
#' library(colorspace)
#' swatchplot(pgl_pals("blueberry_matcha_boba", "christine")())
#' swatchplot(pgl_pals("cat_coaster", "xin")())
#' swatchplot(pgl_pals("cat_coaster", "xin")(15))
#' swatchplot(pgl_pals("hk_postcard_dull", "may")())
#' swatchplot(pgl_pals("hk_postcard_sharp", "may")())
pgl_pals <- function(what = NULL, who = NULL) {
  pals <- list(
    christine = list(
      blueberry_matcha_boba = c("#5C76B6", "#767300", "#CEA15F", "#935F4F")
    ),
    xin = list(
      cat_coaster = c("#3343CF", "#66A89C", "#D9CC04", "#F29F05", "#CC4D06")
    ),
    may = list(
      hk_postcard_dull = c("#44889C", "#E1AF00"),
      hk_postcard_sharp = c("#398DA1", "#FFA466")
    )
  )

  resolve_pal <- function(pal, n) {
    pal_len <- length(pal)
    if (n <= pal_len) {
      pal[seq_len(n)]
    } else if (n > pal_len) {
      grDevices::colorRampPalette(pal)(n)
    }
  }

  if (is.null(what) && is.null(who)) {
    function(n) {
      if (n %in% c(1, 2)) {
        what <- "hk_postcard_dull"
        who <- "may"
      } else if (n %in% c(3, 4)) {
        what <- "blueberry_matcha_boba"
        who <- "christine"
      } else if (n >= 5) {
        what <- "cat_coaster"
        who <- "xin"
      }
      pal <- pals[[who]][[what]]
      resolve_pal(pal, n)
    }
  } else {
    if (!is.null(what) && !is.null(who)) {
      pal <- pals[[who]][[what]]
    } else if (!is.null(what) && is.null(who)) {
      pal <- unlist(unname(pals), recursive = FALSE)[[what]]
    }

    function(n) {
      if (rlang::is_missing(n)) {
        pal
      } else {
        resolve_pal(pal, n)
      }
    }
  }
}

#' Discrete color scales
#'
#' @param what,who passed to `pgl_pals()`
#' @param ... passed to `ggplot2::discrete_scale()`
#' @rdname scale_discrete
#' @export
scale_color_pgl_discrete <- function(what = NULL, who = NULL, ...) {
  ggplot2::discrete_scale(
    "color", "pgl_continuous",
    pgl_pals(what, who),
    ...
  )
}

#' @rdname scale_discrete
#' @export
scale_colour_pgl_discrete <- function(what = NULL, who = NULL, ...) {
  ggplot2::discrete_scale(
    "colour", "pgl_continuous",
    pgl_pals(what, who),
    ...
  )
}

#' @rdname scale_discrete
#' @export
scale_fill_pgl_discrete <- function(what = NULL, who = NULL, ...) {
  ggplot2::discrete_scale(
    "fill", "pgl_continuous",
    pgl_pals(what, who),
    ...
  )
}
