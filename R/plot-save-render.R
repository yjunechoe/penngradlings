#' Save and open plot
#'
#' Wrapper around `ggplot2::ggsave()` that opens rendered output using the system's default app for the graphic type.
#'
#' @param ... Passed to `ggplot2::ggsave()`, with some defaults optimized for publication figures.
#' @param load_as_magick Whether to show information about the saved plot and invisibly return an ImageMagick object of the plot for post-processing. Defaults to \code{FALSE}.
#'
#' @section Default options for publication figures:
#' \describe{
#'     \item{`width`}{ Defaults to 5.2in, 80% of the full-width of letter-sized paper (8.5in) minus 1in margins. Accepts a string of percentage scale (ex: "50%"). }
#'     \item{`height`}{ Defaults to 3in, 33% of the full-width of letter-sized paper (11in) minus 1in margins. Accepts a string of percentage scale (ex: "50%"). }
#' }
#' If either width or height are specified as percentages, the following defaults are also set:
#' \describe{
#'     \item{`dpi`}{ 300, the print standard. }
#'     \item{`units`}{ "in" }
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # p <- ggplot2::qplot(mpg, hp, data = mtcars)
#' # ggplot2::ggsave2("myplot.png", p, width = "50%", height = "30%")
#' }
ggsave2 <- function(..., load_as_magick = FALSE) {
  inner_height <- 9
  inner_width <- 6.5
  opts <- list(...)
  if (is.null(opts$height)) {
    opts$height <- "33%"
  }
  if (is.null(opts$width)) {
    opts$width <- "80%"
  }
  if (is.character(opts$height)) {
    height_pct <- as.numeric(stringr::str_replace(opts$height, "^(\\d+)%$", "\\1"))
    opts$height <- inner_height * height_pct / 100
    opts$units <- "in"
    if (is.null(opts$dpi)) {
      opts$dpi <- 300
    }
  }
  if (is.character(opts$width)) {
    width_pct <- as.numeric(stringr::str_replace(opts$width, "^(\\d+)%$", "\\1"))
    opts$width <- inner_width * width_pct / 100
    opts$units <- "in"
    if (is.null(opts$dpi)) {
      opts$dpi <- 300
    }
  }
  path <- withVisible(do.call(ggplot2::ggsave, opts))$value
  system2("open", path)
  if (load_as_magick) {
    img <- magick::image_read(path)
    img_info <- magick::image_info(img)
    cli::cli_alert_success("Plot saved at: {.path {path}}")
    return(invisible(img))
  }
}
