#' Save and open a ggplot
#'
#' Wrapper around `ggplot2::ggsave()` that saves the rendered output to a special folder
#' and opens it using the system's default app for the graphic type.
#'
#' @param ... Passed to `ggplot2::ggsave()`, overriding `path` and `filename`.
#' @param load_as_magick Whether to show information about the saved plot and invisibly return an ImageMagick object of the plot for post-processing. Defaults to \code{FALSE}.
#'
#' @return A magick object or path to the saved plot.
#'
#' @note Creates a folder `/.ggsave_auto` in the working directory where figures are saved
#'   with zero-padding to 3 digits (e.g., `ggsave_auto-001.png`). Appends `.ggsave_auto` to
#'   a `.gitignore` file if it exists.
#'
#'   Global params can be set through `options()`, with the argument name followed by `pgl.ggsave_auto.`
#'   such as `options(pgl.ggsave_auto.width = 7)`.
#'
#' @export
#'
ggsave_auto <- function(..., load_as_magick = FALSE) {
  p <- ggplot2::last_plot()
  if (is.null(p)) {
    cli::cli_abort("There is no plot to save.")
  }

  working_dir <- getwd()
  if (!dir.exists(".ggsave_auto")) {
    idx <- 1
    dir.create(".ggsave_auto")
  } else {
    saved <- dir(path = ".ggsave_auto", pattern = "ggsave_auto-\\d{3}\\.png")
    if (length(saved) == 0) {
      idx <- 1
    } else {
      idx <- max(as.integer(stringr::str_extract(saved, "\\d{3}"))) + 1
    }
  }

  params <- list(...)
  params$plot <- p
  params$path <- ".ggsave_auto"
  params$filename <- sprintf("ggsave_auto-%03d.png", idx)

  global_opts <- grep("^pgl\\.ggsave_auto\\.", names(options()), value = TRUE)
  if (length(global_opts) > 0) {
    opts_list <- unlist(lapply(grep("^pgl\\.ggsave_auto\\.", names(options()), value = TRUE), options))
    names(opts_list) <- gsub("^pgl\\.ggsave_auto\\.", "", names(opts_list))
    params <- utils::modifyList(params, as.list(opts_list))
  }

  img_path <- withVisible(do.call(ggplot2::ggsave, params))$value
  cli::cli_alert_success("Plot saved at: {.path {img_path}}")
  system2("open", img_path)

  if (file.exists(".gitignore") && !(".ggsave_auto" %in% readLines(".gitignore"))) {
    writeLines(c(readLines(".gitignore"), ".ggsave_auto", ""), ".gitignore")
  }

  if (load_as_magick) {
    img <- magick::image_read(img_path)
    img_info <- magick::image_info(img)
    invisible(img)
  } else {
    invisible(img_path)
  }
}

#' Save and open any plot
#'
#' Wrapper around `ragg::agg_png()` that saves the output from a plotting expression to a special folder
#' and opens it using the system's default app for the graphic type.
#'
#' @param plot_expr An expression that plots a figure to a device as a side effect, to be captured by `ragg::agg_png()`.
#' @param ... Passed to `ragg::agg_png()`, overriding `filename`. Uses the same width and height as the RStudio viewer pane size by default, like `ggplot2::ggsave()`.
#' @param load_as_magick Whether to show information about the saved plot and invisibly return an ImageMagick object of the plot for post-processing. Defaults to \code{FALSE}.
#'
#' @return A magick object or path to the saved plot.
#'
#' @note Creates a folder `/.raggsave_auto` in the working directory where figures are saved
#'   with zero-padding to 3 digits (e.g., `raggsave_auto-001.png`). Appends `.raggsave_auto` to
#'   a `.gitignore` file if it exists.
#'
#'   Global params can be set through `options()`, with the argument name followed by `pgl.raggsave_auto.`
#'   such as `options(pgl.raggsave_auto.width = 7)`.
#'
#' @export
#'
raggsave_auto <- function(plot_expr = NULL, ..., load_as_magick = FALSE) {
  `%||%` <- function(lhs, rhs) {
    if (is.null(lhs)) {
      rhs
    } else {
      lhs
    }
  }

  plot_expr <- rlang::enquo(plot_expr)

  if (rlang::quo_is_null(plot_expr)) {
    cli::cli_abort("There is no plot expression to evaluate and save.")
  }

  working_dir <- getwd()
  if (!dir.exists(".raggsave_auto")) {
    idx <- 1
    dir.create(".raggsave_auto")
  } else {
    saved <- dir(path = ".raggsave_auto", pattern = "raggsave_auto-\\d{3}\\.png")
    if (length(saved) == 0) {
      idx <- 1
    } else {
      idx <- max(as.integer(stringr::str_extract(saved, "\\d{3}"))) + 1
    }
  }
  if (file.exists(".gitignore") && !(".raggsave_auto" %in% readLines(".gitignore"))) {
    writeLines(c(readLines(".gitignore"), ".raggsave_auto", ""), ".gitignore")
  }

  params <- list(...)
  params$path <- ".raggsave_auto"
  params$filename <- sprintf("raggsave_auto-%03d.png", idx)

  global_opts <- grep("^pgl\\.raggsave_auto\\.", names(options()), value = TRUE)
  if (length(global_opts) > 0) {
    opts_list <- unlist(lapply(grep("^pgl\\.raggsave_auto\\.", names(options()), value = TRUE), options))
    names(opts_list) <- gsub("^pgl\\.raggsave_auto\\.", "", names(opts_list))
    params <- utils::modifyList(params, as.list(opts_list))
  }

  img_path <- paste0(".raggsave_auto/", params$filename)

  dev_size <- grDevices::dev.size(units = "in")
  file <- ragg::agg_png(
    filename = img_path,
    width = params$width %||% dev_size[1],
    height = params$height %||% dev_size[2],
    units = params$units %||% "in",
    res = params$res %||% params$dpi %||% 150,
    pointsize = params$pointsize %||% 12,
    background = params$background %||% "white",
    scaling = params$scaling %||% 1,
    bitsize = params$bitsize %||% 8
  )
  rlang::eval_tidy(plot_expr)
  grDevices::dev.off()

  cli::cli_alert_success("Plot saved at: {.path {img_path}}")
  system2("open", img_path)

  if (load_as_magick) {
    img <- magick::image_read(img_path)
    img_info <- magick::image_info(img)
    invisible(img)
  } else {
    invisible(img_path)
  }
}


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
