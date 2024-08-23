#' Navigate to a file
#'
#' When used interactively, opens the file/ folder.
#'
#' @param x A path.
#'
#' @return Invisibly returns `x`.
#' @export
#'
#' @examples
#' \dontrun{
#' library(fs)
#' file_create(fs::file_temp(ext = "txt")) |> nav()
#' dir_temp() |> nav()
#' }
nav <- function(x) {
  path <- fs::path_tidy(x)
  if (rlang::is_interactive()) {
    fs::file_show(path)
  }
  invisible(path)
}

cli_inform_write <- function(x, start = ".") {
  path <- fs::path_tidy(x)
  relpath <- as.character(fs::path_rel(path, start = start))
  nav_expr <- sprintf('penngradlings::nav("%s")', path)
  cli::cli_inform('Saved to {.run [{relpath}]({nav_expr})}')
}

path_norm_copypaste <- function(x) {
  path <- fs::path_norm(utils::readClipboard())
  utils::writeClipboard(path)
}
