#' Load package fonts
#'
#' Registers fonts contained in {penngradlings} with \code{systemfonts::register_font()}. This function is called silently on package load.
#'
#' Call \code{systemfonts::registry_fonts()} after loading the fonts for more details on the registered fonts.
#'
#' @param verbose Whether the newly registered fonts should be printed to the console. Defaults to \code{TRUE}.
#'
#' @section Fonts loaded:
#' \describe{
#'     \item{`Charis SIL`}{ From SIL International: \url{https://software.sil.org/charis/}}.
#'     \item{`Inter`}{ From Google Fonts: \url{https://fonts.google.com/specimen/Inter}}.
#'     \item{`Lato`}{ From Google Fonts: \url{https://fonts.google.com/specimen/Roboto}}.
#' }
#'
#' All fonts are licensed under the SIL Open Font License (OFL): \url{https://scripts.sil.org/OFL}. Copy of the license is also included in the package source.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_pkg_fonts()
#' }
load_pkg_fonts <- function(verbose = TRUE) {
  load_pkg_font <- function(family) {
    font_dir <- system.file("fonts", family, package = "penngradlings")
    font_paths <- dir(font_dir, full.names = TRUE)
    font_names <- stringr::str_remove(dir(font_dir), "\\..*$")

    purrr::walk2(
      font_names, font_paths,
      function(name, path) {
        systemfonts::register_font(name = name, plain = path)
      }
    )

    if (verbose) {
      cli::cli({
        cli::cli_h2("{.strong {family}}")
        cli::cli_alert_success("{.val {length(font_names)}} styles registered:")
        cli::cli_ul(font_names)
      })
    }
  }
  pkg_fonts <- dir(system.file("fonts", package = "penngradlings"))
  purrr::walk(pkg_fonts, load_pkg_font)
  if (verbose) {
    cli::cli_rule()
    cli::cli_alert_info("Done! Check {.code systemfonts::registry_fonts()} for more details.")
  }
}
