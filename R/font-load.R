#' Load package fonts
#'
#' Registers fonts contained in {penngradlings} with \code{systemfonts::register_font()}. This function is called once silently when the package is loaded.
#'
#' All styles of a font family are registered as the Regular member of their own families in the form of "{family}-{style(s)}".
#'
#' Call \code{systemfonts::registry_fonts()} after loading the fonts for more details (also returned invisibly).
#'
#' @param verbose Whether the newly registered fonts should be printed to the console. Defaults to \code{TRUE}.
#'
#' @section Fonts loaded:
#' \describe{
#'     \item{`Charis SIL`}{ by SIL international, retrieved from \url{https://software.sil.org/charis/}. }
#'     \item{`Inter`}{ by Rasmus Andersson, retrieved from \url{https://fonts.google.com/specimen/Inter}. }
#'     \item{`Piazzolla`}{ by Juan Pablo del Peral, retrieved from \url{https://fonts.google.com/specimen/Piazzolla}. }
#'     \item{`Atkinson Hyperlegible`}{ by Braille Institute, retrieved from \url{https://fonts.google.com/specimen/Atkinson+Hyperlegible}. }
#'     \item{`Bootstrap Icons`}{ by The Bootstrap Authors, retrieved from \url{https://icons.getbootstrap.com/}. }
#'     \item{`Material Icons`}{ by Google Design, retrieved from \url{https://fonts.google.com/icons/}. }
#'     \item{`Font Awesome 5 Free`}{ by Dave Gandy \url{https://fontawesome.com/}.
#'       The free version contains a subset of the Regular, Solid, and Brand styles:
#'       \url{https://fontawesome.com/v5.15/icons?d=gallery&p=2&s=brands,regular,solid&m=free}.
#'     }
#' }
#'
#' Copy of the license for each font is included in the package source.
#' The use of these fonts in the package does not indicate endorsement or liability of the copyright/trademark holders.
#' We thank the developers of the fonts for their open-source contributions and permissive licenses.
#'
#' @return An invisible call to \code{systemfonts::registry_fonts()} for further inspection.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_pkg_fonts()
#' }
#'
#' @importFrom rlang .data .env
load_pkg_fonts <- function(verbose = TRUE) {
  custom_names <- c(
    "bootstrap-icons" = "BootstrapIcons-Regular",
    "MaterialIcons-Regular" = "MaterialIcons-Filled",
    "MaterialIconsOutlined-Regular" = "MaterialIcons-Outlined",
    "Font-Awesome-5-Brands-Regular-400" = "FontAwesome5-Brands",
    "Font-Awesome-5-Free-Regular-400" = "FontAwesome5-Regular",
    "Font-Awesome-5-Free-Solid-900" = "FontAwesome5-Solid"
  )

  .load_pkg_font <- function(family) {
    font_dir <- system.file("fonts", family, package = "penngradlings")
    font_paths <- dir(font_dir, full.names = TRUE)
    font_names <- stringr::str_remove(dir(font_dir), "\\..*$")

    if (all(font_names %in% names(custom_names))) {
      font_names <- unname(custom_names[font_names])
    }

    purrr::walk2(
      font_names, font_paths,
      function(name, path) {
        features <- list("kern" = 1, "zero" = 0)
        if (stringr::str_detect(name, "^Inter-")) {
          features <- c(features, "numbers" = "tabular")
        } else if (stringr::str_detect(name, "^Piazzolla-")) {
          features <- c(features, "numbers" = "proportional")
        } else if (stringr::str_detect(name, "^AtkinsonHyperlegible-")) {
          features <- c(features, "numbers" = "proportional")
        }
        feature_spec <- do.call(systemfonts::font_feature, features)
        systemfonts::register_font(name = name, plain = path, features = feature_spec)
      }
    )

    if (verbose) {
      cli::cli({
        cli::cli_h2("{.strong {family}}")
        cli::cli_alert_success("{.val {length(font_names)}} style{?s} registered:")
        cli::cli_ul(font_names)
      })
    }
  }
  pkg_fonts <- dir(system.file("fonts", package = "penngradlings"))
  purrr::walk(pkg_fonts, .load_pkg_font)
  if (verbose) {
    cli::cli_rule()
    cli::cli_alert_info("Done! Check {.code systemfonts::registry_fonts()} for more details.")
  }
  invisible(systemfonts::registry_fonts())
}

#' Fonts included in the package
#'
#' @return A named list of font families and font styles
#' @export
#'
#' @examples
#' \dontrun{
#' .pgl_fonts()
#' }
.pgl_fonts <- function() {
  families <- dir(system.file("fonts", package = "penngradlings"))
  fonts <- purrr::map(families, ~ dir(system.file("fonts", .x, package = "penngradlings")))
  fonts <- purrr::map(fonts, ~ stringr::str_remove(.x, "\\..*$"))
  stats::setNames(fonts, families)
}
