.onLoad <- function(...) {
  load_pkg_fonts(verbose = FALSE)
}

.onAttach <- function(...) {
  packageStartupMessage("Loaded package fonts. See `?load_pkg_fonts` for details.")
}
