.onLoad <- function(...) {
  load_pkg_fonts(verbose = FALSE)
}

.onAttach <- function(...) {
  packageStartupMessage("Package fonts loaded. See `?load_pkg_fonts` for details.")
}
