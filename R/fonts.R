#' Get information about a font
#'
#' @param family Font family.
#' @param style Font style. Defaults to \code{"Regular"}.
#'
#' @return A dataframe containing information about a font.
#'   The \code{"source"} attribute indicates where it was found.
#'   If the font doesn't exist, invisibly returns \code{NULL}.
#' @export
font_info <- function(family, style = "Regular") {
  sys_font <- systemfonts::system_fonts() %>%
    dplyr::filter(
      .data$family == .env$family,
      .data$style == .env$style
    )
  reg_font <- systemfonts::registry_fonts() %>%
    dplyr::filter(
      .data$family == .env$family,
      .data$style == .env$style
    )
  if (nrow(sys_font) == 0) {
    if (nrow(reg_font) == 0) {
      cli::cli_text("Font {.val {family}} does not exist!")
      invisible(NULL)
    } else {
      attr(reg_font, "source") <- "registry"
      reg_font
    }
  } else {
    attr(sys_font, "source") <- "system"
    sys_font
  }
}

#' Check whether a font is registered
#'
#' Wrapper around \code{font_info} that tests whether the output is \code{NULL}.
#'
#' @param family Font family.
#' @param style Font style. Defaults to \code{"Regular"}.
#'
#' @return Boolean
#' @export
#'
#' @examples
#' \dontrun{
#' font_exists(family = "Arial")
#' }
font_exists <- function(family, style = "Regular") {
  !is.null(suppressMessages(font_info(family, style)))
}

#' Find path to a system font
#'
#' Wrapper around \code{font_info} that pulls the path of the font if it exists.
#'
#' @param family Font family.
#' @param style Font style. Defaults to \code{"Regular"}.
#'   If the font doesn't exist, invisibly returns \code{NULL}.
#'
#' @return String of the path to the font file.
#' @export
font_path <- function(family, style = "Regular") {
  font_info <- suppressMessages(font_info(family, style))
  if (!is.null(font_info)) {
    font_info$path
  } else {
    cli::cli_text("Font {.val {family}} does not exist!")
    invisible(NULL)
  }
}

#' Register members of a font family as their own families
#'
#' @param family The name of the font family, as registered in \code{systemfonts::system_fonts()}
#' @param verbose Whether to print a message of the newly registered font families
#'
#' @export
#'
#' @examples
#' \dontrun{
#' font_hoist(family = "Arial")
#' }
#'
#' @importFrom rlang .data .env
font_hoist <- function(family, verbose = TRUE) {
  font_specs <- systemfonts::system_fonts() %>%
    dplyr::filter(.data$family == .env$family) %>%
    dplyr::mutate(family = paste(.data$family, .data$style)) %>%
    dplyr::select(plain = .data$path, name = .data$family)

  purrr::pwalk(as.list(font_specs), systemfonts::register_font)

  if (verbose) {
    cli::cli({
      cli::cli_alert_success("Hoisted {.val {nrow(font_specs)}} variants for {.val {family}}")
      cli::cli_ul(font_specs$name)
    })
  }
}

#' Register a font variant with OpenType feature specifications
#'
#' @param family A font family. The Regular member of the family is used as the base.
#' @param features Character vector of 4-letter OpenType feature tags.
#' See [complete list of OpenType features](https://docs.microsoft.com/en-us/typography/opentype/spec/featurelist).
#' Unavailable font features are ignored.
#' @param .name (Optional) Name of the new font family. Appends registered feature tags to the new family name by default.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' font_pluck(family = "Alegreya", c("lnum", "ordn"))
#' }
#'
#' @importFrom rlang .data .env
font_pluck <- function(family, features, .name) {
  font_feature_tbl <- c(
    "liga" = "standard",
    "hlig" = "historical",
    "clig" = "contextual",
    "dlig" = "discretionary",
    "cswh" = "swash",
    "calt" = "alternates",
    "hist" = "historical",
    "locl" = "localized",
    "rand" = "randomize",
    "nalt" = "alt_annotation",
    "salt" = "stylistic",
    "subs" = "subscript",
    "sups" = "superscript",
    "titl" = "titling",
    "smcp" = "small_caps",
    "lnum" = "lining",
    "onum" = "oldstyle",
    "pnum" = "proportional",
    "tnum" = "tabular",
    "frac" = "fractions",
    "afrc" = "fractions_alt"
  )

  family_path <- systemfonts::system_fonts() %>%
    dplyr::filter(.data$family == .env$family, .data$style == "Regular") %>%
    dplyr::pull(.data$path)
  if (!rlang::has_length(family_path)) {
    cli::cli_abort("Font family {.val {family}} not found in systemfonts::system_fonts().")
  }

  valid_features <- features[stringr::str_length(features) == 4]
  valid_features <- valid_features[valid_features %in% textshaping::get_font_features(family)[[1]]]
  types <- dplyr::case_when(
    valid_features %in% c("liga", "hlig", "clig", "dlig") ~ "ligatures",
    valid_features %in% c("cswh", "calt", "hist", "locl", "rand", "nalt", "salt", "subs", "sups", "titl", "smcp") ~ "letters",
    valid_features %in% c("lnum", "onum", "pnum", "lnum", "frac", "afrc") ~ "numbers",
    TRUE ~ valid_features
  )
  if (any(duplicated(types))) {
    cli::cli_abort("Multiple specifications for one of: {.arg Ligatures}, {.arg Letters}, {.arg Numbers}.")
  }
  if (rlang::is_missing(.name)) {
    .name <- paste(family, paste(sort(valid_features), collapse = "-"))
  }
  valid_features <- as.list(stats::setNames(valid_features, types))
  for (i in seq_along(valid_features)) {
    if (names(valid_features)[[i]] %in% c("ligatures", "letters", "numbers")) {
      valid_features[[i]] <- unname(font_feature_tbl[valid_features[[i]]])
    } else {
      valid_features[[i]] <- TRUE
    }
  }
  systemfonts::register_font(
    name = .name,
    plain = family_path,
    features = do.call(systemfonts::font_feature, valid_features)
  )
  cli::cli_alert_success("Registered {.val {.name}}.\nCheck systemfonts::registry_fonts() for more details.")
}
