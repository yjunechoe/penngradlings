#' Return npc-based positional values for common alignments
#'
#' @param position One of "top-left", "top-right", "bottom-left", "bottom-right", "center".
#'
#' @return A list of \code{hjust}, \code{vjust}, \code{x}, and \code{y}
#'
compute_position <- function(position) {
  if (!is.character(position)) {
    cli::cli_abort("Invalid {.arg position} value")
  } else {
    switch(position,
      "top-left" = list(hjust = 0, vjust = 1, x = 0, y = 1),
      "top-right" = list(hjust = 1, vjust = 1, x = 1, y = 1),
      "bottom-left" = list(hjust = 0, vjust = 0, x = 0, y = 0),
      "bottom-right" = list(hjust = 1, vjust = 0, x = 1, y = 0),
      "center" = list(hjust = 0.5, vjust = 0.5, x = 0.5, y = 0.5),
      cli::cli_abort("Invalid {.arg position} value")
    )
  }
}


#' HTML textbox annotation
#'
#' A wrapper around \code{gridtext::textbox_grob} with convenient positioning and themed defaults.
#'
#' To be used for non-data-based annotations with \code{ggplot2::annotation_custom}.
#'
#' @param html Content of the textbox in html syn %>% tax. See supported tags in \url{https://wilkelab.org/ggtext/index.html}.
#' @param position One of "top-left", "top-right", "bottom-left", "bottom-right", "center". Calculated with \code{compute_position}.
#' @param width A numeric value for the npc or a \code{grid::unit} object. Defaults to fit the contents passed to \code{html}.
#' @param height A numeric value for the npc or a \code{grid::unit} object. Defaults to fit the contents passed to \code{html}.
#' @param x_offset x-position offset relative to the panel space.
#' @param y_offset y-position offset relative to the panel space.
#' @param gp Graphical parameters for the text as \code{grid::gpar}.
#' @param box_gp Graphical parameters for the bounding box as \code{grid::gpar}.
#' @param padding Spacing for the inner-space between the box boundaries and the box contents, supplied as \code{grid::unit}.
#' @param margin Spacing for the outer-space between the box boundaries and its position, supplied as \code{grid::unit}.
#' @param r Radius of the box corners, supplied as \code{grid::unit}.
#' @param orientation Orientation of the box. One of "upright", "left-rotated", "right-rotated", "inverted".
#' @param ... Passed to \code{gridtext::textbox_grob}
#'
#' @return A \code{gridtext::textbox_grob}.
#' @export
#'
#' @examples
#' \dontrun{
#' annotation <- annotation_textbox(
#'   html = "<span style=\"font-size:18px; font-family:'Inter-Black';\">
#'            This is a cat:</span>
#'           <br><br>
#'           <span style=\"font-size:72px; color:'#7950F2';
#'            font-family:'FontAwesome5-Solid';\">cat</span>
#'           <br><br>
#'           Just look at this cat!",
#'   position = "top-right",
#'   halign = 0.5
#' )
#'
#' ggplot2::qplot(mpg, hp, data = mtcars) +
#'   ggplot2::annotation_custom(annotation) +
#'   ggplot2::theme_classic()
#' }
annotation_textbox <- function(html,
                               position = "top-left",
                               width = NULL,
                               height = NULL,
                               x_offset = 0,
                               y_offset = 0,
                               gp = grid::gpar(fontsize = 10, lineheight = 1.4, fontfamily = "Piazzolla-Regular"),
                               box_gp = grid::gpar(col = "#0E151B", fill = "#e5e7eb", lwd = grid::unit(1.5, "pt")),
                               padding = grid::unit(rep(10, 4), "pt"),
                               margin,
                               r = grid::unit(5, "pt"),
                               orientation = "upright",
                               ...) {
  position <- compute_position(position)

  if (rlang::is_missing(margin)) {
    margin <- grid::unit(rep(box_gp$lwd, 4), "pt")
  }

  gridtext::textbox_grob(
    text = html,
    x = position$x + x_offset,
    y = position$y + y_offset,
    hjust = position$hjust,
    vjust = position$vjust,
    width = width,
    height = height,
    gp = gp,
    box_gp = box_gp,
    padding = padding,
    margin = margin,
    r = r,
    orientation = orientation,
    ...
  )
}

#' Create an HTML span of the unicode zero-width character
#'
#' Helper function for adjusting paragraph margins for use with {ggtext}/{gridtext},
#' which does not natively support margin/padding styles.
#'
#' The zero-width unicode character only ever takes up vertical space, so when placed in-line
#' its size determines the amount of "top-margins" for the paragraph. This allows finer control
#' over spacing.
#'
#' @param size Size of the zero-width character
#' @param units Defaults to \code{"px"}
#'
#' @return A string
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggtext)
#'
#' ggplot() +
#'   geom_richtext(aes(0, 0, label = "This is a line<br><br>This is a paragraph<br>of text."))
#'
#' ggplot() +
#'   geom_richtext(aes(0, 0, label = paste0(
#'     "This is a line<br>",
#'     zerowidth_char(30),
#'     "This is a paragraph<br>of text."
#'   )))
#' }
zerowidth_char <- function(size = 12, units = "px") {
  paste0("<span style='font-size:", size, units, "'>&#8203;</span>")
}

#' Outlined text layer
#'
#' Mimicking SVG-style "stroke" styles, this function uses 1-2 \code{ggfx::with_outer_glow} filters
#' to create outlines around text. There are two layers for the following uses: the first layer is designed to
#' create a visible outline that places emphasis on the text. The second layer is designed to add
#' a mask around the outlined text that makes it stand out from the background.
#'
#' This is particularly useful for labels drawn over panels with grid lines and a non-white background color.
#' Outlines will create a stronger perceptual contrast for the labels.
#'
#' @param ... Passed to the geom layer as specified in the \code{geom} argument.
#' @param geom Which geom layer to apply outlines over. Defaults to
#' @param inner_params Passed to the first \code{ggfx::with_outer_glow} filter (inner outline).
#' @param outer_params Passed to the second \code{ggfx::with_outer_glow} filter (outer outline).
#' @param use_outer Whether the outer outline should be turned on. Defaults to \code{FALSE}.
#'
#' @return A {ggplot2} layer if \code{use_outer} is \code{TRUE}, otherwise a list of two layers.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(mpg, disp, label = rownames(mtcars)))
#'
#' # By default creates a white outline (note the "padding" around gridlines)
#' p + geom_text_outline() +
#'   theme_plg_basic()
#'
#' # You can add a second, outer outline
#' # This is useful if the plot has a background color like in the default theme
#' p + geom_text_outline(use_outer = TRUE)
#'
#' # This outer outline is more obvious against a white background
#' p + geom_text_outline(use_outer = TRUE) +
#'   theme_void()
#'
#' # You can stylize the inner and outer outlines with
#' # arguments passed to `ggfx::with_outer_glow()`.
#' # The most relevant are `expand` and `colour`
#' p +
#'   geom_text_outline(
#'     inner_params = list(expand = 8),
#'     outer_params = list(colour = "red"),
#'     use_outer = TRUE
#'   ) +
#'   theme_void()
#'
#' # You can pass arguments to the layer specified in the `geom` argument in the `...`.
#' # The default geom is `geom_text`, so you can pass arguments specific to it
#' p +
#'   geom_text_outline(
#'     vjust = "inward",
#'     hjust = "inward",
#'     check_overlap = TRUE,
#'     angle = 30
#'   )
#'
#' # You can also invert the text outlines
#' p +
#'   geom_text_outline(
#'     size = 5,
#'     color = "white",
#'     inner_params = list(colour = "black", expand = 1.5),
#'     use_outer = TRUE
#'   )
#'
#' # You can pass other geoms to the `geom` argument (doesn't necessarily have to be a text layer)
#' library(ggrepel)
#' p + geom_text_outline(geom = geom_text_repel)
#' }
geom_text_outline <- function(..., geom = "geom_text", inner_params = list(), outer_params = list(), use_outer = FALSE) {
  .id <- stringr::str_flatten(sample(letters, 24, replace = TRUE))
  geom <- rlang::ensym(geom)
  .inner_params <- list(
    x = eval(rlang::call2(geom, ...)),
    colour = "#FFFFFF",
    sigma = 0.01,
    expand = 4,
    id = .id
  )
  .outer_params <- list(
    x = .id,
    colour = "#EBEBEB",
    sigma = 0.01,
    expand = 8
  )
  if (use_outer) {
    list(
      do.call(ggfx::with_outer_glow, utils::modifyList(.inner_params, inner_params)),
      do.call(ggfx::with_outer_glow, utils::modifyList(.outer_params, outer_params))
    )
  } else {
    .inner_params <- c(.inner_params, include = TRUE)
    do.call(ggfx::with_outer_glow, utils::modifyList(.inner_params, inner_params))
  }
}
