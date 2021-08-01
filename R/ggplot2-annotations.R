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
           "top-right" = list (hjust = 1, vjust = 1, x = 1, y = 1),
           "bottom-left" = list(hjust = 0, vjust = 0, x = 0, y = 0),
           "bottom-right" = list (hjust = 1, vjust = 0, x = 1, y = 0),
           "center" = list(hjust = 0.5, vjust = 0.5, x = 0.5, y = 0.5),
           cli::cli_abort("Invalid {.arg position} value"))
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
                           gp = grid::gpar(fontsize = 10, lineheight = 1.4, fontfamily = "Inter-Medium"),
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
