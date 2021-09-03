#' Basic {penngradlings} theme template
#'
#' @param base_size Base size for text elements
#' @param axis_lines Which axis lines should be drawn, passed as a string.
#' @param grid_lines Which grid lines should be drawn, passed as a string.
#' @param grid_lines_minor Whether minor grid lines should be draw. Defaults to `FALSE`
#'
#' @return A theme object created by `ggplot2::theme()`
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) +
#'   geom_point(size = 2) +
#'   labs(
#'     title = "Petal profile of plant species in the iris dataset",
#'     subtitle = "Virginica has the largest petal of the three species"
#'   ) +
#'   facet_wrap(~ Species) +
#'   theme_plg_basic()
#' }
theme_plg_basic <- function(base_size = 12, axis_lines = "", grid_lines = "xy", grid_lines_minor = FALSE) {
  outline_color <- "#1F1C1C"
  faded_outline_color <- colorspace::lighten(outline_color, 0.35)

  .theme <- ggplot2::theme_minimal(
    base_size = base_size,
    base_family = "Inter-Medium"
  ) +
    ggplot2::theme(
      rect = ggplot2::element_rect(color = faded_outline_color),
      line = ggplot2::element_line(color = faded_outline_color),
      text = ggplot2::element_text(color = outline_color, family = "Inter-Medium"),
      title = ggplot2::element_text(family = "Inter-Bold"),
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.4), family = "Inter-ExtraBold", margin = ggplot2::margin(b = 0.5, unit = "npc")),
      axis.title.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = 0.5, unit = "npc")),
      axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(b = 0.5, unit = "npc")),
      axis.title.y.left = ggplot2::element_text(margin = ggplot2::margin(r = 0.5, unit = "npc")),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 0.5, unit = "npc")),
      plot.subtitle = ggplot2::element_text(color = faded_outline_color, margin = ggplot2::margin(t = 0.1, b = 0.8, unit = "npc")),
      plot.caption = ggplot2::element_text(color = faded_outline_color, family = "Inter-Medium"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(0.03, 0.02, 0.03, 0.02, "npc"),
      panel.grid = ggplot2::element_line(color = "#F2F2F2"),
      panel.spacing = grid::unit(0.015, "npc"),
      legend.title = ggplot2::element_text(family = "Inter-SemiBold"),
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(),
      strip.text = ggplot2::element_text(size = ggplot2::rel(0.85), family = "Inter-SemiBold")
    )

  if (stringr::str_length(axis_lines) <= 2 && grepl("[xyXY]{1,2}", axis_lines)) {
    if (stringr::str_detect(axis_lines, "(x|X)")) {
      .theme <- .theme +
        ggplot2::theme(axis.line.x = ggplot2::element_line())
    }
    if (stringr::str_detect(axis_lines, "(y|Y)")) {
      .theme <- .theme +
        ggplot2::theme(axis.line.y = ggplot2::element_line())
    }
  }

  if (stringr::str_length(grid_lines) <= 2 && grepl("[xyXY]{0,2}", grid_lines)) {
    if (!stringr::str_detect(grid_lines, "(x|X)")) {
      .theme <- .theme +
        ggplot2::theme(
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank()
        )
    }
    if (!stringr::str_detect(grid_lines, "(y|Y)")) {
      .theme <- .theme +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank()
        )
    }
  }

  if (!grid_lines_minor) {
    .theme <- .theme +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  }

  .theme
}
