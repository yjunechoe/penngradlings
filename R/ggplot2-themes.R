#' Basic {penngradlings} theme template
#'
#' @param base_size Base size for text elements
#' @param axis_lines Which axis lines should be drawn, passed as a string.
#' @param grid_lines Which grid lines should be drawn, passed as a string.
#' @param grid_lines_minor Whether minor grid lines should be draw. Defaults to `FALSE`
#' @param outline_color Primary color for text, line, and rectangle outline.
#'
#' @return A theme object created by `theme()`
#'
#' @importFrom ggplot2 theme element_text element_line element_rect element_blank margin rel
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) +
#'   geom_point(size = 2) +
#'   labs(
#'     title = "Petal profile of plant species in the iris dataset",
#'     subtitle = "Virginica has the largest petal of the three species"
#'   ) +
#'   facet_wrap(~ Species) +
#'   theme_plg_basic(base_size = 11)
#'
#' ggplot(diamonds, aes(cut, price)) +
#'   stat_summary(geom = "bar", fun = "mean", width = 0.8) +
#'   scale_y_continuous(
#'     expand = expansion(c(0, 0.1)),
#'     labels = ~ paste0("$", .x)
#'   ) +
#'   labs(
#'     title = "Price of diamonds by cut quality",
#'     subtitle = "This is a bad explanatory plot because carat (size) is a confounding variable",
#'     x = NULL, y = NULL,
#'     caption = "diamonds dataset from {ggplot2}"
#'   ) +
#'   theme_plg_basic(axis_lines = "x", grid_lines = "y")
#' }
theme_plg_basic <- function(base_size = 10,
                            axis_lines = "",
                            grid_lines = "xy",
                            grid_lines_minor = FALSE,
                            outline_color = "#1F1C1C") {

  faded_outline_color <- colorspace::lighten(outline_color, 0.35)

  .theme <- ggplot2::theme_minimal(
    base_size = base_size,
    base_family = "Inter-Medium"
  ) +
    theme(
      rect = element_rect(color = faded_outline_color),
      line = element_line(color = faded_outline_color),
      text = element_text(color = outline_color, family = "Inter-Medium"),
      title = element_text(family = "Inter-Bold"),
      plot.title = element_text(size = rel(1.4), family = "Inter-ExtraBold", margin = margin(b = 0.5, unit = "npc")),
      axis.title.x.bottom = element_text(margin = margin(t = 0.5, unit = "npc")),
      axis.title.x.top = element_text(margin = margin(b = 0.5, unit = "npc")),
      axis.title.y.left = element_text(margin = margin(r = 0.5, unit = "npc")),
      axis.title.y.right = element_text(margin = margin(l = 0.5, unit = "npc")),
      plot.subtitle = element_text(color = faded_outline_color, margin = margin(t = 0.1, b = 0.8, unit = "npc")),
      plot.caption = element_text(color = faded_outline_color, family = "Inter-Medium"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = margin(0.03, 0.02, 0.03, 0.02, "npc"),
      panel.grid = element_line(color = "#F2F2F2"),
      panel.spacing = grid::unit(0.015, "npc"),
      legend.title = element_text(family = "Inter-SemiBold"),
      legend.key = element_blank(),
      strip.background = element_rect(),
      strip.text = element_text(size = rel(0.85), family = "Inter-SemiBold")
    )

  if (nchar(axis_lines) <= 2 && grepl("[xyXY]{1,2}", axis_lines)) {
    if (grepl("(x|X)", axis_lines)) {
      .theme <- .theme +
        theme(axis.line.x = element_line())
    }
    if (grepl("(y|Y)", axis_lines)) {
      .theme <- .theme +
        theme(axis.line.y = element_line())
    }
  }

  if (nchar(grid_lines) <= 2 && grepl("[xyXY]{0,2}", grid_lines)) {
    if (!grepl("(x|X)", grid_lines)) {
      .theme <- .theme +
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
        )
    }
    if (!grepl("(y|Y)", grid_lines)) {
      .theme <- .theme +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()
        )
    }
  }

  if (!grid_lines_minor) {
    .theme <- .theme +
      theme(panel.grid.minor = element_blank())
  }

  .theme
}
