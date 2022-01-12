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
#'   facet_wrap(~Species) +
#'   theme_pgl_minimal(base_size = 11)
#'
#' ggplot(diamonds, aes(cut, price, fill = cut)) +
#'   stat_summary(geom = "bar", fun = "mean", width = 0.8) +
#'   scale_y_continuous(
#'     expand = expansion(c(0, 0.1)),
#'     labels = ~ paste0("$", .x)
#'   ) +
#'   scale_fill_manual(values = pgl_pals("christine", "blueberry_boba")) +
#'   labs(
#'     title = "Price of diamonds by cut quality",
#'     subtitle = "This is a bad explanatory plot because carat (size) is a confounding variable",
#'     x = NULL, y = NULL,
#'     caption = "diamonds dataset from {ggplot2}"
#'   ) +
#'   theme_pgl_minimal(axis_lines = "x", grid_lines = "y")
#' }
theme_pgl_minimal <- function(base_size = 12,
                              axis_lines = "",
                              grid_lines = "xy",
                              grid_lines_minor = FALSE,
                              outline_color = "#1F1C1C") {

  faded_outline_color <- colorspace::lighten(outline_color, 0.35)
  faded_text_color <- colorspace::lighten(outline_color, 0.15)

  pad_in_sm <- base_size/200
  pad_in_md <- base_size/100
  pad_in_lg <- base_size/50

  .theme <- ggplot2::theme_minimal(
    base_size = base_size,
    base_family = "Inter-Medium"
  ) +
    theme(
      rect = element_rect(color = faded_outline_color),
      line = element_line(color = faded_outline_color),
      text = element_text(color = outline_color, family = "Inter-Medium"),
      title = element_text(color = faded_text_color, family = "Inter-Bold"),
      plot.title = element_text(size = rel(1.2), family = "Inter-ExtraBold", margin = margin(b = pad_in_md, unit = "in")),
      plot.background = element_rect(color = NA, fill = "white"),
      plot.subtitle = element_text(color = faded_outline_color, margin = margin(t = 0, b = pad_in_lg, unit = "in")),
      plot.caption = element_text(color = faded_text_color, family = "Inter-Regular", margin = margin(t = pad_in_md, unit = "in")),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.tag = element_text(size = 12, family = "Inter-Medium", margin = margin(0, pad_in_sm, pad_in_sm, 0, unit = "in")),
      plot.margin = margin(pad_in_md, pad_in_md, pad_in_md, pad_in_md, "in"),
      axis.text = element_text(family = "Inter-SemiBold"),
      axis.text.x = element_text(size = rel(1)),
      axis.text.y = element_text(size = rel(0.9)),
      axis.text.x.bottom = element_text(margin = margin(t = pad_in_sm, unit = "in")),
      axis.text.x.top = element_text(margin = margin(b = pad_in_sm, unit = "in")),
      axis.text.y.left = element_text(margin = margin(r = pad_in_sm, unit = "in")),
      axis.text.y.right = element_text(margin = margin(l = pad_in_sm, unit = "in")),
      axis.title.x.bottom = element_text(margin = margin(t = pad_in_sm, unit = "in")),
      axis.title.x.top = element_text(margin = margin(b = pad_in_sm, unit = "in")),
      axis.title.y.left = element_text(margin = margin(r = pad_in_sm, unit = "in")),
      axis.title.y.right = element_text(margin = margin(l = pad_in_sm, unit = "in")),
      panel.grid = element_line(color = "#F2F2F2"),
      panel.spacing = grid::unit(pad_in_sm, "in"),
      legend.title = element_text(family = "Inter-SemiBold"),
      legend.text = element_text(color = faded_text_color, size = rel(0.8), family = "Inter-SemiBold"),
      legend.key = element_blank(),
      strip.placement = "outside",
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
