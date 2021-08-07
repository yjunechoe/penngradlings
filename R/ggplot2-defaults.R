#' Modify defaults ggplot2 layer aesthetics for {penngradlings}
#'
update_ggplot_defaults <- function() {
  geom_opts <- list(
    "text" = list(
      color = "#585555",
      family = "Inter-Medium"
    ),
    "label" = list(
      color = "#585555",
      fill = NA,
      family = "Inter-Medium",
      label.size = 0,
      label.padding = grid::unit(10, "pt")
    ),
    "point" = list(
      fill = "black",
      pch = 21,
      color = "white",
      stroke = 0.6
    ),
    "hline" = list(
      color = "#625f5e",
      linetype = 2
    ),
    "vline" = list(
      color = "#625f5e",
      linetype = 2
    )
  )

  purrr::walk2(names(geom_opts), geom_opts, ggplot2::update_geom_defaults)
}
