
<!-- README.md is generated from README.Rmd. Please edit that file -->

# penngradlings - Themes and utilities for Penn GradLingS

<!-- badges: start -->

[![R-CMD-check](https://github.com/yjunechoe/penngradlings/workflows/R-CMD-check/badge.svg)](https://github.com/yjunechoe/penngradlings/actions)
<!-- badges: end -->

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("yjunechoe/penngradlings")
```

## Example

``` r
library(penngradlings)
#> Package fonts loaded. See `?load_pkg_fonts` for details.
```

``` r
library(ggplot2)

ggplot(diamonds, aes(cut, price, fill = cut)) +
  stat_summary(geom = "bar", fun = "mean", width = 0.8) +
  scale_y_continuous(
    expand = expansion(c(0, 0.1)),
    labels = ~ paste0("$", .x)
  ) +
  scale_fill_manual(values = pgl_pals("christine", "blueberry_boba")) +
  labs(
    title = "Price of diamonds by cut quality",
    subtitle = "This is a bad explanatory plot because carat (size) is a confounding variable",
    x = NULL, y = NULL,
    caption = "diamonds dataset from {ggplot2}"
  ) +
  theme_pgl_minimal(axis_lines = "x", grid_lines = "y")
```

<img src="man/figures/README-theme-minimal-1.jpeg" width="100%" />

More on the [package
website](https://yjunechoe.github.io/penngradlings).
