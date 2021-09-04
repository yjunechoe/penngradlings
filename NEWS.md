# penngradlings (development version)

- Color palettes in `pgl_pals()`

# penngradlings 0.1.0

## New features

### Fonts

- The package packs 8 open source font families: _Atkinson Hyperlegible_, _Bootstrap Icons_, _Charis SIL_, _Font Awesome 5 Free_, _Inter_, _Material Icons_, _Piazzolla_, _Piazzolla SC_. Fonts are automatically registered on package load. See `.pgl_fonts()` for details.
- A `font_*()` family of helper functions for inspecting, registering, and shaping fonts - `font_hoist()`, `font_pluck()`, `font_info()`, `font_exists()`, and `font_path()`.

### Themes

- `theme_pgl_basic()`, a low-ink theme design built on top of `ggplot2::theme_minimal()`, with options for turning on/off axis and grid lines.

### Layers

- Stylized panel annotations with `geom_text_outline()` and `annotation_textbox()`.

### Misc

- `ggsave_auto()`, a wrapper around `ggplot2::ggsave()` offering convenience for interactive plotting. Accumulates all plots rendered with the function in a separate _./ggsave_auto_ directory and opens the saved figure back using the system's default photo-viewing app. Includes option for reading the saved figure back as a {magick} object.
