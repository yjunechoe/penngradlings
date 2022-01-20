# penngradlings (development version)

# penngradlings 0.2.2

### New functions

- `fct_derive()` to derive levels from another factor with unique pair-wise matches

### Enhancements

- `read_pcibex()` gains an `encoding` argument for specifying the file's character encoding (#18)
- `theme_pgl_minimal()` has been re-written to scale better, default value for `base_size` changed to 12

# penngradlings 0.2.1

### New functions

- `raggsave_auto()` to save any arbitrary plot with `ragg::agg_png()`

### Enhancements

- `read_pcibex()` gains an `encoding` argument for specifying the file's character encoding. 
- `ggsave_auto()` supports auto rendering of grobs

# penngradlings 0.2.0

### New functions

- `scale_color/fill_pgl_discrete()`

### Enhancements

- Overhaul of `pgl_pals()`. It's now a function factory

# penngradlings 0.1.1

### New functions

- `name_self()` list-constructor function, mostly to help with naming multiple columns when passing a list of functions to `dplyr::across()`.

- `read_pcibex()` for reading in the results from a PCIbex experiment. 

### Enhancements

- You can now set global options for `ggsave_auto()` with `options(ggsave_auto.*)`. The package now also comes with an RStudio addin for calling `ggsave_auto()` (one possible keyboard binding is Ctrl/Cmd+Shift+G, which overwrites the "Find Previous" shortcut).

- `theme_pgl_minimal()` gets `strip.placement = outside` to hack margins into the space between facet and panel with a blank secondary axis.

# penngradlings 0.1.0

First release.

### New functions

#### Fonts

- The package packs 8 open source font families: _Atkinson Hyperlegible_, _Bootstrap Icons_, _Charis SIL_, _Font Awesome 5 Free_, _Inter_, _Material Icons_, _Piazzolla_, _Piazzolla SC_. Fonts are automatically registered on package load. See `.pgl_fonts()` for details.
- A `font_*()` family of helper functions for inspecting, registering, and shaping fonts - `font_hoist()`, `font_pluck()`, `font_info()`, `font_exists()`, and `font_path()`.

#### Themes

- `theme_pgl_basic()`, a low-ink theme design built on top of `ggplot2::theme_minimal()`, with options for turning on/off axis and grid lines.

#### Layers

- Stylized panel annotations with `geom_text_outline()` and `annotation_textbox()`.

#### Save/render

- `ggsave_auto()`, a wrapper around `ggplot2::ggsave()` offering convenience for interactive plotting. Accumulates all plots rendered with the function in a separate _./ggsave_auto_ directory and opens the saved figure back using the system's default photo-viewing app. Includes option for reading the saved figure back as a {magick} object.
