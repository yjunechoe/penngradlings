g <- gridtext::textbox_grob(
  "**The quick brown fox jumps over the lazy dog.**<br><br>
    The quick brown fox jumps over the lazy dog.
    The **quick <span style='color:brown;'>brown fox</span>** jumps over the lazy dog.
    The quick brown fox jumps over the lazy dog.",
  x = 0, y = 1,
  gp = grid::gpar(fontsize = 12), hjust = 0, vjust = 1, halign=0,
  box_gp = grid::gpar(col = NA, fill = "lightcyan1"),
  padding = grid::unit(c(0.02, 0.02, 0.02, 0.05), "npc")
)
grid.newpage()
grid.draw(g)

el <- grid::linesGrob(x = 0.5, y = c(0, 1), gp = grid::gpar(lwd=1))
grid.newpage()
grid.draw(el)

x <- grid::gList(g, el)

grid.newpage()
grid.draw(x)
