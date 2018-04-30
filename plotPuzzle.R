# plotPuzzle
#
# Make a nice, ggplot2-based drawing of a sudoku grid.

require(tidyverse)

plotPuzzle <- function(Puzzle, resolution = 72) {
  g <- ggplot() +
       scale_x_continuous(breaks = seq(0, 9, 3),
                          minor_breaks = seq(1, 8, 1),
                          labels = NULL,
                          expand = c(0, 0)) +
       scale_y_continuous(breaks = seq(0, 9, 3),
                          minor_breaks = seq(1, 8, 1),
                          labels = NULL,
                          expand = c(0, 0)) +
       coord_equal() +
       annotate("rect", xmin = 0, xmax = 9, ymin = 0, ymax = 9, alpha = 0)

  for (i in 1:9) {
    for (j in 1:9) {
      if (Puzzle[i,j] != 0) {
        g <- g + annotate("text",
                          x = 0.5 + (i - 1),
                          y = 8.5 - (j - 1),
                          label = format(Puzzle[i,j]))
      }
    }
  }
  g <- g + theme_bw() +
           theme(axis.ticks = element_line(size = 0),
           panel.grid.major = element_line(size = 0.75, colour = "black"),
           panel.grid.minor = element_line(size = 0.5, color = "gray50"),
           panel.border = element_rect(size = 1, color = "black")) +
           labs( x = "", y = "", title = "Rukodus V1.0")

  return(g)
}
