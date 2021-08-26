# Author: Komal S. Rathi
# Date: 11/11/2019
# Function: publication quality ggplot2 themes

# ggplot2_publication_theme.R defines a function ggplot2_publication_theme to
# return a ggplot2 theme.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls source("src/ggplot2_publication_theme.R")
#
# Defined variables:
#
# - ggplot2_publication_theme


ggplot2_publication_theme <- function(base_size = 12,
                                      base_family = "Helvetica") {
  res_theme <- ggthemes::theme_foundation(base_size = base_size,
                                          base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0.5),
      text = ggplot2::element_text(),
      panel.background = ggplot2::element_rect(colour = NA),
      plot.background = ggplot2::element_rect(colour = NA),
      panel.border = ggplot2::element_rect(colour = NA),
      axis.title = ggplot2::element_text(size = ggplot2::rel(1)),
      axis.title.y = ggplot2::element_text(angle = 90, vjust = 2),
      axis.title.x = ggplot2::element_text(vjust = -0.2),
      axis.text = ggplot2::element_text(),
      axis.line = ggplot2::element_line(colour = "black"),
      axis.ticks = ggplot2::element_line(),
      panel.grid.major = ggplot2::element_line(colour = "#f0f0f0"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = grid::unit(c(10, 5, 5, 5), "mm"),
      strip.background = ggplot2::element_rect(
        colour = "#f0f0f0", fill = "#f0f0f0"),
      strip.text = ggplot2::element_text(face = "bold"))
}
