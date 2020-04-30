#' @title theme for ggplot
#'
#' @description Theme inspired in the Adnres publication style.
#'
#' @param base_size letter size.
#' @param base_family leter type.
#' @param x.text.angle Inclination of X labels.
#' @param border Border of the box.
#' @param legend Location of the legend with a value "top", "bottom", "left", "right", "none".
#' @param spacing Sparation between facets in case there are.
#' @param margin Add space between multi-facetted plots.
#' @author Santiago Caño-Muñiz
#' @export

theme_Publication <- function(base_size = 14, base_family = "Arial", x.text.angle = 0, border = FALSE,
                              margin = TRUE, legend = c("top", "bottom", "left", "right", "none"), spacing = 0.5) {
  half_line <- base_size/2
  if (!is.numeric(legend))
    legend <- match.arg(legend)
  if (x.text.angle > 5)
    xhjust <- 1
  else xhjust <- NULL
  if (border) {
    panel.border <- ggplot2::element_rect(fill = NA, colour = "black",
                                 size = 0.7)
    axis.line <- ggplot2::element_blank()
  }
  else {
    panel.border <- ggplot2::element_blank()
    axis.line = ggplot2::element_line(colour = "black", size = 0.5)
  }
  if (margin)
    plot.margin <- ggplot2::margin(half_line, half_line, half_line,
                          half_line)
  else plot.margin <- ggplot2::unit(c(0.5, 0.3, 0.3, 0.3), "mm")
  .theme <- ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
    ggplot2::theme(plot.title = element_text(face = "bold",
                                             size = ggplot2::rel(1.2), hjust = 0.5),
                   text = element_text(),
                   panel.background = ggplot2::element_rect(colour = NA),
                   plot.background = ggplot2::element_rect(colour = NA),
                   panel.border = panel.border,
                   axis.title = ggplot2::element_text(face = "bold",size = ggplot2::rel(1)),
                   axis.title.y = ggplot2::element_text(angle=90,vjust =2),
                   axis.title.x = ggplot2::element_text(vjust = -0.2),
                   axis.text = ggplot2::element_text(),
                   axis.line.x = ggplot2::element_line(colour = "black", size = 1),
                   axis.line.y = ggplot2::element_line(colour = "black", size = 1),
                   axis.ticks = ggplot2::element_line(size = 1),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.key = ggplot2::element_rect(colour = NA),
                   legend.position = legend,
                   legend.key.size= unit(0.2, "cm"),
                   panel.spacing  = unit(spacing, "cm"),
                   legend.title = ggplot2::element_text(face="italic"),
                   plot.margin = plot.margin,
                   strip.background = ggplot2::element_rect(colour = NA, fill = NA),
                   strip.text = ggplot2::element_text(face="bold")
    )
  if (x.text.angle != 0)
    .theme <- .theme + ggplot2::theme(axis.text.x = element_text(angle = x.text.angle,
                                                                 hjust = xhjust))
  .theme

}
