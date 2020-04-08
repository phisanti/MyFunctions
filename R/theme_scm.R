#' @title Stylish theme for ggplot
#' @description Helper function to create like-publication graphs using the ggplot environment
#' @param base_size letter size
#' @param base_family leter type
#' @param x.text.angle Inclination of X labels
#' @param border Border of the box
#' @param legend Location of the legend with a value "top", "bottom", "left", "right", "none"
#' @param spacing Sparation between facets in case there are
#'
#' @author Santiago Caño-Muñiz
#' @export

theme_scm <- function(base_size = 15, base_family = "Calibri", x.text.angle = 0, border = FALSE,
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
  else plot.margin <- unit(c(0.5, 0.3, 0.3, 0.3), "mm")
  .theme <- ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
    ggplot2::theme(plot.title = element_text(face = "bold",
                                             size = rel(1.2), hjust = 0.5),
                   text = element_text(),
                   panel.background = element_rect(colour = NA),
                   plot.background = element_rect(colour = NA),
                   panel.border = panel.border,
                   axis.title = element_text(face = "bold",size = rel(1)),
                   axis.title.y = element_text(angle=90,vjust =2),
                   axis.title.x = element_text(vjust = -0.2),
                   axis.text = element_text(),
                   axis.line.x = element_line(colour = "black", size = .5),
                   axis.line.y = element_line(colour = "black", size = .5),
                   axis.ticks = element_line(),
                   panel.grid.major = element_line(colour="#f0f0f0"),
                   panel.grid.minor = element_blank(),
                   legend.key = element_rect(colour = NA),
                   legend.position = legend,
                   legend.key.size= unit(0.2, "cm"),
                   panel.spacing  = unit(spacing, "cm"),
                   legend.title = element_text(face="italic"),
                   plot.margin = plot.margin,
                   strip.background=element_rect(colour=NA,fill=NA),
                   strip.text = element_text(face="bold")
    )
  if (x.text.angle != 0)
    .theme <- .theme + theme(axis.text.x = element_text(angle = x.text.angle,
                                                        hjust = xhjust))
  .theme

}