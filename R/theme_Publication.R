#' @title theme for ggplot
#' @description The theme_Publication generates like-publication graphs using the ggplot environment
#' @param base_size letter size
#' @param base_family leter type
#'
#' @author Santiago Caño-Muñiz
#' @export

theme_Publication <- function(base_size=14, base_family="Calibri") {
  (ggthemes::theme_foundation(base_size=base_size, base_family=base_family) +
     ggplot2::theme(plot.title = element_text(face = "bold",
                                              size = rel(1.2), hjust = 0.5),
                    text = element_text(),
                    panel.background = element_rect(colour = NA),
                    plot.background = element_rect(colour = NA),
                    panel.border = element_rect(colour = NA),
                    axis.title = element_text(face = "bold",size = rel(1)),
                    axis.title.y = element_text(angle=90,vjust =2),
                    axis.title.x = element_text(vjust = -0.2),
                    axis.text = element_text(),
                    axis.line.x = element_line(colour = "black", size = .5),
                    axis.line.y = element_line(colour = "black", size = .5),            axis.ticks = element_line(),
                    panel.grid.major = element_line(colour="#f0f0f0"),
                    panel.grid.minor = element_blank(),
                    legend.key = element_rect(colour = NA),
                    legend.position = "bottom",
                    legend.direction = "horizontal",
                    legend.key.size= unit(0.2, "cm"),
                    panel.spacing  = unit(1, "cm"),
                    legend.title = element_text(face="italic"),
                    plot.margin=unit(c(10,5,5,5),"mm"),
                    strip.background=element_rect(colour=NA,fill=NA),
                    strip.text = element_text(face="bold")
     ))

}
