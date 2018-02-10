#' @title Linear model equations
#' @description Plot the equation of simple linear models generated with lm.
#' @param base_size letter size
#' @param base_family leter type
#'
#' @author Santiago Caño-Muñiz
#' @export


#plot regression equation

ggplotRegression <- function (m) {
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(coef(m)[1], digits = 2),
                        b = format(coef(m)[2], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq))
  ggplot2::ggplot(m$model, aes_string(x = names(m$model)[2], y = names(m$model)[1])) +
    ggplot2::geom_point() +
    ggplot2::stat_smooth(method = "lm", col = "red") +
    ggplot2::labs(title = eq)
}

