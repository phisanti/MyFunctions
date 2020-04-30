#' @title Linear model equation
#' @description Returns the equation of a linear model as "y = b*x + a". The function also accepts lm objects.
#' @param y Dependent variable
#' @param x Independent variable
#' @param m Independent variable
#' @return a character expression in the form "y = b*x + a"
#' @author Santiago Caño-Muñiz
#'
#' @importFrom stats coef
#' @importFrom stats lm
#' @export

lm_eq <- function(x, y, m = NULL){
  if (is.null(m)) {m <- lm(y ~ x)}
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(coef(m)[1], digits = 2),
                        b = format(coef(m)[2], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq))

}

