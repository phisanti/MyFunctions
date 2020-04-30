#' @title Geometric mean
#'
#' @description Generic function to calculate the geometric mean of a vector. The geometric mean is defined as the nth root of the product of n numbers:
#' as described: \deqn{\prod_(i\ =\ 1)^n\ofx_i\ =\ \sqrt(n&x_1\bullet x_i\ldots x_n\ )}
#'
#' @param x `numeric`: vector to calculate the geometric mean.
#' @param na.rm `logic`: should NA values be removed.
#' @author Santiago Caño-Muñiz
#' @export


# Calculate geom mean

gm_mean <- function(x, na.rm = TRUE){
  exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
}
