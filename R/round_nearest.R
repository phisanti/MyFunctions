#' @title Round to the nearest number.
#'
#'
#' @param x numeric: a numeric vector.
#' @param base integer: nearest integer to round round the number.
#' @author Santiago Caño-Muñiz
#'
#' @export


mround <- function(x, base = 5){

  n <- base*round(x/base)
  return(n)
  }
