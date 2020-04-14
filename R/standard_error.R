#' @title Standard error
#'
#' @description Calculates the standard error as: \deqn{(SE=\frac{\sigma }{\sqrt{n}})}
#'
#' @param x numeric vector
#' @author Santiago Caño-Muñiz
#'
#' @export

se <- function(x){stats::sd(x)/sqrt(length(x))}
