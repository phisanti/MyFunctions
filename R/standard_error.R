#' @title Standard error
#'
#' @description Calculates the standard error as: \deqn{(SE=\frac{\sigma }{\sqrt{n}})}
#'
#' @param x numeric vector
#' @param na.rm logig: Remove NA elements from the vector.
#' @author Santiago Caño-Muñiz
#'
#' @export

se <- function(x, na.rm = FALSE){stats::sd(x, na.rm = )/sqrt(length(x))}
