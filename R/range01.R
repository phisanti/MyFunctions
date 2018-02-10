#' @title Range 0 to 1
#' @description Generic function to scale data in a 0-1 range using the maximum and minimum ratio
#' as described: \deqn{(x_i\ \ -\min\funcapply(x))/(\max\funcapply(x)-\min\funcapply(x)\ )}
#' @param x numeric vector
#' @author Santiago Caño-Muñiz
#' @export

#Scale values into a range 0 to 1
range01 <- function(x){(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}
