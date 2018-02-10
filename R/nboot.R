#' @title Bootstrap from a normal distribution
#' @description Manual implementation of bootstrap sampling from normal distribution
#' @param x Numeric vector
#' @param R Number of samples
#'
#' @author Santiago Caño-Muñiz
#' @export

nboot <- function(x, R) {
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  do.call(rbind,
          lapply(1 : R,
                 function(i) {
                   xx <- sort(nsim(n, m, s))
                   p <- seq_along(x) / n - 0.5 / n
                   data.frame(x = xx, p = p, sim = i)
                 }))
}

# Internal function for simulation

nsim <- function(n, m = 0, s = 1) {
  z <- rnorm(n)
  m + s * ((z - mean(z)) / sd(z))
}
