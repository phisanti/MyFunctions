#' @title Gibbs Sampling from a normal distribution
#' @description Manual implementation of basic gibbs sampling from normal distribution
#' @param x Numeric vector
#' @param niter Number of iterations to extract samples
#' @param prior.mu Starting value of mu
#' @param prior.sd Starting value of sigma
#' @param burn.in Warm up samples
#'
#' @author Santiago Caño-Muñiz
#' @export


Gaussian_GS <- function(x, niter = 2000, prior.mu = 0, prior.sd =1, burn.in = 200) {
  # Default priors
  if (prior.mu == 0) {prio.mu <- mean(x)} # Fill prior mu with empirical mu
  if (prior.sd == 1) {prior.sd <- sd(x)} # Fill prior sd with empirical sd
  mu.cur <- prior.mu
  psi.cur <- 1/prior.sd^2 # Calculate psi
  # Output matrix
  sim.gibbs <- matrix(0, nrow = niter + 1, ncol = 2)
  colnames(sim.gibbs) <- c('mu', 'sigma')

  # Initial values
  mu.cur <- mu.cur + rnorm(1, sd = 0.2)
  psi.cur <- psi.cur * exp(rnorm(1, sd = 0.2))
  sim.gibbs[1, 1] <- mu.cur
  sim.gibbs[1, 2] <- psi.cur
  n <- length(x)
  # Sampling
  xbar <- mean(x)
  for (i in 0:niter) {
    mu.cur <- rnorm(1, mean = xbar, sd = 1 / sqrt(n * psi.cur)) # Sample mean
    psi.cur <- rgamma(1, n/2, 0.5 * sum( (x - mu.cur)^2 )) # Sample sigma
    sim.gibbs[i + 1, 1] <- mu.cur
    sim.gibbs[i + 1, 2] <- psi.cur
  }
  sim.gibbs <- sim.gibbs[-c(1:burn.in),] # Remove burn.in

  # Transform psi to sigma
  sim.gibbs[,2] <- 1/sqrt(sim.gibbs[,2])
  return(sim.gibbs)
}
