#' @title Two-dimensional kernel analysis
#' @description Two-dimensional kernel density estimation with an axis-aligned bivariate normal kernel,
#'  evaluated on a square grid implemented with some Rcpp steps.
#' @param x y coordinate of data a list of ggplot objects
#' @param y x coordinate of data
#' @param h vector of bandwidths for x and y directions. Defaults to normal reference bandwidth
#' (see bandwidth.nrd). A scalar value will be taken to apply to both directions.
#' @param n Number of grid points in each direction. Can be scalar or a length-2 integer vector.
#' @param lims The limits of the rectangle covered by the grid as c(xl, xu, yl, yu).
#' @author Santiago Caño-Muñiz
#' @export

kde2dCpp <- function(x, y, h, n = 25, lims = c(range(x), range(y))){
  nx <- length(x)
  if (length(y) != nx)
    stop("data vectors must be the same length")
  if (any(!is.finite(x)) || any(!is.finite(y)))
    stop("missing or infinite values in the data are not allowed")
  if (any(!is.finite(lims)))
    stop("only finite values are allowed in 'lims'")
  n <- rep(n, length.out = 2L)
  gx <- seq.int(lims[1L], lims[2L], length.out = n[1L])
  gy <- seq.int(lims[3L], lims[4L], length.out = n[2L])
  h <- if (missing(h))
    c(MASS::bandwidth.nrd(x), MASS::bandwidth.nrd(y))
  else rep(h, length.out = 2L)
  if (any(h <= 0))
    stop("bandwidths must be strictly positive")
  h <- h/4
  ax <- outer(gx, x, "-")/h[1L]
  ay <- outer(gy, y, "-")/h[2L]
  A <- matrix(dnorm(ax), , nx)
  B <- matrix(dnorm(ay), , nx)
  gridEval <- MyFunctions::eigenTransMatMult(A, B)
  nh_xh_y <- c(1/(nx * h[1L] * h[2L]))
  z <- MyFunctions::ScalarMult(gridEval,nh_xh_y)
  list(x = gx, y = gy, z = z)
}

