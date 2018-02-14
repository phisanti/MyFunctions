#' @title 2 Dimensional K-means filter by densiy
#' @description Filter values using a density grid on a two-dimensional kernel density gradient
#' @param x x coordinate of data
#' @param y	y coordinate of data
#' @param q quantile filter. The data within that quantile will be gated
#' @param n Number of grid points in each direction. Can be scalar or a length-2 integer vector.
#' @param logic If true, the returned value is a logic vector with T or F. If false, the function returns the row of the values within the kernel. Default value is True
#'
#' @author Santiago Caño-Muñiz
#' @export



kde2d_filter <- function(x, y, q = 0.05, n = 200, logic = T) {
  z <- MASS::kde2d(x, y, n = n)
  cuts <- data.frame(x = cut(x, seq(z$x[1], z$x[n], length.out = n + 1)),
                     y = cut(y, seq(z$y[1], z$y[n], length.out = n + 1)),
                     point_number = seq_along(x))
  dens <- expand.grid(x = levels(cuts$x), y = levels(cuts$y))
  dens$z <- as.vector(z$z)
  dens <- merge(cuts, dens, sort = FALSE, all.x = TRUE)
  dens$z[is.na(dens$z)] <- 0
  if (logic == T) {
    pts <- (dens$z >= quantile(dens$z, prob = q))
    return(pts)
  }
  if (logic == F) {
    pts <- dens$point_number[dens$z >= quantile(dens$z, prob = q)]
    return(pts)
  }

}
