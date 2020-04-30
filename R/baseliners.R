#' @title Signal baseline
#'
#' @description Function to find the reference baseline in a
#' fluorescence traceplot. The algorithm operates as follows:
#' - First, it searches for the minimum value within the time series.
#' - Second, removes $k$-neightbours before and after.
#' - Third repeats $n$-times.
#' - Fourth, fit a linear regression line to the filtered points.
#'
#' @param x `numeric`: time series to find the baseline.
#' @param n_points `integer`: number of points to calculate the baseline.
#' @param k `integer`: number of points to clear each iteration.
#' @param filter_lows `logic`: whether or do a linear fitting to remove values
#' above the trend.
#'
#' @importFrom stats predict
#' @importFrom stats quantile
#' @importFrom stats resid
#'
#' @examples
#'
#' x <- rnorm(500)
#' x <- x + 5* exp(-(1:500 - 200)^2/20) +
#'   5* exp(-(1:500 - 100)^2/20) +
#'   5* exp(-(1:500 - 400)^2/20)
#' plot(x, type = "b", pch = 16)
#'
#' example <- find_baseline(x)
#'
#' lines(example$baseline, col = "red")
#'
#' @export
#'

find_baseline <- function(x, n_points = 20L, k = 5L, filter_lows = TRUE) {

  steps <- 1:length(x)
  tmp <- baseline <- data.frame(x, steps)

  # If requested, remove values avobe the expected average
  if (filter_lows) {

    m0 <- lm(x ~ steps, tmp)
    baseline[resid(m0) >0,] <- NA

  }

  k <- k
  min_index <- rep(0, n_points)

  # Loop, find minimun and remove neigbours
  for(i in seq_along(min_index)) {

    if (sum(!is.na(baseline$x)) > 1) {
    N_min_point <- baseline$steps[which.min(baseline$x)]
    min_index[i] <- N_min_point
    baseline[max(0, N_min_point - k) : min(nrow(baseline), N_min_point + k),] <- NA
    }
  }

  m <- lm(x ~ steps, tmp[min_index,])

  out <- list(baseline = predict(m, newdata = tmp))
  out$resid <- x - out$baseline

  return(out)
}
