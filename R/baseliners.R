#' @title Signal baseline
#'
#' @description Function to find the reference baseline in a
#' fluorescence traceplot. The algorithm operates as follows:
#' - First, it searches for the minimum value within the time series.
#' - Second, removes $k$-neightbours before and after.
#' - Third repeats $n$-times.
#' - Fourth, fit a regression line to the filtered points.
#'
#' The difference between the linear base liner and elastic baseliner is the regression method
#' during the last step of the algorithm. In hte linear baseline uses the lm method while the
#' elastic baseline relieas on a gam model.
#'
#' @param x `numeric`: time series to find the baseline.
#' @param n_points `integer`: number of points to calculate the baseline.
#' @param k `integer`: number of points to clear each iteration.
#' @param filter_lows `logic`: whether or do a linear fitting to remove values
#' above the trend.
#'
#' @return A list with three items: The estimated baseline, the adjusted residual value and
#' the relative value.
#'
#'
#' @examples
#'
#' x <- rnorm(500)
#' x <- x + 5* exp(-(1:500 - 200)^2/20) +
#'   5* exp(-(1:500 - 100)^2/20) +
#'   5* exp(-(1:500 - 400)^2/20)
#' plot(x, type = "b", pch = 16)
#'
#' example <- linear_baseline(x)
#' example2 <- elastic_baseline(x)
#'
#' lines(example$baseline, col = "red")
#' lines(example2$baseline, col = "blue")
#'
#' @export
#'

linear_baseline <- function(x, n_points = 20L, k = 5L, filter_lows = TRUE) {

  steps <- 1:length(x)
  tmp <- baseline <- data.frame(x, steps)

  # If requested, remove values avobe the expected average
  if (filter_lows) {

    m0 <- stats::lm.fit(y = tmp$x, x = as.matrix(tmp$steps))
    baseline[stats::resid(m0) >0,] <- NA

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

  m <- lm(x ~ steps, data = tmp[min_index, ])

  out <- list(baseline = stats::predict(m, newdata = tmp))
  out$resid <- x - out$baseline
  out$relative <- x / out$baseline

  return(out)
}

#' @title Signal baseline
#'
#' @inheritParams linear_baseline
#'
#' @export
#'

elastic_baseline <- function (x, n_points = 20L, k = 5L, filter_lows = TRUE)
{
  steps <- 1:length(x)
  x <- data.table::frollapply(x, n = k, FUN = min, na.rm = T, align = "center")

  tmp <- baseline <- data.frame(x, steps)
  if (filter_lows) {
    m0 <- stats::lm(x ~ steps, tmp)
    baseline[stats::resid(m0) > 0, ] <- NA
  }
  k <- k
  min_index <- rep(0, n_points)
  for (i in seq_along(min_index)) {
    if (sum(!is.na(baseline$x)) > 1) {
      N_min_point <- baseline$steps[which.min(baseline$x)]
      min_index[i] <- N_min_point
      baseline[max(0, N_min_point - k):min(nrow(baseline),
                                           N_min_point + k), ] <- NA
    }
  }

  # Add knots at the begining and end
  min_index <- c(1, length(x), min_index)
  mid_point <- round(length(x)/2)
  tmp$x[1] <- min(x[1:mid_point], na.rm = T)
  tmp$x[length(x)] <- min(x[mid_point:length(x)], na.rm = T)

  m <- mgcv::gam(x ~ s(steps, k = 3), tmp[min_index, ], family = "gaussian")
  out <- list(baseline = stats::predict(m, newdata = data.frame(steps = 1:length(x))))
  out$resid <- x - out$baseline
  out$relative <- x / out$baseline

  return(out)
}




