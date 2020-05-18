#' @title Peak In range
#'
#' @description Strategy to define peaks within an expected region. The algorithm
#' uses the ration between a fast average and a slow average to define areas avobe
#' the normal baseline. Those areas are identified as peaks and then labelled sequentially.
#'
#' @param x numeric: time series to find the baseline.
#' @param n1 integer: number of points for the slow average.
#' @param n2 integer: number of points for teh fast average.
#' @param peak_loc integer vector: expected location of the peaks.
#'
#' @return A data frame with the peaks and peak ID
#'
#'
#' @export
#'

peak_inrange <- function(x, n1 = 100L, n2 = 10L, peak_loc) {

  . <- Frame <- peakID <- peak <- NULL
  # Fill NA
  x <- data.table::nafill(x, fill = min(x, na.rm = TRUE))

  # Determine peak/valley with cross of EMA
  slow_s <- stats::HoltWinters(x, gamma = FALSE, beta = FALSE, alpha = 1/(n1 + 1))
  fast_s <- stats::HoltWinters(x, gamma = FALSE, beta = FALSE, alpha = 1/(n2 + 1))
  p_point <- stats::fitted(fast_s)[, "xhat"] - stats::fitted(slow_s)[, "xhat"]
  p_point <- c(0, p_point)

  # Label peaks
  p_point_l <- p_point > 0
  p_point_lID <- p_point_l == T & data.table::shift(p_point_l, n = 1) == F
  p_ID <- cumsum(p_point_lID)
  p_ID <- ifelse(p_point < 0 , 0, p_ID)

  # Find for peaks within our range of interest
  .d <- data.table::data.table(Frame = 1:length(x),
                               peakID = p_ID,
                               peak = p_point_l)
  peak_loc_margin <- sapply(peak_loc, function(x) {(x - 10):(x + 10)}) %>%
    c()
  preaks_inrange <- .d[Frame %in% c(peak_loc_margin), unique(peakID)]
  .d[Frame < n1, c("peakID", "peak") :=  .(0, F)]
  .d[, c("peakID", "peak") :=
       .(ifelse(peakID %in% preaks_inrange, peakID, 0),
         ifelse(peakID %in% preaks_inrange, peak, F))]
  .d[, peakID := as.integer(as.factor(peakID))]
  .d[, Frame := NULL]
  .d <- data.frame(.d)
  return(.d)
  rm(.d)
}
