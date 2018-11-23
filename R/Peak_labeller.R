
# First version peak labeller function

peak_labeler <- function(x) {

  tmp <- NULL

  for (i in seq_along(x)) {

    if (x[i] == 0) { tmp[i] <- 0 } # If baseline, mark as 0

    if (x[i] == 1) {

      # If x[n] belongs to a peak
      if (i == 1) {tmp[i] <- 1} # Label as 1 at t0

      else{

        if (!exists("Peak")) {Peak <- 0}

        if (x[i - 1] == 0) {
          # if previous point is no peak, add as peak

          Peak <- Peak + 1
          tmp[i] <- Peak
        }

        if (x[i - 1] == 1) {
          tmp[i] <- Peak
        }
      }
    }

  }

  return(tmp)

  rm(tmp, Peak, i) # Garbage collection
}



