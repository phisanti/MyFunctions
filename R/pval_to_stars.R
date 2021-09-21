#' @title P-values to stars
#' @description Transform a vector of probabilities into a significant starts
#' @param p numeric vector with probabilities from 0 to 1.
#' @param ns_na logical report non-significant values as NA.
#' @author Santiago Caño-Muñiz
#' @export

pval_to_stars <- function(p, ns_na = FALSE) {
  p_stars <- data.table::fcase(p <= 0.001, "***",
                    p <= 0.01, "**",
                    p <= 0.05, "*",
                    p > 0.05, "N.S.")
  if (ns_na) p_stars <- ifelse(p_stars == "N.S.", NA, p_stars)

  return(p_stars)
}
