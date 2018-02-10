#' @title Scientific axis
#' @description Transform a numeric vector into a character expression in scietific format
#' @param x numeric vector to be transform
#' @author Santiago Caño-Muñiz
#' @export


# Scientific axis
#this function writes the axis numbers in scientific formant
scientific_axis <- function(x) {
  # turn in to character string in scientific notation
  x <- paste0(1,"e",x)
  # turn the 'e+' into plotmath format
  x <- gsub("e", "%*%10^", x)
  # return this as an expression
  parse(text = x)
}

