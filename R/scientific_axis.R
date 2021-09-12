#' @title Scientific axis
#' @description Transform a numeric vector into a character expression in scietific format
#' @param x numeric: vector to be transform
#' @param base integer: Base for the exponent
#' @author Santiago Caño-Muñiz
#' @export


# Scientific axis
#this function writes the axis numbers in scientific formant
scientific_axis <- function(x, base = 10) {
  # turn in to character string in scientific notation
  x <- paste0(1,"e",x)
  # turn the 'e+' into plotmath format
  base <- paste0("%*%", as.character(base), "^")
  x <- gsub("e", base, x)
  # return this as an expression
  parse(text = x)
}

