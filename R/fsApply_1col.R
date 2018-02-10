#' @title Apply function over columns in a flowSet
#' @description like many of the apply-style functions in R acts as an iterator for flowSet objects, allowing the
#' application of a function to either the flowFrame or the data matrix itself. The output can the be reconstructed
#' as either a flowSet, a list or a matrix depending on options and the type of objects returned..
#' @param x flowSet to be used
#' @param y string vector indicating the columns
#' @param FUN the function to be applied to each element of x
#' @author Santiago Caño-Muñiz
#' @export

fsApply_1col <- function(x, y = "FSC",FUN = median(),...){
  .f <- function(x){ FUN(exprs(x)[,y], ...)}
  result <- fsApply(x, FUN = .f)
  .n <- row.names(result)
  result <- c(result)
  names(result) <- rep(x = .n,length(result)/length(.n))
  return(result)
}

