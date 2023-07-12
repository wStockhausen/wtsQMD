#'
#' @title Convert character vector from metric tons to numeric vector in millions of lbs
#'
#' @description Function to convert character string from metric tons to millions of lbs.
#'
#' @param x - numeric of character vector of numbers in metric tons
#'
#' @return numeric vector in millions of lbs
#'
#'@export
t2MLB = function(x){
  f = 2.204624/1000;#--tons to millions lbs
  x = stripNumSyms(x);
  return(f*x);
}

#'
#' @title Get the last n values of a vector
#' @description Function to get the last \code{n} values of a vector.
#' @param x - vector
#' @param n - number of last values to get (default is 1)
#' @return a vector of length \code{n}
#' @details none.
#' @export
#'
last<-function(x,n=1){
  nx = length(x);
  return(x[nx-rev(0:(n-1))])
}

