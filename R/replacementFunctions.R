#'
#' @title Replace NAs in character vector with something else
#' @description Function to replace NAs in character vector with something else.
#' @param x - character vector
#' @param rpl - replacement for NAs
#' @return a character vector with NAs replaced
#' @details None.
#' @importFrom stringr str_replace
#' @export
#'
replaceNAs<-function(x,rpl="--"){
  x = stringr::str_replace(x,"NA",rpl);
  return(x);
}
