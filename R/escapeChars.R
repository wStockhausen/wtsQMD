#'
#' @title Escape special characters in character vector.
#'
#' @description Function to escape special characters in character vector.
#'
#' @param  x - character vector in which to escape special characters
#' @param env - environment for escaping characters (default="latex")
#'
#' @return character vector with special characters escaped.
#'
#' @details Currently escapes the following characters: "&", "%", "_" in
#' a latex environment.
#'
#' @export
#'
escapeChars<-function(x,env="latex"){
  if (env=="latex"){
    x=gsub("\\&","&",x,fixed=TRUE); #--unescape any already escaped first
    x=gsub("&","\\&",x,fixed=TRUE); #--escape all

    x=gsub("\\%","%",x,fixed=TRUE); #--unescape any already escaped first
    x=gsub("%","\\%",x,fixed=TRUE); #--escape all

    x=gsub("\\_","_",x,fixed=TRUE); #--unescape any already escaped first
    x=gsub("_","\\_",x,fixed=TRUE); #--escape all
  }
  return(x);
}

#'
#' @title Escape strings for QMD processing
#' @description Function to escape strings for QMD processing.
#' @param str - string to escape
#' @return string with special QMD characters escaped with "\\" so [cat]'ed result
#' has properly-escaped character.
#' @details
#' The following characters will be escaped:
#' \itemize{
#' \item{' - single quote}
#' \item{" - double quote}
#' }
#' @importFrom stringr str_replace_all
#' @export
#'
escapeQMD<-function(str){
  str = stringr::str_replace_all(str,pattern=stringr::fixed("'"),replacement="\\'");
  str = stringr::str_replace_all(str,pattern=stringr::fixed('"'),replacement='\\"');
  return(str);
}

