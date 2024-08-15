#'
#' @title Escape special characters in a character vector.
#'
#' @description Function to escape special characters in a character string.
#'
#' @param txt - character string in which to escape special characters
#' @param env - environment for escaping characters (default="latex")
#' @param testing - flag for debugging output
#'
#' @return character vector with special characters escaped.
#'
#' @details Currently escapes the following characters: "&", "%", "_", and "'" in
#' a latex environment outside any $-delimited equation sections.
#'
#' "'" is escaped by substituting "\\textquotesingle " from the latex `textcomp` package.
#'
#' @import stringr
#'
#' @export
#'
escapeChars<-function(txt,env="latex",testing=FALSE){
  if (env=="latex"){
    delim = "$";#--equation delimiter
    ids   = stringr::str_locate_all(txt,stringr::fixed(delim))[[1]][,"start"];
    if (testing) cat("ids = ",ids,"\n");
    nids  = length(ids);
    if (nids==0){
      #--no equations detected
      str = escapeForLatex(txt);
    } else {
      #--equations detected
      chnk  = escapeForLatex(txt |> stringr::str_sub(1,ids[1]-1));#--first chunk of text
      if (testing) cat(paste0("0: '",chnk,"'\n"));
      str   = chnk;
      for (i in seq(1,nids,2)){#--loop over starts of equations
        if (testing) cat(i,"\n");
        eqn = txt |> stringr::str_sub(ids[i],ids[i+1]);#--equation text, so skip escaping
        if (testing) cat(paste0("eqn ",i,": '",eqn,"'\n"));
        str = paste0(str,eqn);
        if (i+2<nids) {
          #--outside equation text, but at least one equation before end of text
          chnk = txt |> stringr::str_sub(ids[i+1]+1,ids[i+2]-1);
        } else {
          #--outside equation text, no more equations
          chnk = txt |> stringr::str_sub(ids[i+1]+1,stringr::str_length(txt));
        }
        if (testing) cat(paste0("chnk ",i,": '",chnk,"'\n"));
        chnk = escapeForLatex(chnk);
        if (testing) cat(paste0("chnk ",i,": '",chnk,"'\n"));
        str  = paste0(str,chnk);
      }#--loop over i
    }#--if nids>0
  }#--if latex
  return(str);
}
# escapeChars("$B_{MSY} = \\theta \\cdot \\alpha$ is an equation. B_MSY = \\theta \\cdot \\alpha is not.",testing=TRUE)
# escapeChars("B_MSY = \\theta \\cdot \\alpha is not.",testing=TRUE);

#'
#' @title Escape special characters in a character string for latex.
#'
#' @description Function to escape special characters in a character string for latex.
#'
#' @param txt - character string in which to escape special characters
#'
#' @return character vector with special characters escaped.
#'
#' @details Currently escapes the following characters: "&", "%", "_", and "'"
#'
#' "'" is escaped by substituting "\\textquotesingle " from the latex `textcomp` package.
#'
escapeForLatex<-function(x){
    x=gsub("\\&","&",x,fixed=TRUE); #--unescape any already escaped first
    x=gsub("&","\\&",x,fixed=TRUE); #--escape all

    x=gsub("\\%","%",x,fixed=TRUE); #--unescape any already escaped first
    x=gsub("%","\\%",x,fixed=TRUE); #--escape all

    x=gsub("\\_","_",x,fixed=TRUE); #--unescape any already escaped first
    x=gsub("_","\\_",x,fixed=TRUE); #--escape all

    x=gsub("\\'","'",x,fixed=TRUE); #--unescape any already escaped first
    x=gsub("'","\\textquotesingle ",x,fixed=TRUE); #--replace with code

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
  str = stringr::str_replace_all(str,pattern=stringr::fixed("'"),replacement="\\textquotesingle ");
  str = stringr::str_replace_all(str,pattern=stringr::fixed('"'),replacement='\\"');
  return(str);
}


#' #'
#' #' @title Un-escape characters in equations in strings for QMD processing
#' #' @description Function to un-escape characters in equations in strings for QMD processing.
#' #' @param txt - string with equations to un-escape
#' #' @param delim - equation delimiter symbol
#' #' @param eqn_chars - equation characters to un-escape
#' #' @return string, but with any characters in `eqn_chars` between equation delimiters un-escaped.
#' #' @details
#' #' The following will be un-escaped if found between the equation delimiters:
#' #' \itemize{
#' #' \item{_ - underscore}
#' #' }
#' #'
#' #' Example:
#' #' txt = "\\  $blab$ $B_{MSY}$ blah 80% F_MSY $1 \\theta $   "
#' #' txt = escapeChars(txt);
#' #' str = unescapeEqnChars(txt);
#' #' cat(paste0("'",txt,"'\n"))
#' #' cat(paste0("'",str,"'\n"))
#' #'
#' #' @export
#' #'
#' unescapeEqnChars<-function(txt,delim="$",chars="_"){
#'   str = txt;
#'   for (chr in chars)
#'     str = unescapeEqnChar(str,delim,chr);
#'   return(str)
#' }
#'
#' #'
#' #' @title Un-escape a character in equations in a string for QMD processing
#' #' @description Function to un-escape a character in equations in a string for QMD processing.
#' #' @param txt - string with equations to un-escape
#' #' @param delim - equation delimiter symbol
#' #' @param eqn_char - equation character to un-escape
#' #' @return string, but with any `eqn_char` between equation delimiters un-escaped.
#' #' @details
#' #' By default, the following will be un-escaped if found between the equation delimiters:
#' #' \itemize{
#' #' \item{_ - underscore}
#' #' }
#' #'
#' #' Example:
#' #' txt = "\\  $blab$ $B_{MSY}$ blah 80% F_MSY $1$   "
#' #' txt = escapeChars(txt);
#' #' str = unescapeEqnChar(txt);
#' #' cat(paste0("'",txt,"'\n"))
#' #' cat(paste0("'",str,"'\n"))
#' #'
#' #' @import stringr
#' #'
#' unescapeEqnChar<-function(txt,delim="$",char="_"){
#'   ids = stringr::str_locate_all(txt,fixed(delim))[[1]][,"start"];
#'   n = length(ids);
#'   str = txt |> str_sub(1,ids[1]-1);
#'   for (i in seq(1,n,2)){
#'     cat(i,"\n");
#'     tst = txt |> str_sub(ids[i],ids[i+1]);
#'     tst = tst |> stringr::str_replace_all(stringr::fixed(paste0("\\",char)),"_");
#'     str = paste0(str,tst);
#'     if (i+2<n) {
#'       tst = txt |> str_sub(ids[i+1]+1,ids[i+2]-1);
#'     } else {
#'       tst = txt |> str_sub(ids[i+1]+1,str_length(txt));
#'     }
#'     str = paste0(str,tst);
#'   }
#'   return(str)
#' }
#'
