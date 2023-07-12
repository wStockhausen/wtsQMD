#'
#' @title Check if document is being rendered with knitr
#' @details Checks to see if document is being rendered with [knitr].
#' @return TRUE/FALSE
#' @details Checks if document is being rendered in context of function call by
#' checking is [knitr::fig_path] is -1 (returns FALSE) or not (returns TRUE).
#' @importFrom knitr fig_path
#' @export
#'
isKnitr<-function(){
  return(!(basename(knitr::fig_path())=="-1"));
}

#'
#' @title Check if output is pdf/latex
#' @details Checks to see if output is pdf/latex in a [knitr] rendering context.
#' @return TRUE/FALSE
#' @importFrom knitr is_latex_output
#' @export
#'
isOutputPDF<-function(){
  return(knitr::is_latex_output());
}

#'
#' @title Check if output is html
#' @details Checks to see if output is html in a [knitr] rendering context.
#' @return TRUE/FALSE
#' @importFrom knitr is_html_output
#' @export
#'
isOutputHTML<-function(){
  return(knitr::is_html_output());
}
