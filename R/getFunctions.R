#'
#' @title Create a Quarto/knitr chunk label
#' @description Function to create a Quarto/knitr chunk label.
#' @param xtra - extra string toadd to default label
#' @return a string
#' @details If the function is not run within the context of a knitr chunk,
#' then 'lbl' is used as the default label.
#'
#' If the chunk label starts with "tbl_" or "fig_", it is assumed that the
#' associated chunk output is a table or figure that should be indexed and
#' cross-referenced, so the "_" is replaced by "-" in order that Quarto/Pandoc's
#' automatic indexing tracks the output correctly.
#'
#' Non-alphanumeric characters in \code{xtra}
#' are replaced by "-" in order to conform to Quarto labels.
#'
#' @import knitr
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @export
#'
getLabel<-function(xtra=NULL){
  lbl = knitr::opts_current$get("label");
  if (is.null(lbl)) lbl = "lbl";
  lbl = stringr::str_replace(lbl,"^tbl_","tbl-")
  lbl = stringr::str_replace(lbl,"^fig_","fig-")
  if (!is.null(xtra)) {
    xtra = stringr::str_replace_all(xtra,"[^[[a-z][A-Z][0-9]]]","-");
    lbl = paste0(lbl,xtra);
  }
  return(lbl);
}
#'
#' @title Get a Quarto/knitr chunk figure caption
#' @description Function to get a Quarto/knitr chunk figure caption.
#' @param xtra - extra string (or expression) to  add to caption
#' @return a string
#' @details The function obtains the figure caption using `knitr::opts_current$get("fig.cap")`.
#' If the result is an expression, it is evaluated in the parent frame of the caller.
#' If the function is not run within the context of a knitr chunk,
#' then 'Figure' is used as the default caption.
#' @importFrom knitr opts_current
#' @export
#'
getFigCaption<-function(xtra=NULL){
  cap = knitr::opts_current$get("fig.cap");
  if (is.null(cap)) cap = "Figure";
  if (is.expression(cap)) cap = eval(cap);
  if (!is.null(xtra)){
    if (is.expression(xtra)) xtra = eval(xtra);
    cap = paste0(cap,xtra);
  }
  return(cap);
}
#'
#' @title Create an appropriate filename for a figure
#' @description Function to create an appropriate filename for a figure.
#' @param xtra - extra string to add to default filename
#' @return string
#' @details Non-alphanumeric characters in \code{xtra} that cannot be in a label are replaced by "-".
#' @import knitr
#' @importFrom stringr str_replace_all
#' @importFrom xfun file_ext
#' @importFrom xfun sans_ext
#' @importFrom xfun with_ext
#' @export
#'
getFigFN<-function(xtra=NULL){
  if (!exists("dirFigs")) dirFigs = ".";
  if (!exists("def_ext")) def_ext = ifelse(isOutputPDF(),"pdf","png");
  fn = file.path(dirFigs,basename(xfun::with_ext(knitr::fig_path(),def_ext)))
  if (!is.null(xtra)){
    xtra = stringr::str_replace_all(xtra,"[^[[a-z][A-Z][0-9]]]","-");
    fp  = knitr::fig_path();
    xt = xfun::file_ext(fp);
    if (xt=="") xt = def_ext;
    bn = paste0(basename(xfun::sans_ext(fp)),xtra);#--add xtra to basename
    fn = file.path(dirFigs,xfun::with_ext(bn,xt));
  }
  return(fn);
}
