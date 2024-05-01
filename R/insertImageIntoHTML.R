#'
#' @title Insert an image from a file into a QMD markdown document for HTML
#' @description Function to insert an image from a file into a QMD markdown document for HTML.
#' @param lst - list with figure info, or NULL if remaining inputs are given (see [@details])
#' @param pth - path to image file--ignored if `lst` is given
#' @param cap - caption (or NULL)--ignored if `lst` is given
#' @param wid - image width (in inches, or NULL)--ignored if `lst` is given
#' @param hgt - image width (in inches, or NULL)--ignored if `lst` is given
#' @param dpi - image dots per inch (or NULL; required if `wid` or `hgt` is given)--ignored if `lst` is given
#' @return - invisible NULL
#' @details  Intended for use in a QMD rendered in a HTML environment.
#' The necessary HTML code to insert an image into, e.g., a HTML document
#' is created and [cat]'ed. The QMD chunk that employs this function should
#' set `results="asis"` in the chunk header information.
#'
#' The corresponding function for insertion into latex is [insertImageIntoLatex].
#'
#' The input `lst` should have the following elements:
#' \itemize{
#' \item{pth - path to image relative to `root` of document to be created}
#' \item{lbl - figure label--must start with 'fig-'}
#' \item{wid - figure width in inches, optional}
#' \item{hgt - figure height in inches, optional}
#' \item{dpi - dots per inch (required if `wid` or `hgt` is given)}
#' }
#'
#' @importFrom wtsUtilities abs_to_rel
#'
#' @export
#'
insertImageIntoHTML<-function(lst=NULL,pth=NULL,cap=NULL,wid=NULL,hgt=NULL,dpi=NULL){
  if (is.null(lst)||(!inherits(lst,"list")))
    lst = list(pth=pth,cap=cap,wid=wid,hgt=hgt,dpi=dpi);
  cat("::: {.cell}\n",sep="")
  cat("::: {.cell-output-display}\n");
  cat("![",lst$cap,"](",wtsUtilities::abs_to_rel(lst$pth,root),"){#",lst$lbl,sep="");
  if (!is.null(lst$wid)) cat(" width=",lst$wid*lst$dpi,sep="");
  if (!is.null(lst$hgt)) cat(" height=",lst$hgt*lst$dpi,sep="");
  cat("}\n");
  cat(":::\n");
  cat(":::\n\n");
  return(invisible(NULL));
}
