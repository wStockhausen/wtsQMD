#'
#' @title Insert figure into a QMD markdown document for HTML
#' @description Function to insert a figure into a QMD markdown document for HTML.
#' @param lst - list with figure info (see [@details])
#' @return - invisible NULL
#' @details The input `lst` should have elements
#' \itemize{
#' \item{pth - path relative to `root` to figures folder}
#' \item{lbl - figure label--must start with 'fig-'}
#' \item{wid - figure width in inches, optional}
#' \item{hgt - figure height in inches, optional}
#' \item{dpi - dots per inch}
#' }
#' @export
#'
insertFigInHTML<-function(lst){
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
