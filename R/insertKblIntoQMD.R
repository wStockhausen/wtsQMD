#'
#' @title Insert (print) a [knitr] \code{kable} object into a QMD (Quarto markdown) document
#' @description Function to insert (print) a [knitr] \code{kable} object into a QMD (Quarto markdown) document.
#' @param kbl - [knitr] \code{kable} object
#' @param lbl - table label (i.e., of the form "tbl-..." for cross-references)
#' @param cap - table caption
#' @param ori - orientation ("L" or "P")
#' @return a list wrapped in a list if output environment is latex, otherwise NULL (see [details])
#' @details If the output environment is *not* latex, the resulting table will be printed "here" in the text.
#' To do this, a QMD cell with table caption is printed via [cat]
#' to the output environment (typically an intermediate markdown file used in the
#' process of creating the final output). The value returned by the function is NULL.
#'
#' If the output environment is latex, then a list is returned that should be added to `lstTbls`
#' to be printed in the Tables section. If `lst` is the returned list, there are two ways to do this:
#' \itemize{
#'   \item{`lstTbls = c(lstTbls,lst);`}
#'   \item{`lstTbls[[lbl]] = lst[[1]];`}
#' }
#' where `lbl` in the latter has the same value as the input parameter `lbl`.
#'
#' The following will work in either latex or html output environments:\cr
#' `  lstTbls = c(lstTbls,insertKblIntoQMD(kbl,lbl,cap,ori));`
#'
#' @export
#'
insertKblIntoQMD<-function(kbl,lbl,cap=lbl,ori="L"){
  if (!wtsQMD::isOutputPDF()){
    str = paste0("::: {#",lbl," .cell tbl-cap='",escapeQMD(cap),"'}\n");
    cat(str);
    cat(kbl);
    cat("\n:::\n\n");
    return(invisible(NULL));
  } else {
    lst = list(lbl=lbl,cap=cap,tbl=kbl,ori=ori);
    return(list(lst));
  }
}
