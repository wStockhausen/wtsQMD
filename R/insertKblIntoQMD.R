#'
#' @title Insert (print) a [knitr] \code{kable} object into a QMD (Quarto markdown) document
#' @description Function to insert (print) a [knitr] \code{kable} object into a QMD (Quarto markdown) document.
#' @param kbl - [knitr] \code{kable} object
#' @param lbl - table label (i.e., of the form "tbl-..." for cross-references)
#' @param cap - table caption
#' @param ori - orientation ("L" or "P")
#' @return list if output environment is latex, otherwise NULL
#' @details This resulting table will be
#' \itemize{
#'   \item{printed "here" in the text if the output environment is html}
#'   \item{printed in the Tables section in the text if the output environment is latex}
#' }
#'
#' If the output environment is latex, then a list to add to a `lstTbls`
#' list is returned, otherwise an invisible NULL is returned.
#'
#' If the output environment is not latex, a QMD cell with table caption is printed via [cat]
#' to the output environment (typically an intermediate markdown file used in the
#' process of creating the final output).
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
    return(lst);
  }
}
