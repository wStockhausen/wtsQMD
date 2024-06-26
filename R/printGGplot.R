#'
#' @title "Print" ggplot2 figure in a Quarto markdown document
#' @description Function to "print" ggplot2 figure in a Quarto markdown document.
#' @param p - a ggplot2 figure object
#' @param lbl - figure label (OPTIONAL; should start with "fig-" if given)
#' @param pth - path to saved figure (OPTIONAL)
#' @param cap - figure caption (OPTIONAL)
#' @param asp - figure aspect (height/width) (OPTIONAL)
#' @param wid - figure width (inches) (OPTIONAL)
#' @param dpi - dots-per-inch (OPTIONAL)
#' @param ori - orientation ("P": portrait (default); "L": landscape)
#' @param xtraLbl - extra string to add to label (helpful for printing multiple plots in the same chunk) (OPTIONAL)
#' @param xtraFigFN - extra string to add to path to saved figure (defaults to `xtraLbl`) (OPTIONAL)
#' @param xtraCap - extra string to add to caption. (OPTIONAL)
#' @return NULL, if the [knitr] environment is not latex. Otherwise, a lstFigs list that should be
#' appended to the QMD's lstFigs list.
#' @details All inputs except `p`, the plot to print/save, are optional. Values for `lbl`, `pth`, `cap`
#' `asp`, `wid`, and `dpi` that are not specified by the user will be taken from [knitr]'s
#' current chunk options.
#'
#' @export
#'
printGGplot<-function(p,lbl=NULL,pth=NULL,cap=NULL,
                      asp=NULL,wid=NULL,dpi=NULL,ori="P",
                      xtraLbl=NULL,xtraFigFN=xtraLbl,xtraCap=NULL){
  lstFigs = NULL;
  if (!isOutputPDF()) {print(p);} else {
    if (is.null(dpi)) dpi = knitr::opts_current$get("fig.dpi");
    if (is.null(dpi)) dpi = 100;#--use as default if not supplied and can't get from knitr
    if (is.null(wid)) wid = knitr::opts_current$get("fig.width");
    if (is.null(asp)) asp = knitr::opts_current$get("fig.asp");
    if (is.null(lbl)) lbl = wtsQMD::getLabel(xtraLbl);
    if (is.null(pth)) pth = wtsQMD::getFigFN(xtraFigFN);
    if (is.null(cap)) cap = wtsQMD::getFigCaption(xtraCap);
    lstFigs = list();
    lstFigs[[lbl]] = list(lbl=lbl,cap=cap,pth=pth,wid=wid,dpi=dpi,ori=ori);#--could give hgt here, as well, but  ggsave takes care of it?
    ggsave(pth,plot=p,width=wid,height=asp*wid,units="in",dpi=dpi);
  }
  return(lstFigs)
}
