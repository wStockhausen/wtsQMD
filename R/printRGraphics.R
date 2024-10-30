#'
#' @title "Print" a base R graphics figure into a Quarto markdown document
#' @description Function to "print" a base R graphics figure into a Quarto markdown document.
#' This is the R base graphics counterpart to [printGGplot].
#' @param p - an `expression` yielding a base R graphics figure when evaluated (or NULL)
#' @param lbl - figure label (OPTIONAL; should start with "fig-" if given)
#' @param pth - path to saved figure (OPTIONAL)
#' @param cap - figure caption (OPTIONAL)
#' @param asp - figure aspect (height/width) (OPTIONAL)
#' @param wid - figure width (inches) (OPTIONAL)
#' @param dpi - dots-per-inch (OPTIONAL)
#' @param ori - orientation ("P": portrait (default); "L": landscape)
#' @param type - image file type (as string) compatible with [ggplot2::ggsave()], if not NULL (e.g., "png", "pdf","svg")
#' @param xtraLbl - extra string to add to label (helpful for printing multiple plots in the same chunk) (OPTIONAL)
#' @param xtraFigFN - extra string to add to path to saved figure (defaults to `xtraLbl`) (OPTIONAL)
#' @param xtraCap - extra string to add to caption. (OPTIONAL)
#' @return NULL, if the [knitr] environment is not latex. Otherwise, a lstFigs list that should be
#' appended to the QMD's lstFigs list.
#'
#' @details All inputs except `p`, the `expression` when evaluated that yields a base R graphics figure
#' to print/save, are optional. Values for `lbl`, `pth`, `cap`
#' `asp`, `wid`, and `dpi` that are not specified by the user will be taken from [knitr]'s
#' current chunk options.
#'
#' If `p` is (explicitly) NULL, then `pth` should provide a path to a valid image file.
#'
#' If `type` is not NULL, the plot will be output as the specified type at the path given. Potential values
#' are "png" or "pdf", as well as other options supported by [ggplot2::ggsave()].
#'
#' @importFrom knitr opts_current
#' @importFrom stringr str_starts
#' @importFrom xfun sans_ext
#' @importFrom xfun with_ext
#'
#' @export
#'
printRGraphics<-function(p,lbl=NULL,pth=NULL,cap=NULL,
                         asp=NULL,wid=NULL,dpi=NULL,ori="P",type=NULL,
                         xtraLbl=NULL,xtraFigFN=xtraLbl,xtraCap=NULL,
                         testing=FALSE){
  lstFigs = NULL;
  if (is.null(dpi)) dpi = knitr::opts_current$get("fig.dpi");
  if (is.null(dpi)) dpi = 100;#--use as default if not supplied and can't get from knitr
  #                       knitr::opts_current$set(fig.dpi=dpi);
  if (testing) cat("dpi = ",dpi,"\n\n")
  if (is.null(wid)) wid = knitr::opts_current$get("fig.width");
  #knitr::opts_current$set(fig.wid=wid);
  if (testing) cat("wid = ",wid,"\n\n")
  if (is.null(asp)) asp = knitr::opts_current$get("fig.asp");
  if (testing) cat("asp = ",asp,"\n\n")
  #knitr::opts_current$set(fig.asp=asp);
  if (!isOutputPDF()) {
    if (!isOutputHTML()) {
      if (is.null(p)) {
        message("external immage")
      } else eval(p);#--not in a pdf or html context
    } else {
      #--in an html context
      figlbl = knitr::opts_current$get("label");
      if (testing) cat("figlbl = ",figlbl,"\n\n")
      if (is.null(figlbl)||stringr::str_starts(figlbl,"fig_")){
        #--faking out Quarto
        if (testing) cat("lbl = ",lbl,"\n\n")
        if (testing) cat("pth = ",pth,"\n\n")
        cat("\n::: {.cell-output-display}\n")
        cat(paste0("![",cap,"](",pth,"){#",lbl," width=",dpi*wid,"}\n"))
        cat(":::\n\n");
        if (!is.null(p)){
          if (testing) message("printing plot to png")
          png(pth,width=wid,height=asp*wid,units="in",res=dpi); #--might want to convert for non-default units ("in") (dpi doesn't seem to matter)
          eval(p);
          dev.off();
          #--ggplot2::ggsave(pth,plot=p,width=wid,height=asp*wid,units="in",dpi=dpi);
        }
      } else {
        #--normal Quarto fig- context
        if (is.null(p)){
            #--insert an image filename into markdown
          cat("\n::: {.cell-output-display}\n")
          cat(paste0("![",cap,"](",pth,"){#",lbl," width=",dpi*wid,"}\n"))
          cat(":::\n\n");
        } else eval(p);
      }
    }
  } else {
    #--output is pdf
    if (is.null(lbl)) lbl = wtsQMD::getLabel(xtraLbl);
    if (is.null(pth)) pth = wtsQMD::getFigFN(xtraFigFN);
    if (is.null(cap)) cap = wtsQMD::getFigCaption(xtraCap);
    if (!is.null(type)){
      #--want to substitute "type" for current file extension
      bn = xfun::sans_ext(pth);
      pth = xfun::with_ext(bn,type);
    }
    lstFigs = list();
    lstFigs[[lbl]] = list(lbl=lbl,cap=cap,pth=pth,wid=wid,dpi=dpi,ori=ori);#--could give hgt here, as well, but  ggsave takes care of it?
    if (!is.null(p)){
      if (testing) message("printing plot to pdf")
      pdf(pth,width=wid,height=asp*wid); #--might want to convert for non-default units ("in") (dpi doesn't seem to matter)
      eval(p);
      dev.off();
      #--ggplot2::ggsave(pth,plot=p,width=wid,height=asp*wid,units="in",dpi=dpi);
    }
  }
  return(lstFigs)
}
