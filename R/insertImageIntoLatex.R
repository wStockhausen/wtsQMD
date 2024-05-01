#'
#' @title Create and paste the latex code to insert an image into a latex document
#'
#' @description Function create and paste the latex code to insert an image into a latex document.
#'
#' @param pth - path to image relative to `root` of document to be created
#' @param cap - the caption for the image
#' @param lbl - the latex label for the image
#' @param width - figure width (in inches)
#' @param height - figure height (in inches)
#' @param align - character string indicating latex alignment
#'
#' @return nothing.
#'
#' @details Intended for use in a QMD rendered in a latex environment.
#' The necessary latex code to insert an image into, e.g., a pdf document
#' is created and [cat]'ed. The QMD chunk that employs this function should
#' set `results="asis"` in the chunk header information.
#'
#' The corresponding function for insertion into HTML is [insertImageIntoHTML].
#'
#' @export
#'
insertImageIntoLatex<-function(pth,cap,lbl=cap,width=6.5,height=6,align="ht!"){
  if (!is.na(width)) w=paste0("width=",width,"in");
  if (!is.na(height)) h=paste0("height=",height,"in")
  if (is.na(height)) {
    ig = paste0("\\includegraphics[",w,"]");
   } else if (is.na(width)) {
    ig = paste0("\\includegraphics[",h,"]");
   } else {
    ig = paste0("\\includegraphics[",w,",",h,"]");
   }
  #message("insertImage: pth = ",pth)
  s   = paste0(ig,"{",pth,"} \\caption{",cap,"}\\label{fig:",lbl,"}");
  all = c(paste0("\\begin{figure}[",align,"]"),
          s,
          "\\end{figure}")
  #message("all: ",all)
  cat(all,sep="\n");
}
