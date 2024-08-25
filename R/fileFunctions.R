#'
#' @title Get file name with correct extension for an image in the current knitr environment
#' @description Function to get file with correct extension for an image in the current knitr environment.
#' @param fn - file name for an image
#' @return file name, posibly updated with  extension
#' @details file extension is converted to ".pdf" in a latex environment or "png" in a
#' html environment.
#' @importFrom xfun sans_ext
#' @importFrom xfun with_ext
#' @export
#'
getImageFN<-function(fn){
  def_ext = ifelse(isOutputPDF(),"pdf","png");
  bn = xfun::sans_ext(fn);
  fnp = xfun::with_ext(bn,def_ext);
  if (!file.exists(fnp)) {
    warning(paste0("Image file '",fnp," does not exist. Reverting to original file."))
    fnp = fn;
  }
  return(fnp);
}
