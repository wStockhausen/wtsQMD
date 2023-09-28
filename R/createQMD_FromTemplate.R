#'
#' @title Create a Quarto markdown document from a template
#'
#' @description Function to create a Quarto markdown document from a template.
#'
#' @param fn - filename for output QMD file
#' @param ltx - filename (full path) for latex include file (see [@details])
#'
#' @return nothing (invisible(NULL))
#'
#' @details Writes contents of \code{system.file("file/qmd_chapter_template.qmd",package="wtsQMD")}
#' to the desired file. File extension should be '.qmd'. As a default, the QMD will use the file
#' given by \code{system.file("files/ltx_ExtraLatexIncludes.tex",package="wtsQMD")} as the latex
#' 'include' file.
#'
#' @export
#'
createQMD_FromTemplate<-function(fn,
                                 ltx=system.file("files/ltx_ExtraLatexIncludes.tex",package="wtsQMD")){
  fnt = system.file("files/qmd_chapter_template.qmd",package="wtsQMD");
  lns = readLines(fnt);
  str = paste(lns,collapse="\n");
  str = stringr::str_replace(str,"&&latex_include_file",ltx);
  cat(str,file=fn);
  return(invisible(NULL));
}
