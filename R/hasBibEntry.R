#'
#' @title Check for a bibliographic entry
#'
#' @description
#' Function to check for a bibliographic entry in a .bib file.
#'
#' @param entry - citation entry to look for
#' @param bib - character vector with bib text, or path to bib file
#'
#' @return number of entries found
#'
#' @export
#'
hasBibEntry<-function(entry,bib){
  if (length(bib)==1) {
    #--bib assumed to be a file path
    if (file.exists(bib)) bib = readLines(bib);
  }
  res = sum(stringr::str_detect(bib,stringr::fixed(paste0("{",entry,","))));
  return(res);
}
