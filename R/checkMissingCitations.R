#'
#' @title Check for missing bibliographic entries
#'
#' @description
#' Function to check for missing bibliographic entries in a .bib file.
#'
#' @param entries - missing citation entries to look for, or path to file with such
#' @param bib - character vector with bib text, or path to bib file
#'
#' @return vector of missing entries
#'
#' @importFrom stringr str_remove
#'
#' @export
#'
checkMissingCitations<-function(entries,bib){
  if (length(entries)==1){
    #--entries is a file path
    entries = readLines(entries);
  }
  if (length(bib)==1){
    #--entries is a file path
    bib = readLines(bib);
  }
  eps = stringr::str_remove(entries,stringr::fixed("[WARNING] Citeproc: citation "));
  eps = stringr::str_remove(eps,stringr::fixed(" not found"));
  nf = vector(mode="character");
  for (i in 1:length(eps)){
    if (hasBibEntry(eps[i],bib)==0){
      cat("Entry ",eps[i]," not found.\n");
      nf = c(nf,eps[i]);
    } else {
      cat("Entry ",eps[i]," FOUND!\n");
    }
  }
  return(nf);
}
