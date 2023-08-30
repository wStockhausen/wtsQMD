#'
#' @title Find figure labels (in order of first appearance)
#' @description Function to find figure labels (in order of first appearance).
#' @param txt - text to search, or file path for text to search
#' @param fn - filename to write labels to
#' @return - tibble with figure labels in orser of first occurrence
#' @importFrom readr write_csv
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_remove
#' @importFrom tibble tibble
#' @export
#'
findFigLabels<-function(txt,fn="r_reorderFigs.csv"){
  if (length(txt)==1) txt = readLines(txt);
  yaml = stringr::str_which(txt,"---");#--find start/end of yaml
  txt = txt[(yaml[2]+1):length(txt)];#--drop yaml from search
  res = stringr::str_extract_all(string=txt,pattern="((@fig-.*?])+)+");
  figs = vector(mode="character")
  for (i in 1:length(res)){
    v = res[[i]];
    if (length(v)>0) figs=c(figs,v)
  }
  figs = tibble::tibble(lbl=stringr::str_remove(figs,stringr::fixed("]"))) |>
           dplyr::distinct();
  if (!is.null(fn)) readr::write_csv(figs,file=fn);
}

#'
#' @title Find table labels (in order of first appearance)
#' @description Function to find table labels (in order of first appearance).
#' @param txt - text to search, or file path for text to search
#' @param fn - filename to write labels to
#' @return - tibble with table labels in order of first occurrence
#' @importFrom readr write_csv
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_remove
#' @importFrom tibble tibble
#' @export
#'
findTblLabels<-function(txt,fn="r_reorderTbls.csv"){
  if (length(txt)==1) txt = readLines(txt);
  yaml = stringr::str_which(txt,"---");#--find start/end of yaml
  txt = txt[(yaml[2]+1):length(txt)];#--drop yaml from search
  res = stringr::str_extract_all(string=txt,pattern="((@tbl-.*?])+)+");
  tbls = vector(mode="character")
  for (i in 1:length(res)){
    v = res[[i]];
    if (length(v)>0) tbls=c(tbls,v)
  }
  tbls = tibble::tibble(lbl=stringr::str_remove(tbls,stringr::fixed("]"))) |>
           dplyr::distinct();
  if (!is.null(fn)) readr::write_csv(tbls,file=fn);
  return(tbls);
}


