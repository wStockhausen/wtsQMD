#'
#' @title Convert [tables] \code{tabular} object to [knitr] \code{kable} object
#' @description Function to convert [tables] \code{tabular} object to [knitr] \code{kable} object.
#' @param tblr - [tables] \code{tabular} object to convert
#' @param col_spec - vector of column ids to apply "border_right" to using [kableExtra::column_spec()]
#' @param isHTM - flag indicating whether output is for html (any value other than FALSE indicates TRUE; default=NULL)
#' @param isPDF - flag indicating whether output is for pdf (default=NULL; NULL indicates FALSE)
#' @param scale_down - flag to scale font size down to fit table within margins (works only for pdf, i.e. \code{isHTM=FALSE})
#' @param ltx_font_size - font size for latex table (NULL gives default font size)
#' @param replaceNAs - string to replace NAs with (default=NULL does nothing)
#' @param adjColSpacing - value to adjust latex column spacing by (default=0; default column spacing = 6)
#' @return [knitr] \code{kable} object
#' @importFrom tables toKable
#' @importFrom tables table_options
#' @importFrom kableExtra row_spec
#' @importFrom kableExtra column_spec
#' @importFrom kableExtra kable_styling
#' @details If \code{isHTM} and \code{isPDF} are both NULL (the defaults), the conversion will be to html.
#' @md
#' @export
convert_tblr_to_kbl<-function(tblr,col_spec,isHTM=NULL,isPDF=NULL,
                              scale_down=FALSE,
                              ltx_font_size=NULL,
                              replaceNAs=NULL,
                              adjColSpacing=0){
  nr = nrow(tblr);
  if (is.null(isHTM)&is.null(isPDF)) {
    isHTM = TRUE;#--default behavior
  } else {
    if (is.null(isHTM)) isHTM = is.null(isPDF)||(!isPDF);
  }
  if (isHTM){
    kbl = tblr |> tables::toKable(format="html",booktabs=TRUE) |>
                   kableExtra::row_spec(1:nr,align="c") |>
                   kableExtra::column_spec(col_spec,border_right=TRUE) |>
                   kableExtra::kable_styling(bootstrap_options="striped");
    if (!is.null(replaceNAs)){
      rpl = paste0(replaceNAs,"</td>");#--replacement for NAs
      txt=stringr::str_replace_all(kbl,stringr::fixed("NA</td>"),rpl);
      attributes(txt)<-attributes(kbl);
      kbl = txt;
    }
  } else {
    save = tables::table_options();                               #--saves default options
    tables::table_options(tabular="longtable",justification="r"); #--modifies tables::table_options()
    latex_options="repeat_header";
    if (nrow(tblr)>1) latex_options = c(latex_options,"striped");
    if (scale_down)   latex_options = c(latex_options,"scale_down");
    kbl = tblr |> tables::toKable(booktabs=FALSE) |>
                   kableExtra::kable_styling(latex_options=latex_options,
                                             font_size=ltx_font_size);
    if (!is.null(replaceNAs)){
      rpl = paste0(replaceNAs,"$");#--replacement for NAs
      txt=stringr::str_replace_all(kbl,stringr::fixed("NA$"),rpl);
      attributes(txt)<-attributes(kbl);
      kbl = txt;
    }
    if (adjColSpacing!=0){
      txt=paste0("\\addtolength{\\tabcolsep}{",adjColSpacing,"pt}",
                 as.character(kbl),
                 "\\addtolength{\\tabcolsep}{",-adjColSpacing,"pt}");
      attributes(txt)<-attributes(kbl);
      kbl = txt;
    }
    tables::table_options(save); #--restore default options
  }
  return(kbl);
}
