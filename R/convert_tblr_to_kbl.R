#'
#' @title Convert [tables] \code{tabular} object to [knitr] \code{kable} object
#' @description Function to convert [tables] \code{tabular} object to [knitr] \code{kable} object.
#' @param tblr - [tables] \code{tabular} object to convert
#' @param col_spec - vector of column ids to apply "border_right" to using [kableExtra::column_spec()]
#' @param isHTM - flag indicating whether output is for html (or pdf)
#' @return [knitr] \code{kable} object
#' @importFrom tables toKable
#' @importFrom tables table_options
#' @importFrom kableExtra row_spec
#' @importFrom kableExtra column_spec
#' @importFrom kableExtra kable_styling
#' @details TBD.
#' @md
#' @export
convert_tblr_to_kbl<-function(tblr,col_spec,isHTM){
  nr = nrow(tblr);
  if (isHTM){
    kbl = tblr |> tables::toKable(format="html",booktabs=TRUE) |>
                   kableExtra::row_spec(1:nr,align="c") |>
                   kableExtra::column_spec(col_spec,border_right=TRUE) |>
                   kableExtra::kable_styling(bootstrap_options="striped");
  } else {
    save = tables::table_options();                               #--saves default options
    tables::table_options(tabular="longtable",justification="r"); #--modifies tables::table_options()
    kbl = tblr |> tables::toKable(booktabs=FALSE) |>
                   kableExtra::kable_styling(latex_options=c("striped","repeat_header"));
    tables::table_options(save); #--restore default options
  }
  return(kbl);
}
