#'
#' @title Convert dataframe object to [knitr] \code{kable} object
#' @description Function to convert dataframe object to [knitr] \code{kable} object.
#' @param dfr - dataframe to convert
#' @param border_left - vector of column ids to apply "border_left" to using [kableExtra::column_spec()]
#' @param border_right - vector of column ids to apply "border_right" to using [kableExtra::column_spec()]
#' @param booktabs - flag to use booktabs format
#' @param longtable - flag to use longtable format (latex output only)
#' @param replaceNAs - string to replace NAs with (default=NULL does nothing)
#' @param col_spec - vector of column ids to apply "border_right" to using [kableExtra::column_spec()]
#' @param position - character indicating position (latex output only: "h", "t","p")
#' @param digitis - max number of digits for numeric columns. Can also be a vector of length ncol(dfr).
#' @param format.args - passed on to [kableExtra::kbl()]
#' @param scale_down - flag to scale font size down to fit table within margins (latex output only)
#' @param ltx_font_size - font size for latex table (NULL gives default font size)
#' @param adjColSpacing - value to adjust latex column spacing by (default=0; default column spacing = 6)
#' @return [knitr] \code{kable} object
#' @importFrom kableExtra row_spec
#' @importFrom kableExtra column_spec
#' @importFrom kableExtra kable_styling
#' @details The conversion to html or latex is determined by [knitr::is_latex_output()].
#' @md
#' @export
doKbl<-function(dfr,
                border_left=NULL,
                border_right=NULL,
                booktabs=TRUE,
                longtable=TRUE,
                replaceNAs=NULL,
                position="h",
                digits=getOption("digits"),
                format.args=list(big.mark=","),
                bootstrap_options=c("striped","condensed"),
                latex_options=c("striped","repeat_header"),
                ltx_font_size=NULL,
                scale_down=TRUE,
                adjColSpacing=0){
  nc = ncol(dfr);
  nr = nrow(dfr);
  if (scale_down) latex_options = c(latex_options,"scale_down");
  kbl = dfr |>
            kableExtra::kbl(booktabs=booktabs,digits=digits,format.args=format.args,
                            position=position,longtable=longtable) |>
            kableExtra::kable_styling(bootstrap_options=bootstrap_options,
                                      latex_options=latex_options);
  if (knitr::is_latex_output()){
    ##--latex specs----
    kbl = kbl |> kableExtra::kable_styling(font_size=ltx_font_size);
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
  } else {
    ##--html specs----
    kbl  = kbl|> kableExtra::row_spec(1:nr,align="c");
    if (!is.null(border_right)) #--add border to right of columns identified by index
      kbl = kbl |> kableExtra::column_spec(border_right,border_right=TRUE);
    if (!is.null(border_left)) #--add border to left of columns identified by index
      kbl = kbl |> kableExtra::column_spec(border_left,border_left=TRUE);
    if (!is.null(replaceNAs)){
      rpl = paste0(replaceNAs,"</td>");#--replacement for NAs
      txt=stringr::str_replace_all(kbl,stringr::fixed("NA</td>"),rpl);
      attributes(txt)<-attributes(kbl);
      kbl = txt;
    }
  }
  return(kbl);
}
