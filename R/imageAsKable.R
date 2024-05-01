#'
#' @title Allow an image to be printed as if a table (i.e., a kable object)
#' @description function to allow image to be printed as if a table (i.e. a kable object)
#' @param fn - filename of image to use as if a table
#' @return A character string if output is pdf, otherwise a kable object
#' @details This function allows a previously-created image to be printed as a kable object
#' in a Quarto (qmd) document.
#' @importFrom kableExtra column_spec
#' @importFrom kableExtra kbl
#' @importFrom tibble tibble
#'
#' @export
#'
imageAsKable<-function(fn){
  if (isOutputPDF()){
    # str = paste0("\\begin{table}[H]\n",
    #             "\\centering\n",
    #             "\\begin{tabular}{c}\n",
    #             "\\includegraphics{",fn,"}\n",
    #             "\\end{tabular}\n",
    #             "\\end{table}\n"
    #             );
    str = paste0("\\begin{tabular}{c}\n",
                "\\includegraphics{",fn,"}\n",
                "\\end{tabular}\n"
                );
    return(str);
  }
  #--default to use kableExtra
  dfr = tibble::tibble(img="");
  kbl = kableExtra::kbl(dfr,booktabs=TRUE,col.names="") |>
          kableExtra::column_spec(1,image=fn,width="100%");
  return(kbl);
}
