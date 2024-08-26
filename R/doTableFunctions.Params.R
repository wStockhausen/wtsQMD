#'
#' @title Create a tabular object for parameters-at-bounds for TCSAM02 models
#' @description Function to create a tabular object for parameters-at-bounds for TCSAM02 models.
#' @param dfr - dataframe from call to [rCompTCMs::extractMDFR.Results.ParametersAtBounds()]
#' @return tabular object
#'
#' @import dplyr
#' @import tables
#'
#' @export
#'
doTable.PsAtBs<-function(dfr){
  tmp = dfr |> dplyr::mutate(case=as.factor(case),
                              category=as.factor(category),
                              process=as.factor(process),
                              name=as.factor(name),
                              label=as.factor(label),
                              test=ifelse(test=="at upper bound",1,-1));
  tbl = tabular(category*process*name*label~case*(test*sum)*DropEmpty(empty="--",which=c("row","cell")),data=tmp);
  colLabels(tbl) = colLabels(tbl)[2,];
  return(tbl);
}

#'
#' @title Create a tabular object for parameter values for TCSAM02 models
#' @description Function to create a tabular object for parameter values for TCSAM02 models.
#' @param dfr - dataframe from call to [rCompTCMs::extractMDFR.Results.ParameterValues()]
#' @param ctgs -  parameter categories to select  (default=NULL; i.e., all)
#' @param prcs -  parameter processes to select   (default=NULL; i.e., all)
#' @param lbls -  parameter labels to select      (default=NULL; i.e., all)
#' @param nams - parameter names to select        (default=NULL; i.e., all)
#' @param grep_names - flag to select parameter names using grep
#' @param param_type - string ("number" or "vector")
#' @param dropPrcLabel - flag to drop process label (default=FALSE)
#' @return tabular object
#'
#' @importFrom stringr str_trim
#'
#' @import dplyr
#' @import tables
#'
#' @export
#'
doTable.ParamVals<-function(dfr,ctgs=NULL,prcs=NULL,lbls=NULL,nams=NULL,
                            grep_names=NULL,param_type="number",
                            dropPrcLabel=FALSE){
  tmp = dfr;
  if (!is.null(ctgs))  tmp = tmp |> dplyr::filter(category %in% ctgs) |> dplyr::mutate(category=factor(category,levels=ctgs));
  if (!is.null(prcs))  tmp = tmp |> dplyr::filter(process %in% prcs)  |> dplyr::mutate(process =factor(process,levels=prcs));
  if (!is.null(lbls))  tmp = tmp |> dplyr::filter(label %in% lbls)    |> dplyr::mutate(label   =factor(label,levels=lbls));
  if (!is.null(nams))  {
    #--starting with none selected, select those that match desired names
    idx1 = vector(mode="logical",length=nrow(tmp)); #--all FALSE
    for (nam in nams)
      if(substr(nam,1,1)!="!") {
        nmp = nam;
        idx1 = idx1 | (substr(stringr::str_trim(tmp$name),1,nchar(nam))==nam);
      }
    #--starting with none selected, select those that match undesired names
    idx2 = vector(mode="logical",length=nrow(tmp)); #--all FALSE
    for (nam in nams)
      if(substr(nam,1,1)=="!") {
        nmp = substr(nam,2,nchar(nam));
        idx2 = idx2 | (substr(stringr::str_trim(tmp$name),1,nchar(nam)-1)==nmp);
      }
    tmp = tmp |> dplyr::filter(idx1&(!idx2)) |> dplyr::mutate(name=factor(name));
  }

  if (!is.null(grep_names)) tmp = tmp |> dplyr::filter(grepl(grep_names,name));

  tmp = tmp |> dplyr::mutate(final_param_value=ifelse(abs(final_param_value)<0.00001,0,final_param_value),
                             stdv=ifelse(stdv<0.0001,0,stdv));
  tmp = tmp |> dplyr::mutate(final_param_value=signif(final_param_value,digits=5),
                             stdv=signif(stdv,digits=3));

  f1<-function(x){formatC(x,digits=4,format="g",flag="#")};
  f2<-function(x){formatC(x,digits=2,format="g",flag="#")};
  if (param_type=="number"){
    tmp = tmp |> dplyr::filter(stringr::str_trim(type) %in% c("param_init_number","param_init_bounded_number")) |>
             dplyr::select(case,process,label,type,name,estimate=final_param_value,`std. dev.`=stdv) |>
             dplyr::mutate(case=as.factor(case),
                           process=as.factor(process),
                           name=as.factor(name),
                           label=as.factor(label));
    if (dropPrcLabel) {
      tmp = tmp |> dplyr::select(!process);
      tbl = tabular(name*label~
                      case*(Heading(`est.`)*estimate*Format(f1())*sum+
                              Heading(`sd.`)*`std. dev.`*Format(f2())*sum)*
                      DropEmpty(empty="--",which=c("row","cell")),
                    data=tmp);
    } else {
      tbl = tabular(process*name*label~
                      case*(Heading(`est.`)*estimate*Format(f1())*sum+
                              Heading(`sd.`)*`std. dev.`*Format(f2())*sum)*
                      DropEmpty(empty="--",which=c("row","cell")),
                    data=tmp);
    }
  } else {
    tmp = tmp |> dplyr::filter(!(stringr::str_trim(type) %in% c("param_init_number","param_init_bounded_number"))) |>
             dplyr::select(case,process,label,name,index,estimate=final_param_value,`std. dev.`=stdv) |>
             dplyr::mutate(case=as.factor(case),
                           process=as.factor(process),
                           name=as.factor(name),
                           index=as.factor(index),
                           label=as.factor(label));
    if (dropPrcLabel) {
      tmp = tmp |> dplyr::select(!process);
      tbl = tabular(name*label*index~
                      case*(Heading(`est.`)*estimate*Format(f1())*sum+
                              Heading(`sd.`)*`std. dev.`*Format(f2())*sum)*
                      DropEmpty(empty="--",which=c("row","cell")),
                    data=tmp);
    } else {
      tbl = tabular(process*name*label*index~
                      case*(Heading(`est.`)*estimate*Format(f1())*sum+
                              Heading(`sd.`)*`std. dev.`*Format(f2())*sum)*
                      DropEmpty(empty="--",which=c("row","cell")),
                    data=tmp);
    }
  }
  colLabels(tbl) = colLabels(tbl)[c(2,3),];
  return(tbl);
}

