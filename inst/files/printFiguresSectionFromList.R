#--print figures section from list in lstFigs
if (!exists("reorderFigures")) reorderFigures = FALSE;
  nFigs = length(lstFigs);
  if (nFigs>0){
    if (reorderFigures){
      fn = "r_reorderFigs.csv";
      if (file.exists(fn)){
        dfr = readr::read_csv(fn);
        nr = nrow(dfr);
        if (nr!=nFigs) {
          warning("Cannot reorder figures: incompatible number of figures!");
        } else {
          lstFigs = lstFigs[dfr$lbl];
        }
      } else warning("Tables not reordered. 'r_reorderFigs.csv' dos not exist.")
    }
    ctr = 0;
    if (length(lstTbls)==0) cat("{{< pagebreak >}}\n\n");
    if (knitr::is_latex_output()) cat("\\FloatBarrier\n\n");
    cat("# Figures {-}\n\n")
    if (knitr::is_latex_output()) cat("\\listoffigures\n\n\\FloatBarrier\n\n")
    #if (knitr::is_html_output()){
      last = length(lstFigs);
      ctr = 0;
      for (f in 1:nFigs){
        lst = lstFigs[[f]];
        if ("section" %in% names(lst)){
          cat("\n\n##",lst$section,"{-}\n\n\\FloatBarrier\n\n");
          ctr = ctr+1;
        } else {
          lscp = knitr::is_latex_output()&&(!(is.null(lst$ori)||(stringr::str_starts(tolower(lst$ori),"p"))));
          if (lscp) cat("\\landscape\n\n");
          cat("::: {.cell}\n",sep="")
          cat("::: {.cell-output-display}\n");
          cat("![",lst$cap,"](",wtsUtilities::abs_to_rel(lst$pth,root),"){#",lst$lbl,sep="");
          if (!is.null(lst$wid)) cat(" width=",lst$wid*lst$dpi,sep="");
          if (!is.null(lst$hgt)) cat(" height=",lst$hgt*lst$dpi,sep="");
          cat("}\n");
          cat(":::\n");
          cat(":::\n\n");
          if (lscp) cat("\\endlandscape\n\n");
          ctr = ctr+1;
          if (ctr!=last) cat("{{< pagebreak >}}\n\n");#--insert page break
        }
      } #--lst
    if (knitr::is_latex_output()) cat("\\FloatBarrier\n\n");
    #}
    tmplst = list();
    for (nm in names(lstFigs)) {
      lst = lstFigs[[nm]];
      if (!("section" %in% names(lst)))
        tmplst[[nm]] = tibble::as_tibble(lst);
    }
    dfrFigs = dplyr::bind_rows(tmplst);
    readr::write_csv(dfrFigs,file.path(child_path$peek(),"r_ListForFiguresInfo.csv"));
    wtsUtilities::saveObj(lstFigs,file.path(child_path$peek(),"r_ListForFigures.RData"));
    rm(ctr,tmplst,lst,dfrFigs);
  }
