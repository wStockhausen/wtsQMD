#--print figures section from list in lstFigs
  nFigs = length(lstFigs);
  if (nFigs>0){
    ctr = 0;
    if (length(lstTbls)==0) cat("{{< pagebreak >}}\n\n");
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
          cat("::: {.cell}\n",sep="")
          cat("::: {.cell-output-display}\n");
          cat("![",lst$cap,"](",wtsUtilities::abs_to_rel(lst$pth,root),"){#",lst$lbl,sep="");
          if (!is.null(lst$wid)) cat(" width=",lst$wid*lst$dpi,sep="");
          if (!is.null(lst$hgt)) cat(" height=",lst$hgt*lst$dpi,sep="");
          cat("}\n");
          cat(":::\n");
          cat(":::\n\n");
          ctr = ctr+1;
          if (ctr!=last) cat("{{< pagebreak >}}\n\n");#--insert page break
        }
      } #--lst
    #}
    tmplst = list();
    for (nm in names(lstFigs)) {
      lst = lstFigs[[nm]];
      if (!("section" %in% names(lst)))
        tmplst[[nm]] = tibble::as_tibble(lst);
    }
    dfrFigs = dplyr::bind_rows(tmplst);
    readr::write_csv(dfrFigs,file.path(child_path$peek(),"q00_LabelInfoFigs.csv"));
    rm(ctr,tmplst,lst,dfrFigs);
  }
