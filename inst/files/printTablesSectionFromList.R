#--print tables section from list in lstTbls
  if (!exists("reorderTables")) reorderTables = FALSE;
  nTbls = length(lstTbls);
  if (nTbls>0){
    if (reorderTables){
      fn = "r_reorderTbls.csv";
      if (file.exists(fn)){
        dfr = readr::read_csv(fn);
        nr = nrow(dfr);
        if (nr!=nTbls) {
          warning("Cannot reorder tables: incompatible number of tables!");
        } else {
          lstTbls = lstTbls[dfr$lbl];
        }
      } else warning("Tables not reordered. 'r_reorderTbls.csv' dos not exist.")
    }
    ctr = 0;
    cat("{{< pagebreak >}}\n\n")
    if (knitr::is_latex_output()) cat("\\FloatBarrier\n\n");
    cat("# Tables {-}\n\n")
    if (knitr::is_latex_output()) cat("\\listoftables\n\n\\FloatBarrier\n\n")
    #if (knitr::is_latex_output()) cat("\\FloatBarrier\n\n");
    for (t in 1:nTbls){
      lst = lstTbls[[t]];
      if ("section" %in% names(lst)){
          cat("\n\n##",lst$section,"{-}\n\n\\FloatBarrier\n\n");
          ctr = ctr+1;
      } else {
        lscp = knitr::is_latex_output() && !(is.null(lst$ori)||(stringr::str_starts(tolower(lst$ori),"p")));
        if (lscp) {
          # cat("\\KOMAoptions{paper=landscape,pagesize}\n",
          #     "\\recalctypearea\n\n");
          # cat("\\begin{landscape}\n\n");
        }
        cap = escapeChars(lst$cap);
        cat("::: {#",lst$lbl," .cell tbl-cap='",cap,"'}\n",sep="")
        cat("::: {.cell-output-display}\n");
        if (knitr::is_html_output()){
            cat("`````{=html}\n")
            cat(lst$tbl); cat("\n");
            cat("`````\n\n");
        } else if (knitr::is_latex_output()){
          lscp = !(is.null(lst$ori)||(stringr::str_starts(tolower(lst$ori),"p")));
          #cat("lscp=",lscp,"lst$ori=",lst$ori,"\n\n")
          if (!is.null(lst$fontsize)) cat(paste0("\\begin{",lst$fontsize,"}\n\n"));
          if (lscp) cat("\\begin{landscape}\n\n");
          cat(lst$tbl); cat("\n");
          if (lscp) cat("\\end{landscape}\n\n");
          if (!is.null(lst$fontsize)) cat(paste0("\\end{",lst$fontsize,"}\n\n"));
        }
        cat(":::\n");
        cat(":::\n\n");
        if (knitr::is_latex_output()) {
          cat("{{< pagebreak >}}\n\n");
          # if (!is.null(lst$ori)) {
          #   # cat("\\end{landscape}\n\n");
          #   # cat("\\KOMAoptions{paper=portrait,pagesize}\n",
          #   #     "\\recalctypearea\n\n");
          # }
        }#--latex?
      }#--section?
    }#--lst
    if (knitr::is_latex_output()) cat("\\FloatBarrier\n\n");
    tmplst = list();
    for (t in 1:nTbls) {
      lst = lstTbls[[t]];
      if (!("section" %in% names(lst)))
        tmplst[[t]] = tibble::as_tibble(lst) |> dplyr::select(!tbl);
    }
    dfrTbls = dplyr::bind_rows(tmplst);
    readr::write_csv(dfrTbls,file.path(child_path$peek(),"csv_ListForTablesInfo.csv"));
    wtsUtilities::saveObj(lstTbls,file.path(child_path$peek(),"rda_ListForTablesInfo.RData"));
    rm(tmplst,lst,dfrTbls);
  }
