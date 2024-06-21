#--setup code for qmd file
  require(wtsQMD);

  if (!exists("testing")) testing=TRUE;

  #--determine folder paths
  child_path=fastmap::faststack();#--stack object with methods push, pop, peek
  root = knitr::opts_knit$get("root.dir");
  if (testing) cat("root = '",root,"'\n\n",sep="");
  if (is.null(root)||(root=="")){
    if (testing) cat("root is NULL or empty\n\n",sep="");
    #--for testing running individual chunks
    if (rstudioapi::isAvailable()) {#--use folder of active document
      dn = dirname(rstudioapi::getActiveDocumentContext()$path);
      if (testing) {
        cat("rstudioapi::isAvailable()==TRUE.\n")
        cat("Setting child_path to active document path '",dn,"'.\n\n",sep="");
      }
      child_path$push(dn);
    } else {
      if (testing) cat("rstudioapi::isAvailable() = FALSE. Setting child_path to '.'.\n\n")
      child_path$push(".");#--default to current folder
    }
  } else {
    if (testing) cat("Setting child_path to root.\n\n");
    child_path$push(root);#--set toplevel folder to root
  }
  if (testing){
    cat("In qmd_setup:\n\n");
    cat("root = '",root,"'\n",sep="");
    cat("peek = '",child_path$peek(),"'\n\n",sep="");
  }
  dirFigs = file.path(child_path$peek(),"figures",dirname(knitr::fig_path()));
  if(!dir.exists(dirFigs)) dir.create(dirFigs,recursive=TRUE);
  dirTbls = file.path(child_path$peek(),"tables",dirname(knitr::fig_path()));
  if(!dir.exists(dirTbls)) dir.create(dirTbls,recursive=TRUE);

  #--constants
  MILLION=1000000;
  THOUSAND=1000;     #--convert kg to t
  LBStoKG = 0.453592;#--convert lbs to kg

  #--rendering info
  isPDF = wtsQMD::isOutputPDF();
  isHTM = wtsQMD::isOutputHTML();

  #--default figure values (TODO: put these as param elements in yaml?)
  def_ext = "png";
  def_dpi = 100; #--default dpi
  def_asp = 1.4; #--default figure aspect (h = w/asp)
  def_wid = 8.0; #--default width, in inches
  if (isPDF){
    def_ext = "pdf";
    def_dpi = 100; #--default dpi
    def_asp = 2.0; #--default figure aspect (h = w/asp)
    def_wid = 6.5; #--default width, in inches
  }

  #--empty lists for tables, figures to be printed at end of document
  lstTbls = list(); #--will have elements of type list(lbl=lbl,cap=cap,tbl=tbl)
  lstFigs = list(); #--will have elements of type list(lbl=lbl,cap=cap,pth=pth,wid=def_wid,dpi=def_dpi)

