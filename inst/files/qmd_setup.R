#--setup code for qmd file
  require(wtsQMD);

  #--determine folder paths
  root = knitr::opts_knit$get("root.dir");
  if (is.null(root))
    root = ".";#--for testing running individual chunks
  if (rstudioapi::isAvailable())
    root = dirname(rstudioapi::getActiveDocumentContext()$path);
  dirFigs = file.path(root,"figures",dirname(knitr::fig_path()));
  if(!dir.exists(dirFigs)) dir.create(dirFigs,recursive=TRUE);
  dirTbls = file.path(root,"tables",dirname(knitr::fig_path()));
  if(!dir.exists(dirTbls)) dir.create(dirTbls,recursive=TRUE);

  child_path=fastmap::faststack();#--stack object with methods push, pop, peek
  child_path$push(root);#--set toplevel folder

  #--constants
  MILLION=1000000;
  THOUSAND=1000;#--convert kg to t

  #--rendering info
  isPDF = wtsQMD::isOutputPDF();
  isHTM = wtsQMD::isOutputHTML();

  #--default figure values (TODO: put these as param elements in yaml?)
  def_ext = "pdf";
  def_dpi = 100; #--default dpi
  def_asp = 2.0; #--default figure aspect (h = w/asp)
  def_wid = 6.5; #--default width, in inches
  if (isHTM){
    def_ext = "png";
    def_dpi = 100; #--default dpi
    def_asp = 1.4; #--default figure aspect (h = w/asp)
    def_wid = 8.0; #--default width, in inches
  }

  #--empty lists for tables, figures to be printed at end of document
  lstTbls = list(); #--will have elements of type list(lbl=lbl,cap=cap,tbl=tbl)
  lstFigs = list(); #--will have elements of type list(lbl=lbl,cap=cap,pth=pth,wid=def_wid,dpi=def_dpi)

