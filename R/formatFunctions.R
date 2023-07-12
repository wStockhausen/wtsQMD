#'
#' @title Format numeric 'x' to specified number of digits
#' @description Function to format numeric 'x' to specified number of digits.
#' @param x - numeric vector
#' @param digits - number of significant digits for output (default = 0)
#' @return character vector with formatted values
#' @details Shortcut to [base::formatC()] function
#'
#' @export
#'
fmtT<-function(x,digits=0){
  formatC(x,digits=digits,big.mark=",",format="f");
}

#'
#' @title Determine Tier level from stock status value
#' @description Function to etermine Tier level from stock status value.
#' @param status - stock status value (i.e., B/$B_{MSY}$)
#' @return string with tier level ("a", "b", or "c")
#' @details Tier level is determined from status by:
#'
#'
#' @export
#'
getTierLevel<-function(status){
  tier = "a";
  if (status < 1)    tier = "b";
  if (status < 0.25) tier = "c";
  return(tier);
}

#'
#' @title Determine whether stock is overfished based on stock status value
#' @description Function to whether stock is overfished based on stock status value.
#' @param status - stock status value (i.e., B/$B_{MSY}$)
#' @return string with overfished status ("overfished", "not overfished")
#' @details overfished designation is determined from status by:
#'
#'
#' @export
#'
getOverfished<-function(status){
  tier = "not overfished";
  if (status < 0.5) tier = "overfished";
  return(tier);
}

