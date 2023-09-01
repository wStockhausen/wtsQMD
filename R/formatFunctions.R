#' @title Apply a pretty number format to a mixed vector of numbers and strings
#' @description Function to apply a pretty format to a mixed vector of numbers and strings.
#' @param x - vector (numeric or character) of numbers to apply pretty format to
#' @param d - number of significant digits to keep
#' @param n - number of "small" digits
#' @param sci - flag for scientific notation (default = FALSE)
#' @return character vector
#' @details Character strings that do not represent a number are returned 'as is'.
#' For numbers (or strings that can be converted to numbers using [strinpNumSyms()]),
#' [base::format()] with format='f', big.mark=',',digits=\code{d}, and nsmall=\code{n} is
#' applied to the numeric value.
#' \code{d} determines the minimum number of significant digits to be printed. All digits to
#' the left of the decimal place will be printed; if necessary, digits to the right of the decimal place
#' will be printed to make up \code{d} total digits.
#' \code{n} determines the minimum number of digits printed to the right of the decimal place, regardless
#' of \code{d}.
#' NAs are represented as "--".
#'
#' @export
#'
num<-function(x,d=1,n=0,sci=FALSE){
  nx = length(x);
  r = as.character(x);
  xp = stripNumSyms(x);#--strip any symbols
  for (i in 1:nx) {
    y = xp[i];
    if (is.na(y)) {
      r[i] = x[i];
    } else {
      r[i] = format(y,format="f",big.mark=",",digits=d,nsmall=n,scientific=sci);
    }
  }
  r[is.na(r)] = "--";
  return(r);
}

#'
#' @title Format numeric 'x' to specified number of digits
#' @description Function to format numeric 'x' to specified number of digits.
#' @param x - numeric vector
#' @param d - number of significant digits for output (default = 0)
#' @return character vector with formatted values
#'
#' @details Shortcut to [base::formatC()] function using format='f', big.mark=',',digits=\code{d}
#'
#' @export
#'
fmtT<-function(x,d=0){
  formatC(x,digits=d,big.mark=",",format="f");
}

#'
#' @title Determine Tier level from stock status value
#' @description Function to determine Tier level from stock status value.
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

#'
#' @title Convert years to crab year strings
#' @description Function to convert year to crab year string.
#' @param year - year (numeric or character) to convert
#' @return character vector with years in format YYYY/YY+1.
#' @details crab year is in format YYYY/YY+1.
#' @export
crabYear<-function(year){
  warn = getOption("warn");
  options(warn=-1);
    year = as.numeric(year);
    str<-paste(year,'/',wtsUtilities::formatZeros((year+1)%%100,2),sep='');
  options(warn=warn);
  return(str);
}

#'
#' @title Strip number symbols ("$",",") from a number
#' @description Function to strip number symbols ("$",",") from a number.
#' @param x - (character or numeric) vector to strip symbols from
#' @return numeric vector.
#' @details Strips dollar symbols and comma delineators from a vector of formatted numbers.
#' Simply passes \code{x} through if it is already numeric.
#'
#' @importFrom stringr fixed
#' @importFrom stringr str_replace_all
#'
#' @export
#'
stripNumSyms<-function(x){
  if (is.numeric(x)) return(x);
  warn = getOption("warn");
  options(warn=-1);
  x = as.character(x) |>
       stringr::str_replace_all(stringr::fixed(","),"") |>
       stringr::str_replace_all(stringr::fixed("$"),"") |>
       as.numeric();
  options(warn=warn);
  return(x);
}

#' @title Apply a pretty format for numbers in metric tons
#' @description Function to apply a pretty format for numbers in metric tons.
#' @param x - vector (numeric or character) of numbers to apply pretty format to
#' @return character vector of formatted numbers, with ' t' appended to indicate units
#'
#' @details Values >= 1000 are rounded to single ones, values > 100 are formatted
#' with 1 decimal places, values >10 are formatted with 2 decimal places,
#' values > 0 are formatted with 3 decimal places.
#' @export
numT<-function(x){
  if (is.character(x)) x = stripNumSyms(x);
  if (x>=1e3) return(paste0(num(x,n=0)," t"));
  if (x>=1e2) return(paste0(num(x,n=1)," t"));
  if (x>=1e1) return(paste0(num(x,n=2)," t"));
  if (x>0)    return(paste0(num(x,n=3)," t"));
  return(paste0(prettyNum(x)," t"));
}

#' @title Apply a pretty format for numbers in 1,000's of metric tons
#' @description Function to apply a pretty format for numbers in 1,000's of metric tons.
#' @param x - vector (numeric or character) of numbers to apply pretty format to
#' @return character vector
#' @details Values >= 1000 thousand t are rounded to single ones, values > 100 thousand t are formatted
#' with 1 decimal place, values > 10 are formatted with 2 decimal places, values > 0
#' are formatted with 3 decimal places.
#' @export
numKT<-function(x){
  if (x>=1e3) return(paste0(formatC(x,format="f",big.mark=",",digits=0)," thousand t"));
  if (x>=1e2) return(paste0(formatC(x,format="f",big.mark=",",digits=1)," thousand t"));
  if (x>=1e1) return(paste0(formatC(x,format="f",big.mark=",",digits=2)," thousand t"));
  if (x>0)    return(paste0(formatC(x,format="f",big.mark=",",digits=3)," thousand t"));
  return(paste0(prettyNum(x)," thousand t"));
}

#' @title Apply a pretty format for numbers in millions
#' @description Function to apply a pretty format for numbers in millions.
#' @param x - vector (numeric or character) of numbers to apply pretty format to
#' @return character vector
#' @details Values >= 10000 are rounded to single ones,
#' values >= 1000 are formatted with 1 decimal place,
#' values >= 100 are formatted with 2 decimal places,
#' values < 100 are formatted with 3 decimal places.
#' @export
numM<-function(x){
  if (x>=1e4)  return(paste0(formatC(x,format="f",big.mark=",",digits=0)," million"));
  if (x>=1e3)  return(paste0(formatC(x,format="f",big.mark=",",digits=1)," million"));
  if (x>=1e2)  return(paste0(formatC(x,format="f",big.mark=",",digits=2)," million"));
  if (x>=1e1)  return(paste0(formatC(x,format="f",big.mark=",",digits=3)," million"));
  if (x>0)     return(paste0(formatC(x,format="f",big.mark=",",digits=3)," million"));
  return(paste0(prettyNum(x)," million"));
}

#' @title Apply a pretty format to numbers in kg, converted to t
#' @description Function to apply a pretty format to numbers in kg, converted to t.
#' @param x - numeric or character vector in kg
#' @return - character vector in t, formatted using [numT()]
#' @details - Uses [stripNumSyms()] (if necessary) and [numT()].
#' @export
#'
numKGinT<-function(x){
  if (is.character(x)) x = stripNumSyms(x);
  x = x/1000;
  return(numT(x));
}

#' @title Apply a pretty format to numbers in 1000s t, converted to t
#' @description Function to apply a pretty format to numbers in 1000s, converted to t.
#' @param x - numeric or character vector in 1000s t
#' @return - character vector in t, formatted using [numT()]
#' @details - Uses [stripNumSyms()] (if necessary) and [numT()].
#' @export
#'
numKTinT<-function(x){
  if (is.character(x)) x = stripNumSyms(x);
  return(numT(1000*x));
}

#' @title Apply a pretty format to numbers in t, converted to 1000s t
#' @description Function to apply a pretty format to numbers in t, converted to 1000s t.
#' @param x - numeric or character vector in t
#' @return - character vector in 1000s t, formatted using [numKT()]
#' @details - Uses [stripNumSyms()] (if necessary) and [numKT()].
#' @export
#'
numTinKT<-function(x){
  if (is.character(x)) x = stripNumSyms(x);
  return(numKT(x/1000));
}


