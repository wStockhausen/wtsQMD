% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isFunctions.R
\name{isKnitr}
\alias{isKnitr}
\title{Check if document is being rendered with knitr}
\usage{
isKnitr()
}
\value{
TRUE/FALSE
}
\description{
Check if document is being rendered with knitr
}
\details{
Checks to see if document is being rendered with \link{knitr}.

Checks if document is being rendered in context of function call by
checking is \link[knitr:fig_path]{knitr::fig_path} is -1 (returns FALSE) or not (returns TRUE).
}
