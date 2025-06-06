% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/printRGraphics.R
\name{printRGraphics}
\alias{printRGraphics}
\title{"Print" a base R graphics figure into a Quarto markdown document}
\usage{
printRGraphics(
  p,
  lbl = NULL,
  pth = NULL,
  cap = NULL,
  asp = NULL,
  wid = NULL,
  dpi = NULL,
  ori = "P",
  type = NULL,
  xtraLbl = NULL,
  xtraFigFN = xtraLbl,
  xtraCap = NULL,
  testing = FALSE
)
}
\arguments{
\item{p}{\itemize{
\item an \code{expression} yielding a base R graphics figure when evaluated (or NULL)
}}

\item{lbl}{\itemize{
\item figure label (OPTIONAL; should start with "fig-" if given)
}}

\item{pth}{\itemize{
\item path to saved figure (OPTIONAL)
}}

\item{cap}{\itemize{
\item figure caption (OPTIONAL)
}}

\item{asp}{\itemize{
\item figure aspect (height/width) (OPTIONAL)
}}

\item{wid}{\itemize{
\item figure width (inches) (OPTIONAL)
}}

\item{dpi}{\itemize{
\item dots-per-inch (OPTIONAL)
}}

\item{ori}{\itemize{
\item orientation ("P": portrait (default); "L": landscape)
}}

\item{type}{\itemize{
\item image file type (as string) compatible with \code{\link[ggplot2:ggsave]{ggplot2::ggsave()}}, if not NULL (e.g., "png", "pdf","svg")
}}

\item{xtraLbl}{\itemize{
\item extra string to add to label (helpful for printing multiple plots in the same chunk) (OPTIONAL)
}}

\item{xtraFigFN}{\itemize{
\item extra string to add to path to saved figure (defaults to \code{xtraLbl}) (OPTIONAL)
}}

\item{xtraCap}{\itemize{
\item extra string to add to caption. (OPTIONAL)
}}
}
\value{
NULL, if the \link{knitr} environment is not latex. Otherwise, a lstFigs list that should be
appended to the QMD's lstFigs list.
}
\description{
Function to "print" a base R graphics figure into a Quarto markdown document.
This is the R base graphics counterpart to \link{printGGplot}.
}
\details{
All inputs except \code{p}, the \code{expression} when evaluated that yields a base R graphics figure
to print/save, are optional. Values for \code{lbl}, \code{pth}, \code{cap}
\code{asp}, \code{wid}, and \code{dpi} that are not specified by the user will be taken from \link{knitr}'s
current chunk options.

If \code{p} is (explicitly) NULL, then \code{pth} should provide a path to a valid image file.

If \code{type} is not NULL, the plot will be output as the specified type at the path given. Potential values
are "png" or "pdf", as well as other options supported by \code{\link[ggplot2:ggsave]{ggplot2::ggsave()}}.
}
