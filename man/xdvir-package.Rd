\docType{package}
\name{xdvir-package}
\alias{xdvir-package}
\alias{xdvir}
\title{xdvir: Render 'LaTeX' in Plots}
\description{
  High-level functions to render 'LaTeX' fragments in plots, 
  including as labels and data symbols in 'ggplot2' plots, plus
  low-level functions to 
  author 'LaTeX' fragments (to produce 'LaTeX' documents), 
  typeset 'LaTeX' documents (to produce 'DVI' files), 
  read 'DVI' files (to produce "DVI" objects), 
  and render "DVI" objects.
}
\section{Package options}{
  The default 'TeX' typesetting engine is taken from
  \code{getOption("xdvir.engine")} (currently one of
  \code{"luatex"} or \code{"xetex"}).

  Whether to use a DVI cache is taken from
  \code{getOption("xdvir.useDVIcache")}.
  This is \code{TRUE} by default, but setting it to \code{FALSE}
  will ensure that typesetting will always occur (even if the
  'LaTeX' fragment has been typeset before).

  Whether to substitute very thin 'TeX' rules (lines) with
  line segments (rather than rectangles) is taken from
  \code{getOption("xdvir.substituteRule")}.
  This is \code{NULL} by default, which is taken as \code{TRUE}
  (i.e., DO substitute), but can be set to \code{FALSE}
  to force all rules to be drawn as rectangles.
q}

\seealso{
\url{https://github.com/pmur002/xdvir}
}
\author{
\strong{Maintainer}: Paul Murrell \email{paul@stat.auckland.ac.nz} (\href{https://orcid.org/0000-0002-3224-8858}{ORCID})
}
\keyword{internal}