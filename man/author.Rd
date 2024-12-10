\name{author}
\alias{author}
\title{
  Generate a LaTeX Document
}
\description{
  Generate a LaTeX document from a LaTeX fragment.
}
\usage{
author(tex,
       width=NA,
       engine=getOption("xdvir.engine"),
       packages=NULL)
}
\arguments{
  \item{tex}{LaTeX code.  See Details.}
  \item{width}{Either \code{NA} or a numeric value.
    The latter is assumed to be a number of inches.}
  \item{engine}{The TeX engine that should be used for authoring.
    May be the name of a TeX engine (character)
    or a \code{"TeXengine"} object.}
  \item{packages}{The LaTeX packages to be used.}
}
\details{
  \code{author()} can be used to generate a complete LaTeX document
  from a LaTeX fragment, either as a character vector or an external file.
}
\value{
  A \code{"LaTeXdocument"} object.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{LaTeXpackage}}.
}
\examples{
author("this is a test")
}
\keyword{ dplot }

