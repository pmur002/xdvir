\name{xcolor Package}
\alias{xcolorPackage}
\title{
  LaTeX Package xcolor.
}
\description{
  Define a LaTeXPackage for the LaTeX xcolor package.
}
\usage{
xcolorPackage()
}
\details{
  This function creates a \code{"LaTeXpackage"} object
  that provides support for the LaTeX xcolor package.

  This allows commands like \code{\\color{blue}}
  to be included in LaTeX snippets when calling
  \code{\link{grid.latex}}.
  
  A predefined package is pre-registered
  under the name \code{"xcolor"}.
}
\value{
  A \code{"LaTeXpackage"} object.
}
\examples{
cat(author("test", packages="xcolor"), sep="\n")
}
\author{
  Paul Murrell
}
\keyword{ dplot }

