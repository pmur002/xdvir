\name{zref Package}
\alias{zrefPackage}
\title{
  LaTeX Package zref.
}
\description{
  Define a LaTeXPackage for the LaTeX zref package.
}
\usage{
zrefPackage()
}
\details{
  This function creates a \code{"LaTeXpackage"} object
  that provides support for the LaTeX zref package.

  This allows commands like \code{\\zsavepos{label}}
  to be included in LaTeX snippets when calling
  \code{\link{grid.latex}}.

  A new command, \code{\\xdvirzmark{label}}, is defined
  to allow saved positions also to be recorded in the rendered LaTeX in
  R.  See the \code{\link{getMark}} function.
  
  A predefined package is pre-registered
  under the name \code{"zref"}.
}
\value{
  A \code{"LaTeXpackage"} object.
}
\examples{
cat(author("test", packages="zref"), sep="\n")
}
\author{
  Paul Murrell
}
\keyword{ dplot }

