\name{fontspec Package}
\alias{fontspecPackage}
\title{
  LaTeX Package fontspec.
}
\description{
  Define a LaTeXPackage for the LaTeX fontspec package.
}
\usage{
fontspecPackage(font=NULL, name=font)
}
\arguments{
  \item{font}{The name of a font to use as the main font.  See details.}
  \item{name}{The name of the resulting package.}
}
\details{
  This function creates a \code{"LaTeXpackage"} object
  that provides support for the LaTeX fontspec package.
  
  The \code{font} argument provides some convenience for setting the main
  font to be used.  The \code{font} can be the common name of
  a system font or a complete path to the font file.

  For more complex situations, fontspec commands can be added to
  the LaTeX code that is sent to functions like
  \code{\link{author}} and \code{\link{grid.latex}} (see the Examples).

  A predefined package, with no main font defined, is pre-registered
  under the name \code{"fontspec"}.
}
\value{
  A \code{"LaTeXpackage"} object.
}
\examples{
cat(author("test", packages="fontspec"), sep="\n")
cat(author("test", packages=fontspecPackage(font="Courier")), sep="\n")
tex <- "\\\\setmainfont{fontname.ttf}[Path=/path/to/font/]\ntest"
cat(author(tex, packages="fontspec"), sep="\n")
}
\author{
  Paul Murrell
}
\keyword{ dplot }

