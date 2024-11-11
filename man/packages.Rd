\name{Predefined Packages}
\alias{fontspecPackage}
\alias{xcolorPackage}
\alias{previewPackage}
\title{
  LaTeX Package fontspec.
}
\description{
  Define a LaTeXPackage for the LaTeX fontspec package.
}
\usage{
fontspecPackage(font=NULL, name=font)
xcolorPackage()
previewPackage()
}
\arguments{
  \item{font}{The name of a font to use as the main font.  See details.}
  \item{name}{The name of the resulting package.}
}
\details{
  A \code{"preview"} package is available for setting a baseline
  anchor.
  
  An \code{"xcolor"} package is available for setting colours.
  
  A \code{"fontspec"} package is available for selecting fonts.
  
  This function provides some convenience for also setting the main
  font to be used.  The \code{name} should be the common name of
  a system font.

  For more complex situations, fontspec commands can be added to
  the LaTeX code that is sent to functions like
  \code{\link{author}} and \code{\link{grid.latex}} (see the Examples).
}
\value{
  These functions return \code{"LaTeXpackage"} objects.
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

