\name{Predefined Packages}
\alias{fontspecPackage}
\alias{xcolorPackage}
\alias{previewPackage}
\alias{tikzPackage}
\alias{tikzPicture}
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
tikzPackage(name="tikz", packages=NULL, bbox=TRUE)
tikzPicture(name="tikzPicture", packages=NULL, bbox=TRUE)
}
\arguments{
  \item{font}{The name of a font to use as the main font.  See details.}
  \item{name}{The name of the resulting package.}
  \item{packages}{A character list of TikZ package names.}
  \item{bbox}{Either a logical indicating whether to use (or ignore) the
    TikZ bounding box at the end of a TikZ picture,
    or a numeric vector of 4 values (left, bottom, right, top)
    describing the bounding box to use for the TikZ picture.}
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

  A \code{"tikz"} package is available for including TikZ pictures.

  The \code{"tikzPicture"} package is provided for convenience if the
  LaTeX code only consists of TikZ code.
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

