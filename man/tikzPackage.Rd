\name{tikz Package}
\alias{tikzPackage}
\alias{tikzPicture}
\title{
  LaTeX Package tikz.
}
\description{
  Define a LaTeXPackage for the LaTeX tikz package.
}
\usage{
tikzPackage(name="tikz", packages=NULL, bbox=TRUE, quote=TRUE)
tikzPicture(name="tikzPicture", packages=NULL, bbox=TRUE, quote=TRUE)
}
\arguments{
  \item{name}{The name of the resulting package.}
  \item{packages}{A character list of TikZ package names.}
  \item{bbox}{Either a logical indicating whether to use (or ignore) the
    TikZ bounding box at the end of a TikZ picture,
    or a numeric vector of 4 values (left, bottom, right, top)
    describing the bounding box to use for the TikZ picture.}
  \item{quote}{Whether to place single quotes around the path to
    the pgfsysdriver file.  This might help if the path to the
    R installation contains spaces.  Conversely, at least some LuaTeX
    versions require this to be \code{FALSE}.}
}
\details{
  This function creates a \code{"LaTeXpackage"} object
  that provides support for the LaTeX xcolor package.

  This allows TikZ pictures to be included in LaTeX snippets.
  when calling \code{\link{grid.latex}}.

  The \code{"tikzPicture"} package is provided for convenience if the
  LaTeX snippet only consists of TikZ code.
  
  Two predefined packages are pre-registered
  under the names \code{"tikz"} and \code{"tikzPicture"}.
}
\note{
  If \code{"tikzmark"} is one of the \code{packages}, then a new
  command, \code{\\xdvirtikzmark{label}}, is defined 
  to allow saved positions also to be recorded in the rendered LaTeX in
  R.  This produces \code{\link[grid]{nullGrob}} objects at the
  relevant locations plus anchors (for justification) at the relevant
  locations.
}
\value{
  A \code{"LaTeXpackage"} object.
}
\examples{
cat(author("test", packages="tikz"), sep="\n")
cat(author("test", packages="tikzPicture"), sep="\n")
}
\author{
  Paul Murrell
}
\keyword{ dplot }

