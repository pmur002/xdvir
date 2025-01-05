\name{preview Package}
\alias{previewPackage}
\title{
  LaTeX Package preview.
}
\description{
  Define a LaTeXPackage for the LaTeX preview package.
}
\usage{
previewPackage()
}
\details{
  This function creates a \code{"LaTeXpackage"} object
  that provides support for the LaTeX preview package.

  This adds a \code{"preview-baseline"} anchor to use for
  aligning the rendered LaTeX in R.

  A predefined package is pre-registered
  under the name \code{"preview"}.
}
\section{Warning}{
  This package wraps LaTeX snippets in a preview environment, which
  may not work for complex LaTeX snippets.
}
\value{
  A \code{"LaTeXpackage"} object.
}
\examples{
cat(author("test", packages="preview"), sep="\n")
}
\author{
  Paul Murrell
}
\keyword{ dplot }

