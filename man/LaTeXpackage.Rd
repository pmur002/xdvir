\name{LaTeXpackage}
\alias{LaTeXpackage}
\alias{registerPackage}
\title{
  Define a LaTeX Package.
}
\description{
  Define and register a LaTeX package for authoring, typesetting,
  and rendering LaTeX documents.
}
\usage{
LaTeXpackage(name,
             preamble=NULL,
             prefix=NULL,
             suffix=NULL,
             special=NULL,
             init=NULL,
             final=NULL)
registerPackage(package)
}
\arguments{
  \item{name}{Character name for the package.}
  \item{preamble, prefix, suffix}{Character preamble, prefix, and suffix
    for authoring LaTeX documents.}
  \item{special}{Function for handling DVI specials during rendering.}
  \item{init, final}{Functions to initialise package before rendering
    and finalise after rendering.}
  \item{package}{A \code{"LaTeXpackage"} object.}
}
\details{
  For simple cases, all that is required is a package name
  and a preamble that just contains a \code{\\usepackage} command.
  See the example below.

  More complex cases may involve adding a \code{prefix} and
  a \code{suffix} that, for example, begins and ends a 'LaTeX'
  environment.

  Advanced cases may involve writing functions \code{init},
  \code{special}, and \code{final}, which are called to handle DVI
  specials.  The predefined
  support for 'TikZ' is a particularly complicated example.
}
\value{
  \code{LaTeXpackage()} returns a \code{"LaTeXpackage"} object.
}
\author{
  Paul Murrell
}
\examples{
LaTeXpackage("times", "\\\\usepackage{times}")
}
\keyword{ dplot }

