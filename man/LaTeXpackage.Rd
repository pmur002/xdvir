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
\value{
  \code{LaTeXpackage()} returns a \code{"LaTeXpackage"} object.
}
\author{
  Paul Murrell
}
\keyword{ dplot }

