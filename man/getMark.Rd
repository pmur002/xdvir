\name{getMark}
\alias{getMark}
\alias{addMark}
\title{
  Access LaTeX Marks
}
\description{
  \code{getMark()}
  allows access a location within LaTeX output relative to the rendering
  in R.

  \code{addMark()} allows third-party packages to add marks to the
  rendering in R.
}
\usage{
getMark(name)
addMark(name, devx, devy, vpx=NA, vpy=NA, vpPath=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{The name of a mark.}
  \item{devx, devy}{The location of the mark on the device (in inches).}
  \item{vpx, vpy}{The location of the mark relative to the viewport in
    which the mark was rendered.}
  \item{vpPath}{The viewport path to the viewport in which the mark
    was rendered.}
}
\details{
  Some LaTeX packages, e.g., see \code{\link{zrefPackage}},
  allow positions within text to be
  saved with a label.  This function allows those saved locations
  to be accessed, e.g., to add further drawing relative to those locations.
}
\section{Warning}{
  The saved locations are only relative to the current device size.
  Resizing the device, or copying between devices will result in
  incorrect locations.

  A call to \code{addMark()} using an existing mark name will
  overwrite the previous mark information.
}
\value{
  \code{getMark} returns a list containing the location (and viewport)
  information for the mark.
}
\author{
  Paul Murrell
}
\keyword{ dplot }
