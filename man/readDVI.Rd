\name{readDVI}
\alias{readDVI}
\title{
  Read DVI File
}
\description{
  Read a DVI file (produced by LaTeX) into R. 
}
\usage{
readDVI(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{A character value giving the name of a DVI file.}
}
\value{
  A \code{"DVI"} object.

  There is a print method that produces
  a nice human-readable format.
}
\details{
  A \code{"DVI"} object is a list of memory blocks (as produced by
  functions from 
  the \pkg{hexView} package), with one block per DVI operation.
      
  This is the detailed, byte-level contents of the DVI file.
}
\author{
  Paul Murrell
}
\examples{
readDVI(system.file("DVI", "test-pdftex.dvi", package="dvi"))
readDVI(system.file("DVI", "test-luatex.dvi", package="dvi"))
readDVI(system.file("DVI", "test-xetex.xdv", package="dvi"))
readDVI(system.file("DVI", "test-uptex.dvi", package="dvi"))
}
\keyword{ dplot }
