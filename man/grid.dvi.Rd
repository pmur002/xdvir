\name{grid.dvi}
\alias{grid.dvi}
\alias{render}
\alias{dviGrob}
\alias{dviGrob.DVI}
\alias{dviGrob.character}
\alias{dviGrob.list}
\title{
  Render DVI File in R
}
\description{
  Render a DVI file in R graphics.
}
\usage{
dviGrob(dvi, \dots) 
\method{dviGrob}{character}(dvi, \dots)
\method{dviGrob}{DVI}(dvi, ...,
        packages=NULL,
        engine=getOption("xdvir.engine"))
\method{dviGrob}{list}(dvi, x = 0.5, y = 0.5,
        margin=0, rot=0,
        default.units = "npc",
        hjust="centre", vjust="centre",
        dpi=NA,
        page=1,
        packages=NULL,
        engine=getOption("xdvir.engine"),
        fontLib=getOption("xdvir.fontLib"),
        \dots,
        name=NULL,
        gp=gpar(),
        vp=NULL)
grid.dvi(\dots)
render(\dots)
}
\arguments{
  \item{dvi}{A \code{"DVI"} object, as produced by
    \code{\link{readDVI}}, or a \code{"DVIfile"} object, as produced by
    \code{\link{typeset}}, or the name of a DVI file, or a list
    containing some combination of all three.}
  \item{x, y}{Numeric values or units specifying where to draw the
    output.}
  \item{margin}{Numeric values or units specifying margins (in the order
    bottom, left, top, right).  Recycled if necessary.}
  \item{rot}{Rotation angle (in degrees).}
  \item{default.units}{Units to use if \code{x} or \code{y} are
    numeric.}
  \item{hjust, vjust}{Justification of the output relative to the
    \code{x}/\code{y} location.}
  \item{dpi}{Resolution (dots per inch) for rendering.}
  \item{page}{Which page should be drawn.}
  \item{engine}{The TeX engine that should be used to render the DVI
    file (see Details).}
  \item{fontLib}{The font libraary the should be used to query fonts
    and glyphs.}
  \item{packages}{The LaTeX packages to be used in rendering the DVI.}
  \item{name}{Character value giving name for the grob.}
  \item{gp}{Graphical parameter settings.}
  \item{vp}{A viewport or \code{NULL}.}
  \item{\dots}{
    Arguments specific to methods of \code{dviGrob}.
  }
}
\details{
  If the \code{engine} is \code{NULL} (the default), one is chosen based
  on the engine attribute of the \code{dvi} input (if an engine
  of that name has been registered).

  If the \code{engine} is specified, but does not match the engine
  attribute of the \code{dvi} then, if the \code{dvi} engine was
  guessed the \code{engine} will be used, otherwise the conflict will
  result in an error.

  \code{render()} is an alias for \code{grid.dvi()}.
}
\value{
  A \code{"DVIgrob"} object.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{readDVI}}, 
  \code{\link{LaTeXpackage}}.
}
\keyword{ dplot }

