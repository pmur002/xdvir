\name{grid.latex}
\alias{grid.latex}
\alias{latexGrob}
\alias{grid.xelatex}
\alias{xelatexGrob}
\alias{grid.lualatex}
\alias{lualatexGrob}
\title{
  Render LaTeX in R
}
\description{
  Author, typeset, and render LaTeX in R graphics.
}
\usage{
latexGrob(tex,
          x=0.5, y=0.5,
          margin=0, rot=0,
          default.units="npc",
          hjust="centre", vjust="centre",
          width=NA,
          dpi=NA,
          page=1,
          packages=NULL,
          engine=getOption("xdvir.engine"),
          fontLib=getOption("xdvir.fontLib"),
          texFile=NULL,
          name=NULL,
          gp=gpar(),
          vp=NULL) 
grid.latex(\dots)

xelatexGrob(tex, \dots)
grid.xelatex(\dots)

lualatexGrob(tex, \dots)
grid.lualatex(\dots)

}
\arguments{
  \item{tex}{LaTeX code as a character vector.}
  \item{x, y}{Numeric values or units specifying where to draw the
    output.}
  \item{margin}{Numeric values or units specifying margins (in the order
    bottom, left, top, right).  Recycled if necessary.}
  \item{rot}{Rotation angle (in degrees).}
  \item{default.units}{Units to use if \code{x} or \code{y} are
    numeric.}
  \item{hjust, vjust}{Justification of the output relative to the
    \code{x}/\code{y} location.}
  \item{width}{Either \code{NA} or a numeric value or a unit
    specifying the width for typesetting.  \code{NA} means the natural width of
    \code{tex}.}
  \item{dpi}{Resolution (dots per inch) for rendering.}
  \item{page}{Which page should be drawn.}
  \item{packages}{The LaTeX packages to be used.
    May be the name of a LaTeX package (character) or a
    \code{"LaTeXpackage"} object.}
  \item{engine}{The TeX engine that should be used to render the LaTeX.
    May be the name of a TeX engine (character)
    or a \code{"TeXengine"} object.}
  \item{fontLib}{The font libraary the should be used to query fonts
    and glyphs.}
  \item{name}{Character value giving name for the grob.}
  \item{gp}{Graphical parameter settings.}
  \item{vp}{A viewport or \code{NULL}.}
  \item{texFile}{Name of a file to use for LaTeX code.}
  \item{\dots}{
    Arguments passed to \code{\link{dviGrob}}.
  }
}
\details{
  \code{grid.latex()} takes a LaTeX fragment, generates a LaTeX
  document, typesets it, reads the resulting DVI file and renders the
  result.

  \code{grid.xelatex()} is just a convenient front end for
  \code{grid.latex()}, with appropriate default \code{engine}.

  \code{grid.latex()} attempts to be smart about
  delaying typesetting until drawing time if necessary in order
  to get the correct context for \code{width} and \code{gp} settings.
  This means that, unless \code{gp} is set to \code{NULL} and
  \code{width} is set to either \code{NA} or an
  absolute unit (e.g., a number of inches), the calculation of, for
  example, the width of a grob will be less efficient
  because the typesetting has to be performed
  all over again.  
}
\value{
  A \code{"DVIgrob"} object.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{LaTeXpackage}}.
}
\examples{
\dontrun{
# Requires TeX installation
grid.latex("this is a test")
}
}
\keyword{ dplot }

