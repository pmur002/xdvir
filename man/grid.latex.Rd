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
          x=0.5, y=0.5, default.units="npc",
          hjust="centre", vjust="centre",
          dpi=NA,
          packages=NULL,
          engine=getOption("xdvir.engine"),
          fontLib=getOption("xdvir.fontLib"),
          texFile=NULL,
          name=NULL,
          gp=gpar(),
          vp=NULL) 
grid.latex(\dots)

xelatexGrob(tex,
            x=0.5, y=0.5, default.units="npc",
            hjust="centre", vjust="centre",
            dpi=NA,
            packages=NULL,
            fontLib=getOption("xdvir.fontLib"),
            texFile=NULL,
            name=NULL,
            gp=gpar(),
            vp=NULL) 
grid.xelatex(\dots)

lualatexGrob(tex,
             x=0.5, y=0.5, default.units="npc",
             hjust="centre", vjust="centre",
             dpi=NA,
             packages=NULL,
             fontLib=getOption("xdvir.fontLib"),
             texFile=NULL,
             name=NULL,
             gp=gpar(),
             vp=NULL) 
grid.lualatex(\dots)

}
\arguments{
  \item{tex}{LaTeX code as a character vector.}
  \item{x, y}{Numeric values or units specifying where to draw the
    output.}
  \item{default.units}{Units to use if \code{x} or \code{y} are
    numeric.}
  \item{hjust, vjust}{Justification of the output relative to the
    \code{x}/\code{y} location.}
  \item{dpi}{Resolution (dots per inch) for rendering.}
  \item{engine}{The TeX engine that should be used to render the LaTeX.
    May be the name of a TeX engine (character)
    or a \code{"TeXengine"} object.}
  \item{fontLib}{The font libraary the should be used to query fonts
    and glyphs.}
  \item{packages}{The LaTeX packages to be used.}
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
% Requires TeX installation
grid.latex("this is a test")
}
}
\keyword{ dplot }

