\name{TeXengine}
\alias{TeXengine}
\alias{registerEngine}
\title{
  Define a TeX Engine.
}
\description{
  Define and register a TeX engine for authoring, typesetting,
  and rendering LaTeX documents.
}
\usage{
TeXengine(name,
          version,
          command,
          isEngine,
          fontFile,
          glyphIndex,
          options=NULL,
          preamble="",
          dviSuffix=".dvi")
registerEngine(engine)
}
\arguments{
  \item{name}{Character name for the engine.}
  \item{version}{A function with no arguments that returns the engine
    version as a character value.}
  \item{command}{The command used to typeset a latex document with this
    engine.}
  \item{isEngine}{A function with one argument, a \code{"DVI"} object,
    that returns a logical indicating whether this engine was used to
    generate that DVI output.}
  \item{fontFile}{A function with one argument, a font description from
    a font definition opertaion in DVI output,
    that returns a path to the appropriate font file.}
  \item{glyphIndex}{A function with one argument, a raw vector of bytes
    from a set char operation in DVI output,
    that returns an integer index of the appropriate glyph.}
  \item{options}{Any required options to \code{command} to ensure that
    the engine generates DVI output.}
  \item{preamble}{A preamble that is added during authoring of a complete
    LaTeX document from a LaTeX snippet.  See \code{\link{author}} and
    \code{\link{grid.latex}}.}
  \item{dviSuffix}{The file suffix used for DVI files that are generated
    by this engine.}
  \item{engine}{A \code{"TeXengine"} object, as generated by
    \code{TeXengine()}.}
}
\details{
  \code{TeXengine()} can be used to create a typesetting engine for use
  with, e.g., \code{\link{grid.latex}}.  Registering the engine via
  \code{registerEngine()} means that the engine can be specified by
  \code{name}.
}
\value{
  \code{TeXengine()} returns a \code{"TeXengine"} object.
}
\author{
  Paul Murrell
}
\keyword{ dplot }

