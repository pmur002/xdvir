\name{geom_latex}
\alias{geom_latex}
\alias{GeomLatex}
\title{Latex Data Symbols in ggplot2 Plots}
\usage{
geom_latex(mapping=NULL, data=NULL, stat="identity",
           position="identity", ...,
           nudge_x=0, nudge_y=0,
           width=NA,
           dpi=NA, packages=NULL,
           engine=getOption("xdvir.engine"),
           na.rm=FALSE,
           show.legend=NA, inherit.aes=TRUE)
}
\arguments{
  \item{mapping}{Set of aesthetic mappings created by
    \code{\link[ggplot2:aes]{aes()}} or
    \code{\link[ggplot2:aes_]{aes_()}}.
    If specified and \code{inherit.aes = TRUE} (the
    default), it is combined with the default mapping at the top level of the
    plot. You must supply \code{mapping} if there is no plot mapping.}

  \item{data}{The data to be displayed in this layer. There are three
    options:

    If \code{NULL}, the default, the data is inherited from the plot
    data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

    A \code{data.frame}, or other object, will override the plot
    data. All objects will be fortified to produce a data frame. See
    \code{\link[ggplot2:fortify]{fortify()}} for which variables will be
    created. 

    A \code{function} will be called with a single argument,
    the plot data. The return value must be a \code{data.frame}, and
    will be used as the layer data. A \code{function} can be created
    from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

  \item{stat}{The statistical transformation to use on the data for this
    layer, as a string.}

  \item{position}{Position adjustment, either as a string, or the result of
    a call to a position adjustment function. Cannot be jointy specified with
    \code{nudge_x} or \code{nudge_y}.}

  \item{...}{Other arguments passed on to
    \code{\link[ggplot2:layer]{layer()}}.  These are
    often aesthetics, used to set an aesthetic to a fixed value, like
    \code{colour = "red"} or \code{size = 3}. They may also be parameters
    to the paired geom/stat.}

  \item{nudge_x}{Horizontal and vertical adjustment to nudge labels by.
    Useful for offsetting text from points, particularly on discrete scales.
    Cannot be jointly specified with \code{position}.}

  \item{nudge_y}{Horizontal and vertical adjustment to nudge labels by.
    Useful for offsetting text from points, particularly on discrete scales.
    Cannot be jointly specified with \code{position}.}

  \item{width}{Either \code{NA} or a numeric value or a unit
    specifying the width for typesetting.  \code{NA} means the natural width of
    the label.}
  \item{dpi}{Resolution (dots per inch).}
  \item{packages}{The LaTeX packages to be used.
    May be the name of a LaTeX package (character) or a
    \code{"LaTeXpackage"} object.}
  \item{engine}{The TeX engine that should be used to render the LaTeX.
    May be the name of a TeX engine (character)
    or a \code{"TeXengine"} object.}
  \item{na.rm}{If \code{FALSE}, the default, missing values are removed with
    a warning. If \code{TRUE}, missing values are silently removed.}

  \item{show.legend}{logical. Should this layer be included in the legends?
    \code{NA}, the default, includes if any aesthetics are mapped.
    \code{FALSE} never includes, and \code{TRUE} always includes.
    It can also be a named logical vector to finely select the aesthetics to
    display.}

  \item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
    rather than combining with them. This is most useful for helper functions
    that define both data and aesthetics and shouldn't inherit behaviour from
    the default plot specification, e.g.
    \code{\link[ggplot2:borders]{borders()}}.}
}
\value{
  A ggplot2 layer that can be added to a plot created with
  \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}}.
}
\description{
  This geom draws text labels similar to
  \code{\link[ggplot2:geom_text]{ggplot2::geom_text()}}, but with the
  text interpreted as a LaTeX fragment.
  Most styling parameters can be used as aesthetics and can be applied
  separately to each text label drawn.
}
\section{Aesthetics}{


  \code{geom_latex()} understands the following aesthetics (required
  aesthetics are in bold; select aesthetics are annotated):
  \itemize{
    \item \strong{\code{x}}
    \item \strong{\code{y}}
    \item \strong{\code{label}}
    \item \code{alpha}
    \item \code{angle}
    \item \code{colour} Default color of label text and label outline.
    \item \code{family}
    \item \code{group}
    \item \code{hjust}
    \item \code{lineheight}
    \item \code{size} Default font size of label text.
    \item \code{vjust}
  }
}
