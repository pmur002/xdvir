\name{element_latex}
\alias{element_latex}
\title{ggplot2 Theme Element Supporting Latex Syntax}
\usage{
element_latex(family = NULL,
              fontface = NULL,
              colour = NULL,
              size = NULL,
              hjust = NULL, vjust = NULL,
              angle = NULL,
              lineheight = NULL,
              color = NULL,
              margin = NULL,
              rotate_margins = FALSE,
              inherit.blank = FALSE)
}
\arguments{
\item{family}{The default font family.}
\item{fontface}{The default font face.}
\item{colour, color}{The default text colour.}
\item{size}{The deafult font size.}
\item{hjust}{Horizontal justification.  Typically in \eqn{[0, 1]}, but
  see Details.)}
\item{vjust}{Vertical justification.  Typically in \eqn{[0, 1]}, but
  see Details.)}
\item{angle}{Angle (in \eqn{[0, 360]})}
\item{lineheight}{The deafult lineheight.}
\item{margin}{The margin around the text.}
\item{rotate_margins}{Whether margins should follow the orientation of
  the text.}
\item{inherit.blank}{Should this element inherit the existence of an
\code{element_blank} among its parents? If \code{TRUE} the existence of
a blank element among its parents will cause this element to be blank as
well. If \code{FALSE} any blank parent element will be ignored when
calculating final element state.}
}
\value{
An \code{element_latex} object that can be used in place of
\code{element_text} in ggplot2 theme specifications
}
\description{
  This theme element is an alternative to \code{ggplot2::element_text()}
  for producing labels from LaTeX fragments.

  Both \code{hjust} and \code{vjust} can be character values:
  one of \code{"left"}, \code{"bbleft"}, \code{"centre"},
  \code{"center"}, \code{"right"}, \code{"bbright"} for horizontal
  justification;  and one of \code{"bottom"}, \code{"baseline"},
  \code{"centre"}, \code{"center"}, or \code{"top"} for
  vertical justification.
}
