
## This file provides the set of INTERNAL function calls that are used
## to get information out of fonts.
## It is an INTERNAL API.

## The (current) implementation of the API is separated out into
## ./ttx.R (which makes use of the fonttools 'ttx' program)

## This design allows for a different implementation to be swapped in
## sometime in the future, e.g., one based on the 'systemfonts' package,
## WITHOUT having to change code all over this package.

getFontFamily <- function(file) {
    ttxFontFamily(file)
}

getFontWeight <- function(file) {
    ttxFontWeight(file)
}

getFontStyle <- function(file) {
    ttxFontStyle(file)
}

getGlyphIndex <- function(name, file) {
    ttxGlyphIndex(name, file)
}

getGlyphNameFromUNICODE <- function(code, file, dir) {
    ttxGlyphNameFromUNICODE(code, file, dir)
}

getGlyphWidth <- function(index, file, size) {
    ttxGlyphWidth(index, file, size)
}

getGlyphHeight <- function(index, file, size) {
    ttxGlyphHeight(index, file, size)
}

getGlyphMetrics <- function(index, file, size, dir) {
    ttxGlyphMetrics(index, file, size, dir)
}
