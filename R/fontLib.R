
## Create a "font library", which is a list of functions for
## querying fonts and glyphs

FontLibrary <- function(glyphWidth,
                        glyphHeight,
                        glyphBounds) {
    fontLib <- list(glyphWidth=glyphWidth,
                    glyphHeight=glyphHeight,
                    glyphBounds=glyphBounds)
    class(fontLib) <- "FontLibrary"
    fontLib
}

resolveFontLib <- function(lib) {
    UseMethod("resolveFontLib")
}

resolveFontLib.default <- function(lib) {
    stop("Invalid Font Library")
}

resolveFontLib.NULL <- function(lib) {
    if (is.null(lib) || !inherits(lib, "FontLibrary")) {
        warning(paste("No (valid) Font Library specified;",
                      'falling back to getOption("xdvir.fontLib")'))
        defaultLib <- getOption("xdvir.fontLib")
        if (!inherits(defaultLib, "FontLibrary")) {
            warning(paste("Invalid default Font Library;",
                          "falling back to null Font Library"))
            nullFontLib
        } else {
            defaultLib
        }
    } else {
        lib
    }
}

resolveFontLib.FontLibrary <- function(lib) {
    lib
}

metricUnits <- function(x) {
    attr(x, "unitsPerEm")
}

## The conversions from font file metrics to TeX units (scaled points)
## will in general fail to satisfy the requirement described in dvitype ...
## "This fixed-point multiplication must be done with precisely the same
##  accuracy by all DVI-reading programs, in order to validate the
##  assumptions made by DVI-writing programs like TEX82."
## https://mirror.math.princeton.edu/pub/CTAN/systems/knuth/dist/texware/dvitype.web
## https://texdoc.org/serve/dvitype.pdf/0
## ... not least because we are working with OpenType/TrueType fonts
## rather than TFM files.
## NOTE that dvi-decode goes via pixels (using dpi), but I do not see how
## that is any closer to replicating the dvitype algorithm.
## OTOH, these calculations, when fed the exact values from TFM files
## do produce identical results to dvitype.
TeXglyphWidth <- function(index, file, size, fontLib, state) {
    width <- fontLib$glyphWidth(index, file)
    unitsPerEm <- metricUnits(width)
    ## floor() to get whole number of TeX units (scaled points)
    floor(size * width/unitsPerEm)
}

TeXglyphHeight <- function(index, file, size, fontLib, state) {
    height <- fontLib$glyphHeight(index, file)
    unitsPerEm <- metricUnits(height)
    ## floor() to get whole number of TeX units (scaled points)
    floor(size * height/unitsPerEm)
}

TeXglyphBounds <- function(index, file, size, fontLib, state) {
    bounds <- fontLib$glyphBounds(index, file)
    unitsPerEm <- metricUnits(bounds)
    ## floor() to get whole number of TeX units (scaled points)
    floor(size * bounds/unitsPerEm)
}

################################################################################
## Null Font Library

nullGlyphWidth <- function(index, file) {
    ## Fixed advance width
    w <- 500
    attr(w, "unitsPerEm") <- 1000
    w
}

nullGlyphBounds <- function(index, file) {
    ## Fixed width and fixed height
    bbox <- c(0, 0, 400, 700)
    attr(bbox, "unitsPerEm") <- 1000
    bbox
}

nullFontLib <- FontLibrary(glyphWidth=nullGlyphWidth,
                           glyphHeight=NULL,
                           glyphBounds=nullGlyphBounds)
