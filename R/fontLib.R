
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

TeXglyphWidth <- function(index, file, size, fontLib, pre) {
    width <- fontLib$glyphWidth(index, file)
    unitsPerEm <- metricUnits(width)
    ## round() to get whole number metrix (at 1000 scale)
    ## floor() to match what PDF_StrWidthUTF8() does
    cex <- 1
    widthPts <- floor(size + .5)*cex*
        (round(width/(unitsPerEm/1000)))/1000
    toTeX(unit(widthPts, "bigpts"), pre)
}

TeXglyphHeight <- function(index, file, size, fontLib, pre) {
    height <- fontLib$glyphHeight(index, file)
    unitsPerEm <- metricUnits(height)
    ## round() to get whole number metrix (at 1000 scale)
    ## floor() to match what PDF_StrWidthUTF8() does
    cex <- 1
    heightPts <- floor(size + .5)*cex*
        (round(height/(unitsPerEm/1000)))/1000
    toTeX(unit(heightPts, "bigpts"), pre)
}

TeXglyphBounds <- function(index, file, size, fontLib, pre) {
    bounds <- fontLib$glyphBounds(index, file)
    unitsPerEm <- metricUnits(bounds)
    ## round() to get whole number metrix (at 1000 scale)
    ## floor() to match what PDF_StrWidthUTF8() does
    cex <- 1
    boundsPts <- floor(size + .5)*cex*
        (round(bounds/(unitsPerEm/1000)))/1000
    toTeX(unit(boundsPts, "bigpts"), pre)
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
