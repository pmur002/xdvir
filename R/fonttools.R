
## Define font library from 'fonttools' (if available)

ftFontFamily <- function(file) {
    font <- fonttools::loadFont(file)
    fonttools::fontFamily(font)
}

ftFontWeight <- function(file) {
    font <- fonttools::loadFont(file)
    fonttools::fontWeight(font)
    if (is.null(weight))
        400
    else
        as.numeric(weight)
}

ftFontStyle <- function(file) {
    font <- fonttools::loadFont(file)
    fonttools::fontStyle(font)
}

ftGlyphIndex <- function(name, file) {
    font <- fonttools::loadFont(file)
    fonttools::glyphIndex(font, name)
}

ftGlyphName <- function(code, file, dir) {
    font <- fonttools::loadFont(file)
    fonttools::glyphName(font, code)
}

ftGlyphWidth <- function(index, file, size, transform=TRUE) {
    font <- fonttools::loadFont(file)
    width <- fonttools::glyphWidth(font, index)
    unitsPerEm <- fonttools::fontUnit(font)
    if (transform) {
        ## round() to get whole number metrix (at 1000 scale)
        ## floor() to match what PDF_StrWidthUTF8() does
        fontsize <- size
        cex <- 1
        widthPts <- floor(fontsize + .5)*cex*
            (round(width/(unitsPerEm/1000)))/1000
        xtoTeX(unit(widthPts, "bigpts"))
    } else {
        width
    }
}

ftGlyphHeight <- function(index, file, size, transform=TRUE) {
    font <- fonttools::loadFont(file)
    height <- fonttools::glyphHeight(font, index)
    unitsPerEm <- fonttools::fontUnit(font)
    if (transform) {
        ## round() to get whole number metrix (at 1000 scale)
        ## floor() to match what PDF_StrWidthUTF8() does
        fontsize <- size
        cex <- 1
        heightPts <- floor(fontsize + .5)*cex*
            (round(height/(unitsPerEm/1000)))/1000
        xtoTeX(unit(heightPts, "bigpts"))
    } else {
        height
    }
}

ftGlyphBounds <- function(index, file, size, dir) {
    font <- fonttools::loadFont(file)
    bbox <- fonttools::glyphBounds(font, index)
    unitsPerEm <- fonttools::fontUnit(font)
    ## round() to get whole number metrix (at 1000 scale)
    ## floor() to match what PDF_StrWidthUTF8() does
    fontsize <- size
    cex <- 1
    bboxPts <- floor(fontsize + .5)*cex*(round(bbox/(unitsPerEm/1000)))/1000
    xtoTeX(unit(bboxPts, "bigpts"))    
}
                          
ftFontLibrary <- fontLibrary(ftFontFamily,
                             ftFontWeight,
                             ftFontStyle,
                             ftGlyphIndex,
                             ftGlyphName,
                             ftGlyphWidth,
                             ftGlyphHeight,
                             ftGlyphBounds,
                             init=function() {})
