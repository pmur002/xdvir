
## Define font library from 'fonttools' (if available)

ftFontFamily <- function(file) {
    font <- fonttools::loadFont(file)
    fonttools::fontFamily(font)
}

ftFontWeight <- function(file) {
    font <- fonttools::loadFont(file)
    weight <- fonttools::fontWeight(font)
    if (is.null(weight))
        400
    else
        as.numeric(weight)
}

ftFontStyle <- function(file) {
    font <- fonttools::loadFont(file)
    fonttools::fontStyle(font)
}

ftGlyphNames <- function(file) {
    font <- fonttools::loadFont(file)
    fonttools::fontGlyphNames(font)
}

ftGlyphIndex <- function(name, file) {
    font <- fonttools::loadFont(file)
    ## Try more than one name (if there are multiple options)
    index <- NA
    while (is.na(index) && length(name)) {
        ## Allow for Python Key Error (Dictionary index not found)
        index <- tryCatch(fonttools::fontGlyphIndex(font, name[1]),
                          python.builtin.KeyError = function(e) {
                              ## Silently consume
                              NA
                          })
        name <- name[-1]
    }
    index
}

ftGlyphName <- function(code, file, dir) {
    font <- fonttools::loadFont(file)
    fonttools::fontGlyphName(font, code)
}

ftGlyphWidth <- function(index, file, size, transform=TRUE) {
    font <- fonttools::loadFont(file)
    width <- fonttools::fontGlyphWidth(font, index)
    unitsPerEm <- fonttools::fontUnits(font)
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
    height <- fonttools:fontGlyphHeight(font, index)
    unitsPerEm <- fonttools::fontUnits(font)
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
    ## NOTE the different order 
    bbox <- fonttools::fontGlyphBounds(font, index)[c(1, 3, 2, 4)]
    unitsPerEm <- fonttools::fontUnits(font)
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
                             ftGlyphNames,
                             ftGlyphIndex,
                             ftGlyphName,
                             ftGlyphWidth,
                             ftGlyphHeight,
                             ftGlyphBounds,
                             init=function() {})
