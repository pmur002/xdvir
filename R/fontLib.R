
## Create a "font library", which is a list of functions for
## querying fonts and glyphs

## See ./ttx.R for example font library 

initFontLib <- function() {
    fontLib <- getOption("xdvir.fontLibrary")
    haveFontTools <- nchar(system.file(package="fonttools"))
    if (haveFontTools) {
        message(paste0("fonttools:  ", packageVersion("fonttools")))
    } else {
        message("fonttools:  not installed")
    }
    if (is.null(fontLib)) {
        if (haveFontTools) {
            options("xdvir.fontLibrary"=ftFontLibrary)
        } else {
            options("xdvir.fontLibrary"=ttxFontLibrary)
        }
        fontLib <- getOption("xdvir.fontLibrary")
    }
    fontLib$init()
}

fontLibrary <- function(fontFamily,
                        fontWeight,
                        fontStyle,
                        glyphIndex,
                        glyphName,
                        glyphWidth,
                        glyphHeight,
                        glyphBounds,
                        init) {
    fontLib <- list(fontFamily=fontFamily,
                    fontWeight=fontWeight,
                    fontStylef=fontStyle,
                    glyphIndex=glyphIndex,
                    glyphName=glyphName,
                    glyphWidth=glyphWidth,
                    glyphHeight=glyphHeight,
                    glyphBounds=glyphBounds,
                    init=init)
    class(fontLib) <- "XDVIRfontLibrary"
    fontLib
}
                      
