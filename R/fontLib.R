
## Create a "font library", which is a list of functions for
## querying fonts and glyphs

## See ./ttx.R for example font library 

initFontLib <- function() {
    if (is.null(getOption("xdvir.fontLibrary"))) {
        if (nchar(system.file(package="fonttools"))) {
            options("xdvir.fontLibrary"=ftFontLibrary)
        } else {
            options("xdvir.fontLibrary"=ttxFontLibrary)
        }
    }
    fontLib <- getOption("xdvir.fontLibrary")
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
                      
