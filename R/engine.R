
TeXengine <- function(engine, options="",
                      defineFont,
                      getGlyph,
                      dviSuffix=".dvi") {
    engine <- list(engine=engine, options=options,
                   defineFont=defineFont,
                   getGlyph=getGlyph,
                   dviSuffix=dviSuffix)
    class(engine) <- "TeXengine"
    engine
}



