
TeXengine <- function(engine, options="",
                      defineFont,
                      getGlyph,
                      glyphWidth,
                      glyphHeight,
                      glyphMetrics,
                      dviSuffix=".dvi") {
    engine <- list(engine=engine, options=options,
                   defineFont=defineFont,
                   getGlyph=getGlyph,
                   glyphWidth=glyphWidth,
                   glyphHeight=glyphHeight,
                   glyphMetrics=glyphMetrics,
                   dviSuffix=dviSuffix)
    class(engine) <- "TeXengine"
    engine
}



