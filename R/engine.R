
TeXengine <- function(command,
                      fontDef,
                      getGlyph,
                      preamble="",
                      dviSuffix=".dvi") {
    engine <- list(command=command,
                   preamble=preamble,
                   fontDef=fontDef,
                   getGlyph=getGlyph,
                   dviSuffix=dviSuffix)
    class(engine) <- "TeXengine"
    engine
}

