
TeXengine <- function(command,
                      options=NULL,
                      fontDef,
                      getGlyph,
                      preamble="",
                      dviSuffix=".dvi") {
    engine <- list(command=command,
                   options=options,
                   preamble=preamble,
                   fontDef=fontDef,
                   getGlyph=getGlyph,
                   dviSuffix=dviSuffix)
    class(engine) <- "TeXengine"
    engine
}

