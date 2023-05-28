
TeXengine <- function(command,
                      fontDef,
                      getGlyph,
                      preamble="",
                      prefix="",
                      suffix="",
                      dviSuffix=".dvi") {
    engine <- list(command=command,
                   preamble=preamble,
                   prefix=prefix,
                   suffix=suffix,
                   fontDef=fontDef,
                   getGlyph=getGlyph,
                   dviSuffix=dviSuffix)
    class(engine) <- "TeXengine"
    engine
}

