
latexGrob <- function(tex, engine=lualatexEngine, packages=NULL) {
    texDoc <- author(tex, engine=engine, packages=packages)
    dviFile <- typeset(texDoc, engine=engine)
    dvi <- readDVI(dviFile)
    dviGrob(dvi, engine=engine, package=packages)
}

grid.latex <- function(...) {
    grid.draw(lualatexGrob(...))
}
