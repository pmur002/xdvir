
latexGrob <- function(tex,
                      x=0.5, y=0.5, default.units="npc",
                      hjust="centre", vjust="centre",
                      engine=lualatexEngine, packages=NULL,
                      tinytex=getOption("xdvir.tinytex")) {
    texDoc <- author(tex, engine=engine, packages=packages)
    dviFile <- typeset(texDoc, engine=engine, tinytex=tinytex)
    dvi <- readDVI(dviFile)
    dviGrob(dvi,
            x=x, y=y, default.units=default.units,
            hjust=hjust, vjust=vjust,
            engine=engine, package=packages)
}

grid.latex <- function(...) {
    grid.draw(latexGrob(...))
}
