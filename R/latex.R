
latexGrob <- function(tex,
                      x=0.5, y=0.5, default.units="npc",
                      hjust="centre", vjust="centre",
                      packages=NULL,
                      engine=getOption("xdvir.engine"),
                      fontLib=getOption("xdvir.fontLib"),
                      texFile=NULL,
                      ...,
                      name="LaTeXgrob",
                      gp=gpar(),
                      vp=NULL) {
    engine <- getEngine(engine)
    lib <- resolveFontLib(fontLib)
    pkgs <- resolvePackages(packages)
    texDoc <- author(tex, engine=engine, packages=pkgs)
    dviFile <- typeset(texDoc, engine=engine, texFile=texFile)
    dvi <- readDVI(dviFile)
    dviGrob(dvi,
            x=x, y=y, default.units=default.units,
            hjust=hjust, vjust=vjust,
            engine=engine, package=pkgs, fontLib=lib,
            ...,
            name=name, gp=gp, vp=vp)
}

grid.latex <- function(...) {
    grid.draw(latexGrob(...))
}
