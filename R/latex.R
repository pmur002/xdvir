
latexGrob <- function(tex,
                      x=0.5, y=0.5, default.units="npc",
                      hjust="centre", vjust="centre",
                      dpi=NA,
                      packages=NULL,
                      engine=getOption("xdvir.engine"),
                      fontLib=getOption("xdvir.fontLib"),
                      texFile=NULL,
                      name=NULL,
                      gp=gpar(),
                      vp=NULL) {
    if (length(tex) < 1)
        stop("No LaTeX fragment to render")
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    engine <- getEngine(engine)
    lib <- resolveFontLib(fontLib)
    pkgs <- resolvePackages(packages)
    texDoc <- author(tex, engine=engine, packages=pkgs)
    dviFile <- typeset(texDoc, engine=engine, texFile=texFile)
    dvi <- readDVI(dviFile)
    dviGrob(dvi,
            x=x, y=y, default.units=default.units,
            hjust=hjust, vjust=vjust,
            dpi=dpi,
            engine=engine, package=pkgs, fontLib=lib,
            name=name, gp=gp, vp=vp)
}

grid.latex <- function(...) {
    grid.draw(latexGrob(...))
}
