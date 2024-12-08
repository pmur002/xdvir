
latexGrob <- function(tex,
                      x=0.5, y=0.5,
                      margin=0,
                      rot=0,
                      default.units="npc",
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
    if (!is.unit(margin))
        margin <- unit(margin, default.units)
    margin <- rep(margin, length.out=4)
    ## Resolve args
    engine <- getEngine(engine)
    lib <- resolveFontLib(fontLib)
    pkgs <- resolvePackages(packages)
    ## Only author/typeset unique values
    uniq <- unique(tex)
    index <- match(tex, uniq)
    texDocs <- lapply(uniq, author, engine=engine, packages=pkgs)
    dviFiles <- lapply(texDocs, typeset, engine=engine, texFile=texFile)
    dvi <- lapply(dviFiles, readDVI)
    ## Re-expand dvis
    dvi <- dvi[index]
    dviGrob(dvi,
            x=x, y=y, margin=margin, rot=rot,
            default.units=default.units,
            hjust=hjust, vjust=vjust,
            dpi=dpi,
            engine=engine, package=pkgs, fontLib=lib,
            name=name, gp=gp, vp=vp)
}

grid.latex <- function(...) {
    grid.draw(latexGrob(...))
}
