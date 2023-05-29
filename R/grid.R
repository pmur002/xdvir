
dummyGlyph <- function(x, y, hjust, vjust) {
    dummyGlyph <- glyph(0, 0, 0, 0, 256, 0)
    class(dummyGlyph) <- "XDVIRglyphObj"
    buildGrob(dummyGlyph, x=x, y=y, hjust=hjust, vjust=vjust)
}

setDummyFont <- function() {
    fonts <- get("fonts")
    fonts[[256]] <- list(fontdef=fontDef("", 256, "", 400, "normal"),
                         size=0,
                         op=NULL)
    set("fonts", fonts)
}

################################################################################
## User API

dviGrob <- function(dvi, ...) {
    UseMethod("dviGrob")
}

dviGrob.DVI <- function(dvi,
                        x=0.5, y=0.5,
                        default.units="npc",
                        hjust="centre", vjust="centre",
                        device=names(dev.cur()),
                        name=NULL,
                        gp=gpar(),
                        engine=lualatexEngine,
                        packages=NULL,
                        ...) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    set("engine", engine)
    pkgs <- resolvePackages(packages)
    set("pkgs", pkgs)
    ## Generate objects from DVI
    invisible(lapply(dvi, grobDVI))
    objList <- get("DVIobjList")
    ## Use dummy glyph to establish x/y offset for non-glyph grobs
    setDummyFont()
    dummy <- dummyGlyph(x, y, hjust, vjust)
    coords <- grobCoords(dummy)
    xoffset <- coords[[1]]$x[1]
    yoffset <- coords[[1]]$y[1]
    ## Generate grobs from object list
    grobs <- lapply(objList, buildGrob,
                    x=x, y=y, hjust=hjust, vjust=vjust,
                    xoffset=xoffset, yoffset=yoffset)
    gTree(children=do.call(gList, grobs))
}

grid.dvi <- function(...) {
    grid.draw(dviGrob(...))
}
