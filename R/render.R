
initTeXstate <- function() {
    state <- TeXstate()
    ## Extra slot for dummy font
    TeXset("fonts", vector("list", 256), state)
    TeXset("glyphs", list(), state)
    TeXset("dir", 0, state)
    state
}

calculateOffset <- function(x, y, hjust, vjust, state) {
    ## Create dummy font
    fonts <- TeXget("fonts", state)
    fonts[[256]] <- list(file="", index=0, size=0, op=NULL)
    TeXset("fonts", fonts, state)
    ## Create dummy glyph grob
    dummyGlyph <- glyph(x=0, y=0, index=0, fontindex=256, size=0)
    class(dummyGlyph) <- "XDVIRglyphObj"
    dummyGrob <- objToGrob(dummyGlyph, x=x, y=y, hjust=hjust, vjust=vjust,
                           state=state)
    coords <- grobCoords(dummyGrob)
    ## Offset is location of dummy glyph grob
    list(x=convertX(unit(coords[[1]]$x[1], "in"), "bigpts", valueOnly=TRUE),
         y=convertY(unit(coords[[1]]$y[1], "in"), "bigpts", valueOnly=TRUE))
}

makeContent.DVIgrob <- function(x, ...) {
    state <- initTeXstate()
    TeXset("packages", x$packages, state)
    TeXset("fontLib", x$fontLib, state)
    TeXset("engine", x$engine, state)
    TeXset("dpi", x$dpi, state)
    TeXset("scale", x$scale, state)
    
    ## Generate objs from DVI,
    ## which also establishes metrics of text and any other drawing
    invisible(lapply(x$dvi, DVItoObj, state))
    objList <- TeXget("objList", state)
    if (length(objList)) {
        ## Calculate offset for non-text drawing
        offset <- calculateOffset(x$x, x$y, x$hjust, x$vjust, state)
        ## Generate grobs from objs
        grobs <- lapply(objList, objToGrob,
                        x=x$x, y=x$y, hjust=x$hjust, vjust=x$vjust,
                        xoffset=offset$x, yoffset=offset$y,
                        state=state)
        x <- setChildren(x, do.call(gList, grobs))
    } 
    x
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
                        dpi=NA, scale=1,
                        packages=NULL,
                        engine=getOption("xdvir.engine"),
                        fontLib=getOption("xdvir.fontLib"),
                        ...,
                        name=NULL,
                        gp=gpar(),
                        vp=NULL) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    eng <- resolveEngine(dvi, engine)
    lib <- resolveFontLib(fontLib)
    pkgs <- resolvePackages(packages)
    pkgs <- checkPackages(pkgs, typesetPackages(dvi))
    gTree(dvi=dvi, x=x, y=y, hjust=hjust, vjust=vjust,
          dpi=dpi, scale=scale,
          engine=eng, fontLib=lib, packages=pkgs,
          gp=gp, name=name, vp=vp,
          cl="DVIgrob")
}

## Resolve engine from typeset() engine
dviGrob.DVIfile <- function(dvi, ..., engine=NULL, packages=NULL) {
    eng <- resolveEngine(dvi, engine)
    pkgs <- checkPackages(packages, typesetPackages(dvi))
    dviGrob(readDVI(dvi), ..., engine=eng, packages=pkgs)
}

dviGrob.character <- function(dvi, ...) {
    dviGrob(readDVI(dvi), ...)
}

grid.dvi <- function(...) {
    grid.draw(dviGrob(...))
}

