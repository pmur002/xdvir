
initTeXstate <- function() {
    state <- TeXstate()
    ## This **could** be made user-settable at some point?
    TeXset("scale", 1, state)
    ## Initialise locations
    TeXset("h", 0, state)
    TeXset("v", 0, state)
    TeXset("w", 0, state)
    TeXset("x", 0, state)
    TeXset("y", 0, state)
    TeXset("z", 0, state)
    ## Pixel locations
    TeXset("hh", 0, state)
    TeXset("vv", 0, state)
    ## Init text left/right
    TeXset("textleft", Inf, state)
    TeXset("textright", -Inf, state)
    ## Init bbox
    TeXset("top", Inf, state)
    TeXset("bottom", -Inf, state)
    TeXset("left", Inf, state)
    TeXset("right", -Inf, state)
    ## Init baseline
    TeXset("baseline", NA, state)
    ## Init anchors
    TeXset("hAnchors", NULL, state)
    TeXset("vAnchors", NULL, state)
    ## Init cumulative structures
    ## Extra slot for dummy font
    TeXset("fonts", vector("list", 256), state)
    TeXset("objList", list(), state)
    TeXset("glyphs", list(), state)
    ## Stack for push/pop
    TeXset("stack", list(), state)
    TeXset("i", 0, state)
    ## Font number
    TeXset("f", NA, state)
    ## Default colour
    TeXset("colour", NA, state)
    ## Default text direction
    TeXset("dir", 0, state)
    state
}

calculateOffset <- function(x, y, hjust, vjust, state) {
    ## Create dummy font
    fonts <- TeXget("fonts", state)
    fonts[[256]] <- list(file="", index=0, size=0, op=NULL)
    TeXset("fonts", fonts, state)
    ## Create dummy glyph grob
    dummyGlyph <- glyph(x=0, y=0, xx=0, yy=0, index=0, fontindex=256, size=0)
    class(dummyGlyph) <- "XDVIRglyphObj"
    dummyGrob <- objToGrob(dummyGlyph, x=x, y=y, hjust=hjust, vjust=vjust,
                           dpi=NA, state=state)
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
    
    ## Generate objs from DVI,
    ## which also establishes metrics of text and any other drawing
    invisible(lapply(x$dvi, DVItoObj, state))
    objList <- TeXget("objList", state)
    ## Store copy of most recent object list to make debugging easier
    set(".Last.objList", objList)
    if (length(objList)) {
        ## Calculate offset for non-text drawing
        offset <- calculateOffset(x$x, x$y, x$hjust, x$vjust, state)
        ## Generate grobs from objs
        grobs <- lapply(objList, objToGrob,
                        x=x$x, y=x$y, hjust=x$hjust, vjust=x$vjust,
                        xoffset=offset$x, yoffset=offset$y,
                        dpi=x$dpi,
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
                        dpi=NA, 
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
          dpi=dpi, 
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

