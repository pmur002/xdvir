
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

## Helper to extract glyphInfo (e.g., for use with embedGlyphs())
dviGlyphInfo <- function(grob) {
    if (!inherits(grob, "XDVIRgrob"))
        stop("Can only extract glyph info from an XDVIRgrob")
    if (length(grob$children)) {
        info <- lapply(grob$children,
                       function(x) {
                           if (inherits(x, "glyphgrob")) {
                               x$glyphInfo
                           } else {
                               NULL
                           }
                       })
        info <- info[!sapply(info, is.null)]
        if (length(info)) {
            info
        } else {
            NULL
        }
    } else {
        NULL
    }
}

dviGrob <- function(dvi, ...) {
    UseMethod("dviGrob")
}

dviGrob.DVI <- function(dvi,
                        x=0.5, y=0.5,
                        default.units="npc",
                        hjust="centre", vjust="centre",
                        device=names(dev.cur()),
                        name="XDVIRgrob",
                        gp=gpar(),
                        engine=lualatexEngine,
                        packages=NULL,
                        fontLib=getOption("xdvir.fontLibrary"),
                        ...) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    set("engine", engine)
    pkgs <- resolvePackages(packages)
    set("pkgs", pkgs)
    set("fontLib", fontLib)
    ## Generate objects from DVI
    invisible(lapply(dvi, grobDVI))
    objList <- get("DVIobjList")
    if (length(objList)) {
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
        gTree(children=do.call(gList, grobs), name=name,
              cl="XDVIRgrob")
    } else {
        gTree(name=name, cl="XDVIRgrob")
    }
}

grid.dvi <- function(...) {
    grid.draw(dviGrob(...))
}
