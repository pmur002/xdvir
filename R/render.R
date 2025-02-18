
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
    textleft <- TeX2pt(TeXget("textleft", state), state)
    textright <- TeX2pt(TeXget("textright", state), state)
    left <- TeX2pt(TeXget("left", state), state)
    right <- TeX2pt(TeXget("right", state), state)
    bottom <- -TeX2pt(TeXget("bottom", state), state)
    top <- -TeX2pt(TeXget("top", state), state)
    if (!is.finite(textleft))
        textleft <- left
    if (!is.finite(textright))
        textright <- right
    vAnchorValues <- c(bottom, top, (bottom + top)/2)
    vAnchorLabels <- c("bottom", "top", "centre")
    if (is.finite(TeXget("baseline", state))) {
        vAnchorValues <- c(vAnchorValues,
                           -TeX2pt(TeXget("baseline", state), state))
        vAnchorLabels <- c(vAnchorLabels, "baseline")
    }
    anchors <- TeXget("vAnchors", state)
    if (!is.null(anchors)) {
        vAnchorValues <- c(vAnchorValues, -TeX2pt(anchors$value, state))
        vAnchorLabels <- c(vAnchorLabels, anchors$label)
    }
    vAnchor <- glyphAnchor(vAnchorValues, vAnchorLabels)
    ## NOTE that 'left' and 'right' can exceed 'textleft' and 'textright'
    ## e.g., if there is non-character output beyond the character output
    minX <- min(textleft, left)
    maxX <- max(textright, right)
    hAnchorValues <- c(minX, maxX, (minX + maxX)/2,
                       left, right, (left + right)/2)
    hAnchorLabels <- c("left", "right", "centre",
                       "bbleft", "bbright", "bbcentre")
    anchors <- TeXget("hAnchors", state)
    if (!is.null(anchors)) {
        hAnchorValues <- c(hAnchorValues, TeX2pt(anchors$value, state))
        hAnchorLabels <- c(hAnchorLabels, anchors$label)
    }
    hAnchor <- glyphAnchor(hAnchorValues, hAnchorLabels)
    width <- glyphWidth(c(maxX - minX, right - left),
                        c("width", "bbwidth"),
                        c("left", "bbleft"))
    height <- glyphHeight(top - bottom)
    ## Create dummy font
    fonts <- TeXget("fonts", state)
    fonts[[256]] <- list(file="", index=0, size=0, op=NULL)
    TeXset("fonts", fonts, state)
    ## Create dummy glyph grob
    dummyGlyph <- glyph(x=0, y=0, xx=0, yy=0, index=0, fontindex=256, size=0)
    class(dummyGlyph) <- "XDVIRglyphObj"
    dummyGrob <- objToGrob(dummyGlyph, hjust, vjust,
                           width, height, hAnchor, vAnchor, dpi=NA,
                           state=state)
    coords <- grobCoords(dummyGrob)
    ## Offset is location of dummy glyph grob
    list(x=convertX(unit(coords[[1]]$x[1], "in"), "bigpts", valueOnly=TRUE),
         y=convertY(unit(coords[[1]]$y[1], "in"), "bigpts", valueOnly=TRUE),
         width=width,
         height=height,
         hAnchor=hAnchor, vAnchor=vAnchor)
}

buildState <- function(packages, fontLib, engine, dpi) {
    state <- initTeXstate()
    TeXset("packages", packages, state)
    TeXset("fontLib", fontLib, state)
    TeXset("engine", engine, state)
    TeXset("dpi", dpi, state)
    state
}

buildObjList <- function(dvi, page, state) {
    ## Generate objs from DVI,
    ## which also establishes metrics of text and any other drawing
    TeXset("currentPage", 0, state)
    TeXset("whichPage", page, state)
    lapply(dvi, DVItoObj, state)
    TeXget("objList", state)
}

addMargin <- function(margin, state) {
    ## Create new environment so that multiple calls to xDetails
    ## do not accumulate multiple copies of margins
    state <- as.environment(as.list(state))
    ## margin: bottom, left, top, right
    TeXset("bottom",
           TeXget("bottom", state) +
           pt2TeX(convertY(margin[1], "bigpts", valueOnly=TRUE), state),
           state)
    TeXset("left",
           TeXget("left", state) -
           pt2TeX(convertX(margin[2], "bigpts", valueOnly=TRUE), state),
           state)
    TeXset("top",
           TeXget("top", state) -
           pt2TeX(convertY(margin[3], "bigpts", valueOnly=TRUE), state),
           state)
    TeXset("right",
           TeXget("right", state) +
           pt2TeX(convertX(margin[4], "bigpts", valueOnly=TRUE), state),
           state)
    state
}

rotVP <- function(x, y, rot) {
    viewport(x, y, angle=rot)
}

buildDVIgrob <- function(objList, x, y, rot, hjust, vjust, name, gp, state,
                         margin, dpi) {
    gTree(objList=objList, x=x, y=y, margin=margin, rot=rot, margin=margin,
          hjust=hjust, vjust=vjust, dpi=dpi, state=state,
          name=name, gp=gp, 
          cl="DVIgrob")
}

makeContext.DVIgrob <- function(x, ...) {
    ## This will not override any existing 'vp' because a DVIgrob
    ## is only created by buildDVIgrob() which does not set 'vp'
    ## Viewport for rotation
    x$vp <- rotVP(x$x, x$y, x$rot)
    x
}

makeContent.DVIgrob <- function(x, ...) {
    if (length(x$objList)) {
        state <- addMargin(x$margin, x$state)
        ## Calculate offset for non-text drawing
        offset <- calculateOffset(x$x, x$y, x$hjust, x$vjust, state)
        ## Generate grobs from objs
        grobs <- lapply(x$objList, objToGrob,
                        x=x$x, y=x$y, hjust=x$hjust, vjust=x$vjust,
                        width=offset$width, height=offset$height,
                        hAnchor=offset$hAnchor, vAnchor=offset$vAnchor,
                        dx=offset$x, dy=offset$y,
                        dpi=x$dpi,
                        state=state)
        x <- setChildren(x, do.call(gList, grobs))
    } 
    x
}

xDetails.DVIgrob <- function(x, theta) {
    if (length(x$objList)) {
        state <- addMargin(x$margin, x$state)
        offset <- calculateOffset(x$x, x$y, x$hjust, x$vjust, state)
        vp <- rotVP(x$x, x$y, x$rot)
        left <- TeX2pt(TeXget("left", state), state)
        right <- TeX2pt(TeXget("right", state), state)
        bottom <- -TeX2pt(TeXget("bottom", state), state)
        top <- -TeX2pt(TeXget("top", state), state)
        x <- c(left + offset$x, left + offset$x,
               right + offset$x, right + offset$x)
        y <- c(bottom + offset$y, top + offset$y,
               top + offset$y, bottom + offset$y)
        coords <- grobCoords(polygonGrob(x, y,
                                         default.units="bigpts",
                                         vp=vp))
        xDetails(rectGrob(min(coords[[1]]$x), min(coords[[1]]$y),
                          diff(range(coords[[1]]$x)), diff(range(coords[[1]]$y)),
                          default.units="in",
                          just=c("left", "bottom")),
                 theta)
    } else {
        unit(.5, "npc")
    }
}

yDetails.DVIgrob <- function(x, theta) {
    if (length(x$objList)) {
        state <- addMargin(x$margin, x$state)
        offset <- calculateOffset(x$x, x$y, x$hjust, x$vjust, state)
        vp <- rotVP(x$x, x$y, x$rot)
        left <- TeX2pt(TeXget("left", state), state)
        right <- TeX2pt(TeXget("right", state), state)
        bottom <- -TeX2pt(TeXget("bottom", state), state)
        top <- -TeX2pt(TeXget("top", state), state)
        x <- c(left + offset$x, left + offset$x,
               right + offset$x, right + offset$x)
        y <- c(bottom + offset$y, top + offset$y,
               top + offset$y, bottom + offset$y)
        coords <- grobCoords(polygonGrob(x, y,
                                         default.units="bigpts",
                                         vp=vp))
        yDetails(rectGrob(min(coords[[1]]$x), min(coords[[1]]$y),
                          diff(range(coords[[1]]$x)), diff(range(coords[[1]]$y)),
                          default.units="in",
                          just=c("left", "bottom")),
                 theta)
    } else {
        unit(.5, "npc")
    }
}

widthDetails.DVIgrob <- function(x) {
    if (length(x$objList)) {
        state <- addMargin(x$margin, x$state)
        offset <- calculateOffset(x$x, x$y, x$hjust, x$vjust, state)
        vp <- rotVP(x$x, x$y, x$rot)
        left <- TeX2pt(TeXget("left", state), state)
        right <- TeX2pt(TeXget("right", state), state)
        bottom <- -TeX2pt(TeXget("bottom", state), state)
        top <- -TeX2pt(TeXget("top", state), state)
        x <- c(left + offset$x, left + offset$x,
               right + offset$x, right + offset$x)
        y <- c(bottom + offset$y, top + offset$y,
               top + offset$y, bottom + offset$y)
        coords <- grobCoords(polygonGrob(x, y,
                                         default.units="bigpts",
                                         vp=vp))
        unit(diff(range(coords[[1]]$x)), "in")
    } else {
        unit(0, "mm")
    }
}

heightDetails.DVIgrob <- function(x) {
    if (length(x$objList)) {
        state <- addMargin(x$margin, x$state)
        offset <- calculateOffset(x$x, x$y, x$hjust, x$vjust, state)
        vp <- rotVP(x$x, x$y, x$rot)
        left <- TeX2pt(TeXget("left", state), state)
        right <- TeX2pt(TeXget("right", state), state)
        bottom <- -TeX2pt(TeXget("bottom", state), state)
        top <- -TeX2pt(TeXget("top", state), state)
        x <- c(left + offset$x, left + offset$x,
               right + offset$x, right + offset$x)
        y <- c(bottom + offset$y, top + offset$y,
               top + offset$y, bottom + offset$y)
        coords <- grobCoords(polygonGrob(x, y,
                                         default.units="bigpts",
                                         vp=vp))
        unit(diff(range(coords[[1]]$y)), "in")
    } else {
        unit(0, "mm")
    }
}

grobCoords.DVIgrob <- function(x, closed=TRUE, ...) {
    if (closed && length(x$objList)) {
        state <- addMargin(x$margin, x$state)
        offset <- calculateOffset(x$x, x$y, x$hjust, x$vjust, state)
        vp <- rotVP(x$x, x$y, x$rot)
        left <- TeX2pt(TeXget("left", state), state)
        right <- TeX2pt(TeXget("right", state), state)
        bottom <- -TeX2pt(TeXget("bottom", state), state)
        top <- -TeX2pt(TeXget("top", state), state)
        x <- c(left + offset$x, left + offset$x,
               right + offset$x, right + offset$x)
        y <- c(bottom + offset$y, top + offset$y,
               top + offset$y, bottom + offset$y)
        grobCoords(polygonGrob(x, y,
                               default.units="bigpts",
                               vp=vp))        
    } else {
        emptyGrobCoords(x$name)
    }
}

resolveDVI <- function(dvi, ...) {
    UseMethod("resolveDVI")
}

resolveDVI.DVI <- function(dvi, ...) {
    dvi
}

resolveDVI.character <- function(dvi, ...) {
    readDVI(dvi)
}

repGPar <- function(gp, n) {
    if (length(gp)) {
        ngp <- max(sapply(gp, length))
        lapply(1:n, function(i) gp[(i - 1) %% ngp + 1])
    } else {
        rep(list(gpar()), length.out=n)
    }
}

makeContent.DVIgTree <- function(x, ...) {
    children <- mapply(buildDVIgrob,
                       x$objList, x$x, x$y, x$rot, x$hjust, x$vjust,
                       name=paste0(x$name, ".", 1:x$n), x$gpars, x$state,
                       MoreArgs=list(margin=x$margin, dpi=x$dpi),
                       SIMPLIFY=FALSE)
    x <- setChildren(x, do.call(gList, children))
    x
}

xDetails.DVIgTree<- function(x, theta) {
    children <- mapply(buildDVIgrob,
                       x$objList, x$x, x$y, x$rot, x$hjust, x$vjust,
                       name=paste0(x$name, ".", 1:x$n), x$gpars, x$state,
                       MoreArgs=list(margin=x$margin, dpi=x$dpi),
                       SIMPLIFY=FALSE)
    coords <- lapply(children, grobCoords)
    x <- unlist(lapply(coords, function(x) x[[1]]$x))
    y <- unlist(lapply(coords, function(x) x[[1]]$y))
    xDetails(rectGrob(min(x), min(y),
                      diff(range(x)), diff(range(y)),
                      default.units="in",
                      just=c("left", "bottom")),
             theta)
}

yDetails.DVIgTree <- function(x, theta) {
    children <- mapply(buildDVIgrob,
                       x$objList, x$x, x$y, x$rot, x$hjust, x$vjust,
                       name=paste0(x$name, ".", 1:x$n), x$gpars, x$state,
                       MoreArgs=list(margin=x$margin, dpi=x$dpi),
                       SIMPLIFY=FALSE)
    coords <- lapply(children, grobCoords)
    x <- unlist(lapply(coords, function(x) x[[1]]$x))
    y <- unlist(lapply(coords, function(x) x[[1]]$y))
    yDetails(rectGrob(min(x), min(y),
                      diff(range(x)), diff(range(y)),
                      default.units="in",
                      just=c("left", "bottom")),
             theta)
}

widthDetails.DVIgTree <- function(x) {
    children <- mapply(buildDVIgrob,
                       x$objList, x$x, x$y, x$rot, x$hjust, x$vjust,
                       name=paste0(x$name, ".", 1:x$n), x$gpars, x$state,
                       MoreArgs=list(margin=x$margin, dpi=x$dpi),
                       SIMPLIFY=FALSE)
    coords <- lapply(children, grobCoords)
    x <- unlist(lapply(coords, function(x) x[[1]]$x))
    unit(diff(range(x)), "in")
}

heightDetails.DVIgTree <- function(x) {
    children <- mapply(buildDVIgrob,
                       x$objList, x$x, x$y, x$rot, x$hjust, x$vjust,
                       name=paste0(x$name, ".", 1:x$n), x$gpars, x$state,
                       MoreArgs=list(margin=x$margin, dpi=x$dpi),
                       SIMPLIFY=FALSE)
    coords <- lapply(children, grobCoords)
    y <- unlist(lapply(coords, function(x) x[[1]]$y))
    unit(diff(range(y)), "in")
}

################################################################################
## User API

dviGrob <- function(dvi, ...) {
    UseMethod("dviGrob")
}

dviGrob.DVI <- function(dvi, ...,
                        packages=NULL,
                        engine=getOption("xdvir.engine")) {
    eng <- resolveEngine(dvi, engine)
    pkgs <- resolvePackages(packages)
    pkgs <- checkPackages(pkgs, typesetPackages(dvi))
    dviGrob(list(dvi), ..., engine=eng, packages=pkgs)
}

dviGrob.character <- function(dvi, ...) {
    if (length(dvi) < 1)
        stop("No DVI files to render")
    dvi <- lapply(dvi, readDVI)
    dviGrob(dvi, ...)
}

## Vectorised
dviGrob.list <- function(dvi,
                         x=0.5, y=0.5,
                         margin=0,
                         rot=0,
                         default.units="npc",
                         hjust="centre", vjust="centre",
                         dpi=NA,
                         page=1,
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
    if (!is.unit(margin))
        margin <- unit(margin, default.units)
    margin <- rep(margin, length.out=4)
    ## Vectorise arguments
    n <- max(length(x), length(y), length(dvi))
    dvi <- rep(dvi, length.out=n)
    x <- rep(x, length.out=n)
    y <- rep(y, length.out=n)
    rot <- rep(rot, length.out=n)
    hjust <- rep(hjust, length.out=n)
    vjust <- rep(vjust, length.out=n)
    pages <- rep(page, length.out=n)
    gpars <- repGPar(gp, n)
    ## Resolve args
    dvis <- lapply(dvi, resolveDVI)
    engines <- lapply(dvi, resolveEngine, engine)
    lib <- resolveFontLib(fontLib)
    pkgs <- resolvePackages(packages)
    packages <- mapply(function(dviPkgs, userPkgs) {
                           checkPackages(userPkgs, dviPkgs)
                       },
                       lapply(dvi, typesetPackages),
                       MoreArgs=list(userPkgs=pkgs),
                       SIMPLIFY=FALSE)
    states <- mapply(function(pkgs, eng, fontLib, dpi) {
                         buildState(pkgs, fontLib, eng, dpi)
                     },
                     packages, engines,
                     MoreArgs=list(fontLib=fontLib, dpi=dpi),
                     SIMPLIFY=FALSE)
    objLists <- mapply(buildObjList, dvis, pages, states,
                       SIMPLIFY=FALSE)
    gTree(n=n, dvi=dvis, state=states, objList=objLists,
          x=x, y=y, margin=margin, rot=rot,
          hjust=hjust, vjust=vjust,
          dpi=dpi, 
          gpars=gpars, name=name, vp=vp,
          cl="DVIgTree")
}

grid.dvi <- function(...) {
    grid.draw(dviGrob(...))
}

render <- grid.dvi
