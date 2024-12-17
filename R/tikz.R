
################################################################################
## Code to support specials:

## ops
## Called from setChar()
tikzTransform <- function(state) {
    transform <- TeXget("tikzTransform", state)
    transformDecomp <- TeXget("tikzTransformDecomp", state)
    length(transform) &&
        !(is.null(transformDecomp) ||
          (transformDecomp$rot == 0 &&
           all(transformDecomp$sc == 1) &&
           all(transformDecomp$sk == 0)))
}

## Transformed version of setChar()
setTransformedChar <- function(raw, put=FALSE, state) {
    transform <- TeXget("tikzTransformDecomp", state)
    h <- TeXget("h", state)
    v <- TeXget("v", state)
    hh <- TeXget("hh", state)
    vv <- TeXget("vv", state)
    ## Current font
    fonts <- TeXget("fonts", state)
    f <- TeXget("f", state)
    font <- fonts[[f]]
    colour <- TeXget("colour", state)
    fontLib <- TeXget("fontLib", state)
    id <- glyphIndex(raw)
    bbox <- TeXglyphBounds(id, font$file, font$size, fontLib, state)
    width <- TeXglyphWidth(id, font$file, font$size, fontLib, state)
    ## Position glyph then move
    x <- h
    y <- v
    xx <- hh
    yy <- vv
    glyph <- glyph(x, y, xx, yy, id, f, font$size, colour=colour[1],
                   transform$rot, transform$sc[1], transform$sc[2],
                   transform$sk[1], transform$sk[2])
    ## Bounding box requires transformation
    ## Transformation is for "bigpts"
    tm <- TeXget("tikzTransformText", state)
    bboxLeft <- TeX2pt(h + bbox[1], state)
    bboxRight <- TeX2pt(h + bbox[3], state)
    bboxBottom <- TeX2pt(v - bbox[2], state)
    bboxTop <- TeX2pt(v - bbox[4], state)
    bboxCorners <- cbind(c(bboxLeft, bboxBottom, 1),
                         c(bboxRight, bboxBottom, 1),
                         c(bboxLeft, bboxTop, 1),
                         c(bboxRight, bboxTop, 1))
    transCorners <- tm %*% bboxCorners
    bboxX <- pt2TeX(transCorners[1,], state)
    bboxY <- pt2TeX(-transCorners[2,], state)
    lapply(bboxX, updateBBoxHoriz, state)
    lapply(bboxY, updateBBoxVert, state)
    if (!put) {
        HV <- matrix(TeX2pt(c(h + width[1], -v, 1), state))
        transHV <- tm %*% HV
        h <- pt2TeX(transHV[1], state)
        v <- pt2TeX(-transHV[2], state)
        TeXset("h", h, state)
        TeXset("v", v, state)
        TeXset("hh", round(TeX2px(h, state)), state)
        TeXset("vv", round(TeX2px(v, state)), state)
    }
    addGlyph(glyph)
}

## Transformed version of setGlyphs()
setTransformedGlyphs <- function(op, state) {
    transform <- TeXget("tikzTransformDecomp", state)
    h <- TeXget("h", state)
    v <- TeXget("v", state)
    hh <- TeXget("hh", state)
    vv <- TeXget("vv", state)
    ## Current font
    fonts <- TeXget("fonts", state)
    f <- TeXget("f", state)
    font <- fonts[[f]]
    colour <- TeXget("colour", state)
    fontLib <- TeXget("fontLib", state)
    ## NOTE:
    ##   No concept of text direction (in XDV)
    ##   We have an ARRAY of glyphs
    nGlyphs <- blockValue(op$blocks$op.opparams.n)
    glyphIds <- blockValue(op$blocks$op.opparams.glyphs.id)
    glyphLocs <- paste0("op.opparams.glyphs.xy", 1:(2*nGlyphs))
    glyphH <- 0
    glyphV <- 0
    for (i in 1:nGlyphs) {
        id <- glyphIds[i]
        glyphX <- blockValue(op$blocks[[glyphLocs[2*i - 1]]])
        glyphY <- blockValue(op$blocks[[glyphLocs[2*i]]])
        x <- h + glyphX
        y <- v - glyphY
        xx <- round(TeX2px(x, state))
        yy <- round(TeX2px(y, state))
        glyph <- glyph(x, y, xx, yy, id, f, font$size, colour=colour[1],
                       transform$rot, transform$sc[1], transform$sc[2],
                       transform$sk[1], transform$sk[2])
        ## Update bounding box of drawing
        ## BUT do NOT update h/v
        bbox <- TeXglyphBounds(id, font$file, font$size, fontLib, state)
        width <- TeXglyphWidth(id, font$file, font$size, fontLib, state)
        ## Bounding box requires transformation
        tm <- TeXget("tikzTransformText", state)
        bboxLeft <- TeX2pt(h + bbox[1], state)
        bboxRight <- TeX2pt(h + bbox[3], state)
        bboxBottom <- TeX2pt(v - bbox[2], state)
        bboxTop <- TeX2pt(v - bbox[4], state)
        bboxCorners <- cbind(c(bboxLeft, bboxBottom, 1),
                             c(bboxRight, bboxBottom, 1),
                             c(bboxLeft, bboxTop, 1),
                             c(bboxRight, bboxTop, 1))
        transCorners <- tm %*% bboxCorners
        bboxX <- pt2TeX(transCorners[1,], state)
        bboxY <- pt2TeX(-transCorners[2,], state)
        lapply(bboxX, updateBBoxHoriz, state)
        lapply(bboxY, updateBBoxVert, state)
        addGlyph(glyph, state)
    }
    ## Update h/v at the end for all glyphs
    w <- blockValue(op$blocks$op.opparams.w)
    HV <- matrix(TeX2pt(c(h + w, -v, 1), state))
    transHV <- tm %*% HV
    h <- pt2TeX(transHV[1], state)
    v <- pt2TeX(-transHV[2], state)
    TeXset("h", h, state)
    TeXset("v", v, state)
    TeXset("hh", round(TeX2px(h, state)), state)
    TeXset("vv", round(TeX2px(v, state)), state)
}

## Build grobs from objects
buildTikZobj <- function(obj, dx, dy, grobFn, gp) {
    x <- convertX(obj$x, "bigpts", valueOnly=TRUE) + dx
    y <- convertY(obj$y, "bigpts", valueOnly=TRUE) + dy
    grobFn(x, y, default.units="bigpts", gp=gp)
}

objToGrob.XDVIRtikzPathObj <- function(obj, dx, dy, ...) {
    buildTikZobj(obj, dx, dy, pathGrob, gpar(fill=NA))
}

objToGrob.XDVIRtikzPolylineObj <- function(obj, dx, dy, ...) {
    buildTikZobj(obj, dx, dy, polylineGrob, gpar())
}

objToGrob.XDVIRtikzFillObj <- function(obj, dx, dy, ...) {
    buildTikZobj(obj, dx, dy, pathGrob, gpar(col=NA))
}

buildTikZstretchObj <- function(obj, dx, dy, grobFn, gp) {
    scaleX <- obj$transform$sc[1]
    scaleY <- obj$transform$sc[2]
    skewX <- obj$transform$sk[1]
    skewY <- obj$transform$sk[2]
    defvp <- viewport(0, 0,
                      just=c("left", "bottom"),
                      width=1, height=1)
    defgrob <- defineGrob(grobFn(obj$x, obj$y, default.units="bigpts", gp=gp),
                          vp=defvp,
                          name="xdvirPolylineDef")
    usevp <- viewport(unit(obj$lx + dx, "bigpts"),
                      unit(obj$by + dy, "bigpts"),
                      just=c("left", "bottom"),
                      width=scaleX,
                      height=scaleY)
    usegrob <- useGrob("xdvirPolylineDef",
                       vp=usevp,
                       transform=function(group, ...) {
                           viewportTransform(group,
                                             shear=groupShear(skewX, skewY),
                                             flip=groupFlip(scaleX < 0,
                                                            scaleY < 0))
                       })
    gTree(children=gList(defgrob, usegrob))
}

objToGrob.XDVIRtikzStretchPathObj <- function(obj, dx, dy, ...) {
    buildTikZstretchObj(obj, dx, dy, pathGrob, gpar(fill=NA))
}

objToGrob.XDVIRtikzStretchPolylineObj <- function(obj, dx, dy, ...) {
    buildTikZstretchObj(obj, dx, dy, polylineGrob, gpar())
}

objToGrob.XDVIRtikzStretchFillObj <- function(obj, dx, dy, ...) {
    buildTikZstretchObj(obj, dx, dy, pathGrob, gpar(col=NA))
}

objToGrob.XDVIRtikzParentObj <- function(obj, dx, dy, ...) {
    children <- obj$children
    parent <- NULL
    gp <- do.call(gpar, obj$gs)
    if (!is.null(children)) {
        parent <- gTree(gp=gp)
        parent <- setChildren(parent,
                              do.call(gList,
                                      lapply(children, objToGrob,
                                             dx, dy)))
    }
    parent
}

objToGrob.XDVIRtikzObj <- function(obj, dx, dy, ..., state) {
    gTree(children=do.call(gList,
                           lapply(obj$children, objToGrob, dx, dy)))
}

buildRotatedGlyph <- function(obj, dx, dy, state) {
    ## NEGATE vertical values (because +ve vertical is DOWN in DVI)
    x <- unit(TeX2pt(obj$x, state) + dx, "bigpts")
    y <- unit(-TeX2pt(obj$y, state) + dy, "bigpts")
    vp <- viewport(x, y, 
                   just=c("left", "bottom"), angle=obj$rotation/pi*180)
    font <- TeXget("fonts", state)[[obj$fontindex]]
    glyphFont <- glyphFont(font$file, font$index, "", 0, "")
    info <- glyphInfo(obj$index, 0, 0,
                      1, 
                      TeX2pt(obj$size, state),
                      glyphFontList(glyphFont),
                      1, ## Does not matter because will be left-bottom aligned
                      1, ## Does not matter because will be left-bottom aligned
                      col=obj$colour)
    glyphGrob(info, 0, 0, hjust="left", vjust="bottom", vp=vp)
}

objToGrob.XDVIRrotatedGlyphObj <- function(obj, dx, dy, ..., state) {
    children <- lapply(1:nrow(obj),
                       function(i)
                           buildRotatedGlyph(obj[i,], dx, dy, state))
    gTree(children=do.call(gList, children))
}

buildTransformedGlyph <- function(obj, dx, dy, state) {
    scaleX <- obj$scaleX
    scaleY <- obj$scaleY
    skewX <- obj$skewX
    skewY <- obj$skewY
    angle <- obj$rotation/pi*180
    
    ## NEGATE vertical values (because +ve vertical is DOWN in DVI)
    x <- unit(TeX2pt(obj$x, state) + dx, "bigpts")
    y <- unit(-TeX2pt(obj$y, state) + dy, "bigpts")
    
    font <- TeXget("fonts", state)[[obj$fontindex]]
    glyphFont <- glyphFont(font$file, font$index, "", 0, "")
    info <- glyphInfo(obj$index, 0, 0,
                      1, 
                      TeX2pt(obj$size, state),
                      glyphFontList(glyphFont),
                      1, ## Does not matter because will be left-bottom aligned
                      1, ## Does not matter because will be left-bottom aligned
                      col=obj$colour)
    
    defvp <- viewport(0, 0, just=c("left", "bottom"),
                      width=1, height=1)
    defgrob <- defineGrob(glyphGrob(info, 0, 0, hjust="left", vjust="bottom"),
                          vp=defvp,
                          name="xdvirGlyphDef")
    usevp <- viewport(x, y, just=c("left", "bottom"),
                      angle=angle,
                      width=scaleX,
                      height=scaleY)
    usegrob <- useGrob("xdvirGlyphDef",
                       vp=usevp,
                       transform=function(group, ...) {
                           viewportTransform(group,
                                             shear=groupShear(skewX, skewY),
                                             flip=groupFlip(scaleX < 0,
                                                            scaleY < 0))
                       })
    gTree(children=gList(defgrob, usegrob))
}

objToGrob.XDVIRtransformedGlyphObj <- function(obj, dx, dy, ...,
                                               state) {
    children <- lapply(1:nrow(obj),
                       function(i)
                           buildTransformedGlyph(obj[i,], dx, dy,
                                                 state))
    gTree(children=do.call(gList, children))
}

## Create objects

## Based on
## https://math.stackexchange.com/questions/13150/extracting-rotation-scale-values-from-2d-transformation-matrix/13165#13165
decompose <- function(m) {
    a <- m[1]
    b <- m[2]
    c <- m[4]
    d <- m[5]
    e <- m[7]
    f <- m[8]
    
    delta <- a*d - b*c

    translation <- c(e, f)
    ## Apply the QR-like decomposition.
    if (a != 0 || b != 0) {
        r <- sqrt(a*a + b*b)
        rotation <- if (b > 0) acos(a/r) else -acos(a/r)
        scale <- c(r, delta/r)
        skew <- c(atan2((a*c + b*d), (r*r)), 0)
    } else if (c != 0 || d != 0) {
        s <- sqrt(c*c + d*d)
        rotation <- pi/2 - if (d > 0) acos(-c/s) else -acos(c/s)
        scale <- c(delta/s, s)
        skew <- c(0, atan2((a*c + b*d), (s*s)))
    } else {
        ## a <- b <- c <- d <- 0
        stop("Invalid transformation matrix")
    }
    list(tr=translation,
         rot=round(rotation, 3),
         sc=round(scale, 3),
         sk=round(skew, 3))
}

tikzNameGen <- function() {
    vpIndex <- 0
    function() {
        vpIndex <<- vpIndex + 1
        paste0("tikz", vpIndex)
    }
}
tikzName <- tikzNameGen()

parseMoveTo <- function(x, i, state) {
    xy <- strsplit(x, ",")[[1]]
    sub <- TeXget("tikzSubPath", state) + 1
    pathX <- TeXget("tikzPathX", state)
    pathY <- TeXget("tikzPathY", state)
    pathX[[sub]][[i]] <- as.numeric(xy[1])
    pathY[[sub]][[i]] <- as.numeric(xy[2])
    TeXset("tikzPathX", pathX, state)
    TeXset("tikzPathY", pathY, state)
    TeXset("tikzSubPath", sub, state)
}

parseLineTo <- function(x, i, state) {
    xy <- strsplit(x, ",")[[1]]
    sub <- TeXget("tikzSubPath", state)
    pathX <- TeXget("tikzPathX", state)
    pathY <- TeXget("tikzPathY", state)
    pathX[[sub]][[i]] <- as.numeric(xy[1])
    pathY[[sub]][[i]] <- as.numeric(xy[2])
    TeXset("tikzPathX", pathX, state)
    TeXset("tikzPathY", pathY, state)
}

parseCurveTo <- function(x, i, state) {
    xy <- strsplit(x, ",")[[1]]
    sub <- TeXget("tikzSubPath", state)
    pathX <- TeXget("tikzPathX", state)
    startX <- pathX[[sub]][[i - 1]][length(pathX[[sub]][[i - 1]])]
    pathY <- TeXget("tikzPathY", state)
    startY <- pathY[[sub]][[i - 1]][length(pathY[[sub]][[i - 1]])]    
    ## Convert Bezier to polyline
    bg <- gridBezier::BezierGrob(x=unit(c(startX, xy[c(1, 3, 5)]), units="pt"),
                                 y=unit(c(startY, xy[c(2, 4, 6)]), units="pt"))
    pts <- gridBezier::BezierPoints(bg)
    pathX[[sub]][[i]] <- convertX(unit(pts$x[-1], "in"), "pt", valueOnly=TRUE)
    pathY[[sub]][[i]] <- convertY(unit(pts$y[-1], "in"), "pt", valueOnly=TRUE)
    TeXset("tikzPathX", pathX, state)
    TeXset("tikzPathY", pathY, state)
}

parseClose <- function(i, state) {
    ## Start new subPath
    sub <- TeXget("tikzSubPath", state) + 1
    ## Mark old subPath as closed
    closed <- TeXget("tikzPathClosed", state)
    closed[sub - 1] <- TRUE
    TeXset("tikzPathClosed", closed, state)
    ## New path begins at start point of previous subPath
    ## (this may immediately get superceded by moveto, BUT OTOH it may NOT)
    pathX <- TeXget("tikzPathX", state)
    pathY <- TeXget("tikzPathY", state)
    pathX[[sub]][[i]] <- pathX[[sub - 1]][[1]]
    pathY[[sub]][[i]] <- pathY[[sub - 1]][[1]]
    TeXset("tikzPathX", pathX, state)
    TeXset("tikzPathY", pathY, state)
    TeXset("tikzSubPath", sub, state)
}

recordPathElement <- function(x, i, state) {
    tokens <- strsplit(x, " ")[[1]]
    if (i == 1 && tokens[1] != "moveto") {
        stop("Invalid path (must begin with moveto)")
    }
    switch(tokens[1],
           moveto=parseMoveTo(tokens[-1], i, state),
           lineto=parseLineTo(tokens[-1], i, state),
           curveto=parseCurveTo(tokens[-1], i, state),
           close=parseClose(i, state),
           stop("unsupported path element"))
}

## Text colour is "fill" in TikZ, but "colour" in TeX
## "colour" needs to be set immediately to affect setChar etc;  see recordGS()
## At begin/end scope, need to save/restore "colour" 
pushTextColour <- function(gs, state) {
    col <- TeXget("colour", state)
    TeXset("tikzSavedColour",
           c(col[1], TeXget("tikzSavedColour", state)), state)
}

popTextColour <- function(state) {
    col <- TeXget("tikzSavedColour", state)
    TeXset("tikzSavedColour", col[-1], state)
    TeXset("colour", col[1], state)
}

addParent <- function(x, state) {
    gsStack <- TeXget("tikzGS", state)
    gs <- gsStack[[1]]
    parent <- list(children=NULL, gs=gs)
    class(parent) <- "XDVIRtikzParentObj"
    TeXset("tikzParent", c(list(parent), TeXget("tikzParent", state)), state)
    pushTextColour(gs, state)
}

reduceParent <- function(state) {
    parent <- TeXget("tikzParent", state)
    if (length(parent) == 1) {
        addTikzObj(parent[[1]], state)
    } else {
        parent[[2]]$children <- c(parent[[2]]$children, parent[1])
    }
    TeXset("tikzParent", parent[-1], state)
    popTextColour(state)
}

addChild <- function(x, state) {
    parent <- TeXget("tikzParent", state)
    parent[[1]]$children <- c(parent[[1]]$children, list(x))
    TeXset("tikzParent", parent, state)
}

recordNewPath <- function(x, state) {
    TeXset("tikzPathX", NULL, state)
    TeXset("tikzPathY", NULL, state)
    addParent(x, state)
}

recordGS <- function(x, state) {
    gsStack <- TeXget("tikzGS", state)
    gs <- gsStack[[1]]
    if (length(x) > 0) {
        tokens <- strsplit(x, "=")
        names <- sapply(tokens, "[", 1)
        values <- lapply(tokens, parseSetting)
        names(values) <- names
        gs[names] <- handleOpacity(values)
        gsStack[[1]] <- gs
        TeXset("tikzGS", gsStack, state)
        ## Text colour is "fill" in TikZ, but "colour" in TeX
        ## "colour" needs to be set immediately to affect setChar etc
        ## At begin/end scope, need to save/restore "colour";
        ## see pushTextColour()
        if ("fill" %in% names) {
            col <- TeXget("colour", state)
            col[1] <- gs$fill
            TeXset("colour", col, state)
        }
    }
}

tikzStretch <- function(transform, transformDecomp) {
    length(transform) &&
        !is.null(transformDecomp) &&
        (any(transformDecomp$sc != 1) ||
         any(transformDecomp$sk != 0))
}

stretchPaths <- function(px, py, cl, lx, by, transform, transformDecomp,
                         closedClass, openClass, state) {
    x <- unlist(px)
    y <- unlist(py)
    if (length(unlist(px)) > 1) {
        ## Apply current transform (if any)
        ## NOTE that in stretch case this is just for bbox maintenance;
        ##      the actual transform is implemented later in objToGrob()
        if (length(transform)) {
            tm <- transform[[1]]
            xy <- tm %*% rbind(x, y, 1)
            xx <- xy[1,]
            yy <- xy[2,]
        }
        xx <- lx + xx
        ## Negate by because TikZ is "up" while TeX is "down"
        yy <- -by + yy
        rx <- range(pt2TeX(xx, state))
        updateBBoxHoriz(rx[1], state)
        updateBBoxHoriz(rx[2], state)
        ry <- -range(pt2TeX(yy, state))
        updateBBoxVert(ry[1], state)
        updateBBoxVert(ry[2], state)
        if (cl) {
            child <- list(lx=lx,
                          by=-by,
                          x=unit(x, "pt"),
                          y=unit(y, "pt"),
                          transform=transformDecomp)
            class(child) <- closedClass
        } else {
            child <- list(lx=lx,
                          by=-by,
                          x=unit(x, "pt"),
                          y=unit(y, "pt"),
                          transform=transformDecomp)
            class(child) <- openClass
        }
        addChild(child, state)
    }
}

strokeStretchPaths <- function(px, py, cl, lx, by, transform,
                               transformDecomp, state) {
    stretchPaths(px, py, cl, lx, by, transform, transformDecomp,
                 "XDVIRtikzStretchPathObj", "XDVIRtikzStretchPolylineObj",
                 state)
}

fillStretchPaths <- function(px, py, cl, lx, by, transform,
                             transformDecomp, state) {
    stretchPaths(px, py, cl, lx, by, transform, transformDecomp,
                 "XDVIRtikzStretchFillObj", NA, state)
}

drawPaths <- function(px, py, cl, lx, by, transform,
                      closedClass, openClass, state) {
    x <- unlist(px)
    y <- unlist(py)
    if (length(unlist(px)) > 1) {
        ## Apply current transform (if any)
        if (length(transform)) {
            tm <- transform[[1]]
            xy <- tm %*% rbind(x, y, 1)
            x <- xy[1,]
            y <- xy[2,]
        }
        x <- lx + x
        ## Negate by because TikZ is "up" while TeX is "down"
        y <- -by + y
        rx <- range(pt2TeX(x, state))
        updateBBoxHoriz(rx[1], state)
        updateBBoxHoriz(rx[2], state)
        ry <- -range(pt2TeX(y, state))
        updateBBoxVert(ry[1], state)
        updateBBoxVert(ry[2], state)
        if (cl) {
            child <- list(x=unit(x, "pt"),
                          y=unit(y, "pt"))
            class(child) <- closedClass
        } else {
            child <- list(x=unit(x, "pt"),
                          y=unit(y, "pt"))
            class(child) <- openClass
        }
        addChild(child, state)
    }
}

strokePaths <- function(px, py, cl, lx, by, transform, state) {
    drawPaths(px, py, cl, lx, by, transform,
              "XDVIRtikzPathObj", "XDVIRtikzPolylineObj", state)
}

fillPaths <- function(px, py, cl, lx, by, transform, state) {
    drawPaths(px, py, cl, lx, by, transform,
              "XDVIRtikzFillObj", NA, state)
}

recordDraw <- function(draw, drawStretch, state) {
    pathX <- TeXget("tikzPathX", state)
    pathY <- TeXget("tikzPathY", state)
    closed <- TeXget("tikzPathClosed", state)
    left <- TeXget("pictureLeft", state)
    bottom <- TeXget("pictureBottom", state)
    lx <- left
    by <- bottom
    transform <- TeXget("tikzTransform", state)
    transformDecomp <- TeXget("tikzTransformDecomp", state)
    ## Handle scale and skew separately because only some graphics
    ## devices can support (the R graphics implementation of) scale and skew
    ## (this allows most graphics devices to handle simpler transformations)
    if (tikzStretch(transform, transformDecomp)) { 
        mapply(drawStretch, pathX, pathY, closed,
               MoreArgs=list(lx, by, transform, transformDecomp, state=state))
    } else {
        mapply(draw, pathX, pathY, closed,
               MoreArgs=list(lx, by, transform, state=state))
    }
}

recordStroke <- function(state) {
    recordDraw(strokePaths, strokeStretchPaths, state)
    reduceParent(state)
}

recordFill <- function(state) {
    recordDraw(fillPaths, fillStretchPaths, state)
    reduceParent(state)
}

recordFillStroke <- function(state) {
    recordDraw(fillPaths, fillStretchPaths, state)
    recordDraw(strokePaths, strokeStretchPaths, state)
    reduceParent(state)
}

recordTransform <- function(x, state) {
    tokens <- as.numeric(strsplit(x, ",")[[1]])
    tm <- rbind(c(tokens[1], tokens[3], tokens[5]),
                c(tokens[2], tokens[4], tokens[6]),
                c(0, 0, 1))
    transform <- TeXget("tikzTransform", state)
    if (length(transform) == 0) {
        TeXset("tikzTransform", list(tm), state)
    } else {
        tm <- transform[[1]] %*% tm
        TeXset("tikzTransform", c(list(tm), transform), state)
    }
    td <- TeXget("tikzTransformDepth", state)
    td[1] <- td[1] + 1
    TeXset("tikzTransformDepth", td, state)
    ## TEMPORARILY set h/v (within TikZ picture)
    ## Transform is relative to picture bottom-left
    left <- TeXget("pictureLeft", state)
    bottom <- TeXget("pictureBottom", state)
    ## Move to location of text
    x <- left
    ## Negate y because TikZ is "up" while TeX is "down"
    y <- -bottom
    tmText <- rbind(c(1,0,0), c(0,-1,0), c(0,0,1)) %*%
        rbind(c(1,0,x), c(0,1,y), c(0,0,1)) %*%
        tm %*%
        rbind(c(1,0,-x), c(0,1,-y), c(0,0,1))
    xy <-  tmText %*% c(x, y, 1) 
    TeXset("h", pt2TeX(xy[1], state), state)
    TeXset("v", pt2TeX(xy[2], state), state)
    TeXset("tikzTransformText", tmText, state)
    TeXset("tikzTransformDecomp", decompose(tm), state)
}

parseValueWithUnit <- function(x) {
    unit <- gsub("[0-9.]+", "", x)
    value <- as.numeric(gsub("([0-9.]+).+", "\\1", x))
    switch(unit,
           bp=value/72,
           pt=value/72.27,
           stop("unsupported unit"))
}

parseLineDash <- function(x) {
    if (x == "none") {
        "solid"
    } else {
        ## Convert line-dash to #1234 format
        stop("not yet supported")
    }
}

parseSetting <- function(x) {
    name <- x[1]
    value <- x[2]
    switch(name,
           col=eval(str2lang(value)),
           fill=eval(str2lang(value)),
           lwd=96*parseValueWithUnit(value),
           lty=parseLineDash(value),
           lineend=value,
           linejoin=value,
           `stroke-opacity`=as.numeric(value),
           stop("unsupported setting"))
}

handleOpacity <- function(x) {
    if ("stroke-opacity" %in% names(x)) {
        if ("col" %in% names(x)) {
            x$col <- adjustcolor(x$col, alpha.f=x$"stroke-opacity")
        }
        x$"stroke-opacity" <- NULL
    }
    if ("fill-opacity" %in% names(x)) {
        if ("fill" %in% names(x)) {
            x$fill <- adjustcolor(x$fill, alpha.f=x$"fill-opacity")
        }
        x$"fill-opacity" <- NULL
    }
    x
}

addTikzObj <- function(x, state) {
    tikzObj <- x
    class(tikzObj) <- "XDVIRtikzObj"
    addDVIobj(tikzObj, state)
}

recordBeginScope <- function(x, state) {
    gsStack <- TeXget("tikzGS", state)
    TeXset("tikzGS", c(gsStack[1], gsStack), state)
    td <- TeXget("tikzTransformDepth", state)
    TeXset("tikzTransformDepth", c(0, td), state)
    addParent(x, state)
}

recordEndScope <- function(state) {
    gsStack <- TeXget("tikzGS", state)
    TeXset("tikzGS", gsStack[-1], state)
    td <- TeXget("tikzTransformDepth", state)
    if (td[1] > 0) {
        tm <- TeXget("tikzTransform", state)
        TeXset("tikzTransform", tm[-(1:td[1])], state)
    }
    TeXset("tikzTransformDepth", td[-1], state)
    reduceParent(state)
}

## Generate object from TikZ special
recordSpecial <- function(x, state) {
    ## Ignore "blanks"
    if (grepl("^ *$", x)) return()
    ## Split by ": " (for paths)
    tokens <- strsplit(gsub("^ *| *$", "", x), ":")[[1]]
    if (length(tokens) == 0) {
        warning("Empty special")
    } else if (length(tokens) == 1) {
        tokens <- strsplit(gsub(" *$", "", tokens), " ")[[1]]
        switch(tokens[1],
               `begin-scope`=recordBeginScope(tokens[-1], state),
               `end-scope`=recordEndScope(state),
               `new-path`=recordNewPath(tokens[-1], state),
               `gs`=recordGS(tokens[-1], state),
               `stroke`=recordStroke(state),
               `fill`=recordFill(state),
               `fill-stroke`=recordFillStroke(state), 
               `transform`=recordTransform(tokens[-1], state),
               stop("Unsupported TikZ special"))
    } else {
        ## Path
        n <- length(tokens)
        ## Count number of moveto's and close's
        nsub <- length(grep("moveto|close", tokens))
        ## Create subpath for each moveto and close
        TeXset("tikzSubPath", 0, state)
        ## (record path element i in component i of relevant subpath)
        TeXset("tikzPathX",
               lapply(1:nsub, function(i) vector("list", n)), state)
        TeXset("tikzPathY",
               lapply(1:nsub, function(i) vector("list", n)), state)
        ## Is each subpath closed ? (FALSE by default)
        TeXset("tikzPathClosed", logical(nsub), state)
        mapply(recordPathElement, tokens, 1:n, MoreArgs=list(state=state))
        invisible()
    }
}

recordBBox <- function(x, state) {
    tokens <- strsplit(gsub(" *$", "", x), " ")[[1]]
    bbox <- as.numeric(strsplit(gsub("pt|;", "", tokens[-1]), ",")[[1]])
    left <- TeXget("pictureLeft", state)
    bottom <- TeXget("pictureBottom", state)
    ## Update DVI bbox for TikZ bbox
    updateBBoxHoriz(pt2TeX(left +
                           convertX(unit(bbox[1], "pt"), "bigpts",
                                    valueOnly=TRUE), state),
                    state)
    updateBBoxVert(pt2TeX(bottom -
                          convertY(unit(bbox[2], "pt"), "bigpts",
                                   valueOnly=TRUE), state),
                   state)
    updateBBoxHoriz(pt2TeX(left +
                           convertX(unit(bbox[3], "pt"), "bigpts",
                                    valueOnly=TRUE),
                           state),
                    state)
    updateBBoxVert(pt2TeX(bottom -
                          convertY(unit(bbox[4], "pt"), "bigpts",
                                   valueOnly=TRUE), state),
                   state)
}

beginPicture <- function(state) {
    h <- TeXget("h", state)
    v <- TeXget("v", state)
    TeXset("savedH", h, state)
    TeXset("savedV", v, state)
    x <- TeX2pt(h, state)
    y <- TeX2pt(v, state)
    TeXset("pictureLeft", x, state)
    TeXset("pictureBottom", y, state)
    TeXset("inPicture", TRUE, state)
    TeXset("tikzParent", NULL, state)
    TeXset("tikzTransform", NULL, state)
    TeXset("tikzTransformDepth", 0, state)
    TeXset("tikzTransformDecomp", NULL, state)
    TeXset("tikzTransformText", diag(3), state)
    TeXset("tikzGS", list(list()), state)
    TeXset("tikzSavedColour", NA, state)
}

endPicture <- function(special, state) {
    ## recordBBox(special, state)
    TeXset("h", TeXget("savedH", state), state)
    TeXset("v", TeXget("savedV", state), state)        
    TeXset("inPicture", FALSE, state)
}

tikzSpecial <- function(specialString, state) {
    ## Ignore any other specials
    if (grepl("^xdvir:: ", specialString)) {
        special <- gsub("xdvir:: ", "", specialString)
        if (grepl("^begin-picture", special)) {
            beginPicture(state)
        } else if (grepl("^end-picture", special)) {
            endPicture(special, state)
        } else {
            if (TeXget("inPicture", state)) {
                ## Record special
                ## Output may be multiple specials from
                ## "protocolled" (recorded) output, so split first by ";"
                specials <- strsplit(special, ";")[[1]]
                lapply(specials, recordSpecial, state)
            }
        }
    }
}

################################################################################
## The basic preamble, etc
## NOTE that the preamble loads the xdvir-specific pgfsysdriver

tikzInit <- function(state) {
    TeXset("inPicture", FALSE, state)
}

tikzPreamble <- function(packages=NULL) {
    if (!is.null(packages)) {
        if (!is.character(packages))
            stop("Invalid TikZ packages")
        usepackages <- paste0("\\usetikzlibrary{", packages, "}",
                              collapse="\n")
    } else {
        usepackages <- NULL
    }
    ## NOTE: quote path in case it contains spaces
    c(paste0("\\def\\pgfsysdriver{'",
             system.file("tikz", "pgfsys-xdvir.def",
                         package="xdvir"),
             "'}"),
      "\\usepackage{tikz}",
      usepackages)
}

tikzPrefix <- "\\begin{tikzpicture}"

tikzSuffix <- function(bbox=NULL) {
    suffix <- "\\end{tikzpicture}"
    if (!is.null(bbox)) {
        if (is.numeric(bbox) && length(bbox) == 4) {
            suffix <- c(paste0("\\pgfresetboundingbox\\useasboundingbox (",
                               bbox[1], ",", bbox[2], ") rectangle (",
                               bbox[3], ",", bbox[4], ");"),
                        suffix)
        } else {
            stop("Invalid bbox")
        }
    }
    suffix
}

tikzPackage <- function(name="tikz", packages=NULL) {
    LaTeXpackage(name=name,
                 preamble=tikzPreamble(packages),
                 special=tikzSpecial,
                 init=tikzInit)
}

tikzPicture <- function(name="tikzPicture", packages=NULL, bbox=NULL) {
    LaTeXpackage(name=name,
                 preamble=tikzPreamble(packages),
                 prefix=tikzPrefix,
                 suffix=tikzSuffix(bbox),
                 special=tikzSpecial,
                 init=tikzInit)
}


