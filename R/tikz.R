
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
    x <- fromTeX(h, state)
    y <- fromTeX(v, state)
    glyph <- glyph(x, y, id, f, font$size, colour=colour[1],
                   transform$rot, transform$sc[1], transform$sc[2],
                   transform$sk[1], transform$sk[2])
    ## Bounding box requires transformation
    tm <- TeXget("tikzTransformText", state)
    bboxLeft <- convertX(unit(fromTeX(h + bbox[1], state), "mm"), "pt",
                         valueOnly=TRUE)
    bboxRight <- convertX(unit(fromTeX(h + bbox[2], state), "mm"), "pt",
                          valueOnly=TRUE)
    bboxBottom <- convertY(unit(fromTeX(v - bbox[3], state), "mm"), "pt",
                           valueOnly=TRUE)
    bboxTop <- convertY(unit(fromTeX(v - bbox[4], state), "mm"), "pt",
                        valueOnly=TRUE)
    bboxCorners <- cbind(c(bboxLeft, bboxBottom, 1),
                         c(bboxRight, bboxBottom, 1),
                         c(bboxLeft, bboxTop, 1),
                         c(bboxRight, bboxTop, 1))
    transCorners <- tm %*% bboxCorners
    bboxX <- toTeX(unit(transCorners[1,], "pt"), state)
    bboxY <- toTeX(-unit(transCorners[2,], "pt"), state)
    lapply(bboxX, updateBBoxHoriz, state)
    lapply(bboxY, updateBBoxVert, state)
    if (!put) {
        HV <- matrix(c(convertX(unit(fromTeX(h + width[1], state), "mm"), "pt",
                                valueOnly=TRUE),
                       convertY(unit(-y, "mm"), "pt", valueOnly=TRUE),
                       1))
        transHV <- tm %*% HV
        TeXset("h", toTeX(unit(transHV[1], "pt"), state), state)
        TeXset("v", toTeX(unit(-transHV[2], "pt"), state), state)
    }
    addGlyph(glyph)
}

## Transformed version of setGlyphs()
setTransformedGlyphs <- function(op, state) {
    transform <- TeXget("tikzTransformDecomp", state)
    h <- TeXget("h", state)
    v <- TeXget("v", state)
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
        xx <- fromTeX(x, state)
        yy <- fromTeX(y, state)
        glyph <- glyph(xx, yy, id, f, font$size, colour=colour[1],
                       transform$rot, transform$sc[1], transform$sc[2],
                       transform$sk[1], transform$sk[2])
        ## Update bounding box of drawing
        ## BUT do NOT update h/v
        bbox <- TeXglyphBounds(id, font$file, font$size, fontLib, state)
        width <- TeXglyphWidth(id, font$file, font$size, fontLib, state)
        ## Bounding box requires transformation
        tm <- TeXget("tikzTransformText", state)
        bboxLeft <- convertX(unit(fromTeX(h + bbox[1], state), "mm"), "pt",
                             valueOnly=TRUE)
        bboxRight <- convertX(unit(fromTeX(h + bbox[2], state), "mm"), "pt",
                              valueOnly=TRUE)
        bboxBottom <- convertY(unit(fromTeX(v - bbox[3], state), "mm"), "pt",
                               valueOnly=TRUE)
        bboxTop <- convertY(unit(fromTeX(v - bbox[4], state), "mm"), "pt",
                            valueOnly=TRUE)
        bboxCorners <- cbind(c(bboxLeft, bboxBottom, 1),
                             c(bboxRight, bboxBottom, 1),
                             c(bboxLeft, bboxTop, 1),
                             c(bboxRight, bboxTop, 1))
        transCorners <- tm %*% bboxCorners
        bboxX <- toTeX(unit(transCorners[1,], "pt"), state)
        bboxY <- toTeX(-unit(transCorners[2,], "pt"), state)
        lapply(bboxX, updateBBoxHoriz, state)
        lapply(bboxY, updateBBoxVert, state)
        HV <- matrix(c(convertX(unit(fromTeX(h + width[1], state), "mm"), "pt",
                                valueOnly=TRUE),
                       convertY(unit(-y, "mm"), "pt", valueOnly=TRUE),
                       1))
        transHV <- tm %*% HV
        ## Keep track of total glyph movement
        glyphH <- glyphH + transHV[1]
        glyphV <- glyphV + transHV[2]
        addGlyph(glyph, state)
    }
    ## Update h/v at the end for all glyphs
    TeXset("h", toTeX(unit(glyphH, "pt"), state), state)
    TeXset("v", toTeX(unit(-glyphV, "pt"), state), state)
}

## Build grobs from objects
buildTikZobj <- function(obj, xoffset, yoffset, grobFn, gp) {
    x <- convertX(obj$x, "bigpts", valueOnly=TRUE) +
        convertX(unit(xoffset, "in"), "bigpts", valueOnly=TRUE)
    y <- convertY(obj$y, "bigpts", valueOnly=TRUE) +
        convertY(unit(yoffset, "in"), "bigpts", valueOnly=TRUE)
    grobFn(x, y, default.units="bigpts", gp=gp)
}

objToGrob.XDVIRtikzPathObj <- function(obj, xoffset, yoffset, ...) {
    buildTikZobj(obj, xoffset, yoffset, pathGrob, gpar(fill=NA))
}

objToGrob.XDVIRtikzPolylineObj <- function(obj, xoffset, yoffset, ...) {
    buildTikZobj(obj, xoffset, yoffset, polylineGrob, gpar())
}

objToGrob.XDVIRtikzFillObj <- function(obj, xoffset, yoffset, ...) {
    buildTikZobj(obj, xoffset, yoffset, pathGrob, gpar(col=NA))
}

buildTikZstretchObj <- function(obj, xoffset, yoffset, grobFn, gp) {
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
    usevp <- viewport(unit(obj$lx, "bigpts") + unit(xoffset, "in"),
                      unit(obj$by, "bigpts") + unit(yoffset, "in"),
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

objToGrob.XDVIRtikzStretchPathObj <- function(obj, xoffset, yoffset, ...) {
    buildTikZstretchObj(obj, xoffset, yoffset, pathGrob, gpar(fill=NA))
}

objToGrob.XDVIRtikzStretchPolylineObj <- function(obj, xoffset, yoffset, ...) {
    buildTikZstretchObj(obj, xoffset, yoffset, polylineGrob, gpar())
}

objToGrob.XDVIRtikzStretchFillObj <- function(obj, xoffset, yoffset, ...) {
    buildTikZstretchObj(obj, xoffset, yoffset, pathGrob, gpar(col=NA))
}

objToGrob.XDVIRtikzParentObj <- function(obj, xoffset, yoffset, ...) {
    children <- obj$children
    parent <- NULL
    if (!is.null(children)) {
        parent <- gTree(gp=obj$gp)
        parent <- setChildren(parent,
                              do.call(gList,
                                      lapply(children, objToGrob,
                                             xoffset, yoffset)))
    }
    parent
}

objToGrob.XDVIRtikzObj <- function(obj, xoffset, yoffset, ..., state) {
    gTree(children=do.call(gList,
                           lapply(obj$children, objToGrob, xoffset, yoffset)))
}

buildRotatedGlyph <- function(obj, xoffset, yoffset, state) {
    ## NEGATE vertical values (because +ve vertical is DOWN in DVI)
    x <- unit(obj$x, "mm") + unit(xoffset, "in")
    y <- unit(-obj$y, "mm") + unit(yoffset, "in")
    vp <- viewport(x, y, just=c("left", "bottom"), angle=obj$rotation/pi*180)
    font <- TeXget("fonts", state)[[obj$fontindex]]
    glyphFont <- glyphFont(font$file, font$index, "", 0, "")
    info <- glyphInfo(obj$index, 0, 0,
                      1, 
                      obj$size,
                      glyphFontList(glyphFont),
                      1, ## Does not matter because will be left-bottom aligned
                      1, ## Does not matter because will be left-bottom aligned
                      col=obj$colour)
    glyphGrob(info, 0, 0, hjust="left", vjust="bottom", vp=vp)
}

objToGrob.XDVIRrotatedGlyphObj <- function(obj, xoffset, yoffset, ..., state) {
    children <- lapply(1:nrow(obj),
                       function(i)
                           buildRotatedGlyph(obj[i,], xoffset, yoffset, state))
    gTree(children=do.call(gList, children))
}

buildTransformedGlyph <- function(obj, xoffset, yoffset, state) {
    scaleX <- obj$scaleX
    scaleY <- obj$scaleY
    skewX <- obj$skewX
    skewY <- obj$skewY
    angle <- obj$rotation/pi*180
    
    ## NEGATE vertical values (because +ve vertical is DOWN in DVI)
    x <- unit(obj$x, "mm") + unit(xoffset, "in")
    y <- unit(-obj$y, "mm") + unit(yoffset, "in")
    
    font <- TeXget("fonts", state)[[obj$fontindex]]
    glyphFont <- glyphFont(font$file, font$index, "", 0, "")
    info <- glyphInfo(obj$index, 0, 0,
                      1, 
                      obj$size,
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

objToGrob.XDVIRtransformedGlyphObj <- function(obj, xoffset, yoffset, ...,
                                               state) {
    children <- lapply(1:nrow(obj),
                       function(i)
                           buildTransformedGlyph(obj[i,], xoffset, yoffset,
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

pushTextColour <- function(gp, state) {
    fill <- NA
    if ("fill" %in% names(gp)) {
        fill <- gp$fill
    }
    TeXset("tikzTextColour", c(gp$fill, TeXget("tikzTextColour", state)), state)
    TeXset("colour", fill, state)
}

popTextColour <- function(state) {
    fill <- TeXget("tikzTextColour", state)
    TeXset("tikzTextColour", fill[-1], state)
    TeXset("colour", fill[1], state)
}

addParent <- function(x, state) {
    if (length(x) == 0) {
        gp <- gpar()
    } else {
        tokens <- strsplit(x, "=")
        names <- sapply(tokens, "[", 1)
        values <- lapply(tokens, parseSetting)
        names(values) <- names
        gp <- do.call(gpar, handleOpacity(values))
    }
    parent <- list(children=NULL, gp=gp)
    class(parent) <- "XDVIRtikzParentObj"
    TeXset("tikzParent", c(list(parent), TeXget("tikzParent", state)), state)
    pushTextColour(gp, state)
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

tikzStretch <- function(transform, transformDecomp) {
    length(transform) &&
        !is.null(transformDecomp) &&
        (any(transformDecomp$sc != 1) ||
         any(transformDecomp$sk != 0))
}

stretchPaths <- function(px, py, cl, lx, by, transformDecomp,
                         closedClass, openClass, state) {
    x <- unlist(px)
    y <- unlist(py)
    if (length(unlist(px)) > 1) {
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

strokeStretchPaths <- function(px, py, cl, lx, by, transformDecomp, state) {
    stretchPaths(px, py, cl, lx, by, transformDecomp,
                 "XDVIRtikzStretchPathObj", "XDVIRtikzStretchPolylineObj",
                 state)
}

fillStretchPaths <- function(px, py, cl, lx, by, transformDecomp, state) {
    stretchPaths(px, py, cl, lx, by, transformDecomp,
                 "XDVIRtikzStretchFillObj", NA, state)
}

drawPaths <- function(px, py, cl, lx, by, transform,
                      closedClass, openClass, state) {
    x <- unlist(px)
    y <- unlist(py)
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
    if (length(unlist(px)) > 1) {
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
    lx <- convertX(unit(left, "mm"), "pt", valueOnly=TRUE)
    by <- convertY(unit(bottom, "mm"), "pt", valueOnly=TRUE)
    transform <- TeXget("tikzTransform", state)
    transformDecomp <- TeXget("tikzTransformDecomp", state)
    ## Handle scale and skew separately because only some graphics
    ## devices can support (the R graphics implementation of) scale and skew
    ## (this allows most graphics devices to handle simpler transformations)
    if (tikzStretch(transform, transformDecomp)) { 
        mapply(drawStretch, pathX, pathY, closed,
               MoreArgs=list(lx, by, transformDecomp, state=state))
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
    x <- convertX(unit(left, "mm"), "pt", valueOnly=TRUE)
    ## Negate y because TikZ is "up" while TeX is "down"
    y <- convertY(unit(-bottom, "mm"), "pt", valueOnly=TRUE)
    tmText <- rbind(c(1,0,0), c(0,-1,0), c(0,0,1)) %*%
        rbind(c(1,0,x), c(0,1,y), c(0,0,1)) %*%
        tm %*%
        rbind(c(1,0,-x), c(0,1,-y), c(0,0,1))
    xy <-  tmText %*% c(x, y, 1) 
    TeXset("h", toTeX(convertX(unit(xy[1], "pt"), "mm"), state), state)
    TeXset("v", toTeX(convertY(unit(xy[2], "pt"), "mm"), state), state)
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
    td <- TeXget("tikzTransformDepth", state)
    TeXset("tikzTransformDepth", c(0, td), state)
    addParent(x, state)
}

recordEndScope <- function(state) {
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
    updateBBoxHoriz(toTeX(unit(left, "mm") + unit(bbox[1], "pt"), state),
                    state)
    updateBBoxVert(toTeX(unit(bottom, "mm") - unit(bbox[2], "pt"), state),
                   state)
    updateBBoxHoriz(toTeX(unit(left, "mm") + unit(bbox[3], "pt"), state),
                    state)
    updateBBoxVert(toTeX(unit(bottom, "mm") - unit(bbox[4], "pt"), state),
                   state)
}

tikzSpecial <- function(specialString, state) {
    ## Ignore any other specials
    if (grepl("^xdvir:: ", specialString)) {
        special <- gsub("xdvir:: ", "", specialString)
        if (grepl("^begin-picture", special)) {
            h <- TeXget("h", state)
            v <- TeXget("v", state)
            TeXset("savedH", h, state)
            TeXset("savedV", v, state)
            x <- fromTeX(h, state)
            y <- fromTeX(v, state)
            TeXset("pictureLeft", x, state)
            TeXset("pictureBottom", y, state)
            TeXset("inPicture", TRUE, state)
            TeXset("tikzParent", NULL, state)
            TeXset("tikzTransform", NULL, state)
            TeXset("tikzTransformDepth", 0, state)
            TeXset("tikzTransformDecomp", NULL, state)
            TeXset("tikzTransformText", diag(3), state)
            TeXset("tikzTextColour", NA, state)
        } else if (grepl("^end-picture", special)) {
            recordBBox(special, state)
            TeXset("h", TeXget("savedH", state), state)
            TeXset("v", TeXget("savedV", state), state)        
            TeXset("inPicture", FALSE, state)
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
    c(paste0("\\def\\pgfsysdriver{",
             system.file("tikz", "pgfsys-xdvir.def",
                         package="xdvir"),
             "}"),
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


