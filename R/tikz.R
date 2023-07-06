
################################################################################
## Code to support specials:

## ops
## Called from setChar()
tikzTransform <- function() {
    transform <- get("tikzTransform")
    transformDecomp <- get("tikzTransformDecomp")
    length(transform) &&
        !(is.null(transformDecomp) ||
          (transformDecomp$rot == 0 &&
           all(transformDecomp$sc == 1) &&
           all(transformDecomp$sk == 0)))
}

setTransformedChar <- function(raw, put=FALSE) {
    transform <- get("tikzTransformDecomp")
    h <- get("h")
    v <- get("v")
    ## Current font
    fonts <- get("fonts")
    f <- get("f")
    font <- fonts[[f]]
    engine <- get("engine")
    colour <- get("colour")
    fontLib <- get("fontLib")
    ## Different engines specify glyphs in different ways
    glyphInfo <- engine$getGlyph(raw, font, dir, fontLib)
    bbox <- fontLib$glyphBounds(glyphInfo$index, font$fontdef$file,
                                font$size, dir)
    width <- fontLib$glyphWidth(glyphInfo$index, font$fontdef$file,
                                font$size)
    x <- fromTeX(h)
    y <- fromTeX(v)
    glyph <- glyph(x, y, glyphInfo$char, glyphInfo$index, f, font$size,
                   colour=colour[1],
                   transform$rot, transform$sc[1], transform$sc[2],
                   transform$sk[1], transform$sk[2])
    ## Bounding box requires transformation
    tm <- get("tikzTransformText")
    bboxLeft <- convertX(unit(fromTeX(h + bbox[1]), "mm"), "pt",
                         valueOnly=TRUE)
    bboxRight <- convertX(unit(fromTeX(h + bbox[2]), "mm"), "pt",
                          valueOnly=TRUE)
    bboxBottom <- convertY(unit(fromTeX(v - bbox[3]), "mm"), "pt",
                           valueOnly=TRUE)
    bboxTop <- convertY(unit(fromTeX(v - bbox[4]), "mm"), "pt",
                        valueOnly=TRUE)
    bboxCorners <- cbind(c(bboxLeft, bboxBottom, 1),
                         c(bboxRight, bboxBottom, 1),
                         c(bboxLeft, bboxTop, 1),
                         c(bboxRight, bboxTop, 1))
    transCorners <- tm %*% bboxCorners
    bboxX <- xtoTeX(unit(transCorners[1,], "pt"))
    bboxY <- ytoTeX(-unit(transCorners[2,], "pt"))
    lapply(bboxX, updateBBoxHoriz)
    lapply(bboxY, updateBBoxVert)
    if (!put) {
        HV <- matrix(c(convertX(unit(fromTeX(h + width[1]), "mm"), "pt",
                                valueOnly=TRUE),
                       convertY(unit(-y, "mm"), "pt", valueOnly=TRUE),
                       1))
        transHV <- tm %*% HV
        set("h", xtoTeX(unit(transHV[1], "pt")))
        set("v", ytoTeX(unit(-transHV[2], "pt")))
    }
    addGlyph(glyph)
}

## Build grobs from objects
buildTikZobj <- function(obj, xoffset, yoffset, grobFn, gp) {
    x <- convertX(obj$x, "bigpts", valueOnly=TRUE) +
        convertX(unit(xoffset, "in"), "bigpts", valueOnly=TRUE)
    y <- convertY(obj$y, "bigpts", valueOnly=TRUE) +
        convertY(unit(yoffset, "in"), "bigpts", valueOnly=TRUE)
    grobFn(x, y, default.units="bigpts", gp=gp)
}

buildGrob.XDVIRtikzPathObj <- function(obj, xoffset, yoffset, ...) {
    buildTikZobj(obj, xoffset, yoffset, pathGrob, gpar(fill=NA))
}

buildGrob.XDVIRtikzPolylineObj <- function(obj, xoffset, yoffset, ...) {
    buildTikZobj(obj, xoffset, yoffset, polylineGrob, gpar())
}

buildGrob.XDVIRtikzFillObj <- function(obj, xoffset, yoffset, ...) {
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
    defgrob <- defineGrob(polylineGrob(obj$x, obj$y,
                                       default.units="bigpts"),
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

buildGrob.XDVIRtikzStretchPathObj <- function(obj, xoffset, yoffset, ...) {
    buildTikZstretchObj(obj, xoffset, yoffset, pathGrob, gpar(fill=NA))
}

buildGrob.XDVIRtikzStretchPolylineObj <- function(obj, xoffset, yoffset, ...) {
    buildTikZstretchObj(obj, xoffset, yoffset, polylineGrob, gpar())
}

buildGrob.XDVIRtikzStretchFillObj <- function(obj, xoffset, yoffset, ...) {
    buildTikZstretchObj(obj, xoffset, yoffset, pathGrob, gpar(col=NA))
}

buildGrob.XDVIRtikzParentObj <- function(obj, xoffset, yoffset, ...) {
    children <- obj$children
    parent <- NULL
    if (!is.null(children)) {
        parent <- gTree(gp=obj$gp)
        parent <- setChildren(parent,
                              do.call(gList,
                                      lapply(children, buildGrob,
                                             xoffset, yoffset)))
    }
    parent
}

buildGrob.XDVIRtikzObj <- function(obj, xoffset, yoffset, ...) {
    gTree(children=do.call(gList,
                           lapply(obj$children, buildGrob, xoffset, yoffset)))
}

buildRotatedGlyph <- function(obj, xoffset, yoffset) {
    ## NEGATE vertical values (because +ve vertical is DOWN in DVI)
    x <- unit(obj$x, "mm") + unit(xoffset, "in")
    y <- unit(-obj$y, "mm") + unit(yoffset, "in")
    vp <- viewport(x, y, just=c("left", "bottom"), angle=obj$rotation/pi*180)
    font <- get("fonts")[[obj$fontindex]]
    glyphFont <- glyphFont(font$fontdef$file, font$fontdef$index,
                           font$fontdef$family, font$fontdef$weight,
                           font$fontdef$style)
    info <- glyphInfo(obj$index, 0, 0,
                      1, 
                      obj$size,
                      glyphFontList(glyphFont),
                      1, ## Does not matter because will be left-bottom aligned
                      1, ## Does not matter because will be left-bottom aligned
                      col=obj$colour)
    glyphGrob(info, 0, 0, hjust="left", vjust="bottom", vp=vp)
}

buildGrob.XDVIRrotatedGlyphObj <- function(obj, xoffset, yoffset, ...) {
    children <- lapply(1:nrow(obj),
                       function(i)
                           buildRotatedGlyph(obj[i,], xoffset, yoffset))
    gTree(children=do.call(gList, children))
}

buildTransformedGlyph <- function(obj, xoffset, yoffset) {
    scaleX <- obj$scaleX
    scaleY <- obj$scaleY
    skewX <- obj$skewX
    skewY <- obj$skewY
    angle <- obj$rotation/pi*180
    
    ## NEGATE vertical values (because +ve vertical is DOWN in DVI)
    x <- unit(obj$x, "mm") + unit(xoffset, "in")
    y <- unit(-obj$y, "mm") + unit(yoffset, "in")
    
    font <- get("fonts")[[obj$fontindex]]
    glyphFont <- glyphFont(font$fontdef$file, font$fontdef$index,
                           font$fontdef$family, font$fontdef$weight,
                           font$fontdef$style)
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

buildGrob.XDVIRtransformedGlyphObj <- function(obj, xoffset, yoffset, ...) {
    children <- lapply(1:nrow(obj),
                       function(i)
                           buildTransformedGlyph(obj[i,], xoffset, yoffset))
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

parseMoveTo <- function(x, i) {
    xy <- strsplit(x, ",")[[1]]
    sub <- get("tikzSubPath") + 1
    pathX <- get("tikzPathX")
    pathY <- get("tikzPathY")
    pathX[[sub]][[i]] <- as.numeric(xy[1])
    pathY[[sub]][[i]] <- as.numeric(xy[2])
    set("tikzPathX", pathX)
    set("tikzPathY", pathY)
    set("tikzSubPath", sub)
}

parseLineTo <- function(x, i) {
    xy <- strsplit(x, ",")[[1]]
    sub <- get("tikzSubPath")
    pathX <- get("tikzPathX")
    pathY <- get("tikzPathY")
    pathX[[sub]][[i]] <- as.numeric(xy[1])
    pathY[[sub]][[i]] <- as.numeric(xy[2])
    set("tikzPathX", pathX)
    set("tikzPathY", pathY)
}

parseCurveTo <- function(x, i) {
    xy <- strsplit(x, ",")[[1]]
    sub <- get("tikzSubPath")
    pathX <- get("tikzPathX")
    startX <- pathX[[sub]][[i - 1]][length(pathX[[sub]][[i - 1]])]
    pathY <- get("tikzPathY")
    startY <- pathY[[sub]][[i - 1]][length(pathY[[sub]][[i - 1]])]    
    ## Convert Bezier to polyline
    bg <- gridBezier::BezierGrob(x=unit(c(startX, xy[c(1, 3, 5)]), units="pt"),
                                 y=unit(c(startY, xy[c(2, 4, 6)]), units="pt"))
    pts <- gridBezier::BezierPoints(bg)
    pathX[[sub]][[i]] <- convertX(unit(pts$x[-1], "in"), "pt", valueOnly=TRUE)
    pathY[[sub]][[i]] <- convertY(unit(pts$y[-1], "in"), "pt", valueOnly=TRUE)
    set("tikzPathX", pathX)
    set("tikzPathY", pathY)
}

parseClose <- function(i) {
    ## Start new subPath
    sub <- get("tikzSubPath") + 1
    ## Mark old subPath as closed
    closed <- get("tikzPathClosed")
    closed[sub - 1] <- TRUE
    set("tikzPathClosed", closed)
    ## New path begins at start point of previous subPath
    ## (this may immediately get superceded by moveto, BUT OTOH it may NOT)
    pathX <- get("tikzPathX")
    pathY <- get("tikzPathY")
    pathX[[sub]][[i]] <- pathX[[sub - 1]][[1]]
    pathY[[sub]][[i]] <- pathY[[sub - 1]][[1]]
    set("tikzPathX", pathX)
    set("tikzPathY", pathY)
    set("tikzSubPath", sub)
}

recordPathElement <- function(x, i) {
    tokens <- strsplit(x, " ")[[1]]
    if (i == 1 && tokens[1] != "moveto") {
        stop("Invalid path (must begin with moveto)")
    }
    switch(tokens[1],
           moveto=parseMoveTo(tokens[-1], i),
           lineto=parseLineTo(tokens[-1], i),
           curveto=parseCurveTo(tokens[-1], i),
           close=parseClose(i),
           stop("unsupported path element"))
}

pushTextColour <- function(gp) {
    fill <- NA
    if ("fill" %in% names(gp)) {
        fill <- gp$fill
    }
    set("tikzTextColour", c(gp$fill, get("tikzTextColour")))
    set("colour", fill)
}

popTextColour <- function() {
    fill <- get("tikzTextColour")
    set("tikzTextColour", fill[-1])
    set("colour", fill[1])
}

addParent <- function(x) {
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
    set("tikzParent", c(list(parent), get("tikzParent")))
    pushTextColour(gp)
}

reduceParent <- function() {
    parent <- get("tikzParent")
    if (length(parent) == 1) {
        addTikzObj(parent[[1]])
    } else {
        parent[[2]]$children <- c(parent[[2]]$children, parent[1])
    }
    set("tikzParent", parent[-1])
    popTextColour()
}

addChild <- function(x) {
    parent <- get("tikzParent")
    parent[[1]]$children <- c(parent[[1]]$children, list(x))
    set("tikzParent", parent)
}

recordNewPath <- function(x) {
    set("tikzPathX", NULL)
    set("tikzPathY", NULL)
    addParent(x)
}

tikzStretch <- function(transform, transformDecomp) {
    length(transform) &&
        !is.null(transformDecomp) &&
        (any(transformDecomp$sc != 1) ||
         any(transformDecomp$sk != 0))
}

stretchPaths <- function(px, py, cl, lx, by, transformDecomp) {
}

strokeStretchPaths <- function(px, py, cl, lx, by, transformDecomp) {
    x <- unlist(px)
    y <- unlist(py)
    if (length(unlist(px)) > 1) {
        if (cl) {
            child <- list(lx=lx,
                          by=-by,
                          x=unit(x, "pt"),
                          y=unit(y, "pt"),
                          transform=transformDecomp)
            class(child) <- "XDVIRtikzStretchPathObj"
        } else {
            child <- list(lx=lx,
                          by=-by,
                          x=unit(x, "pt"),
                          y=unit(y, "pt"),
                          transform=transformDecomp)
            class(child) <- "XDVIRtikzStretchPolylineObj"
        }
        addChild(child)
    }
}

drawPaths <- function(px, py, cl, lx, by, transform,
                      closedClass, openClass) {
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
        addChild(child)
    }
}

strokePaths <- function(px, py, cl, lx, by, transform) {
    drawPaths(px, py, cl, lx, by, transform,
              "XDVIRtikzPathObj", "XDVIRtikzPolylineObj")
}

fillPaths <- function(px, py, cl, lx, by, transform) {
    drawPaths(px, py, cl, lx, by, transform,
              "XDVIRtikzFillObj", NA)
}

recordDraw <- function(draw, drawStretch) {
    pathX <- get("tikzPathX")
    pathY <- get("tikzPathY")
    closed <- get("tikzPathClosed")
    left <- get("pictureLeft")
    bottom <- get("pictureBottom")
    lx <- convertX(unit(left, "mm"), "pt", valueOnly=TRUE)
    by <- convertY(unit(bottom, "mm"), "pt", valueOnly=TRUE)
    transform <- get("tikzTransform")
    transformDecomp <- get("tikzTransformDecomp")
    ## Handle scale and skew separately because only some graphics
    ## devices can support (the R graphics implementation of) scale and skew
    ## (this allows most graphics devices to handle simpler transformations)
    if (tikzStretch(transform, transformDecomp)) { 
        mapply(drawStretch, pathX, pathY, closed,
               MoreArgs=list(lx, by, transformDecomp))
    } else {
        mapply(draw, pathX, pathY, closed,
               MoreArgs=list(lx, by, transform))
    }
}

recordStroke <- function() {
    recordDraw(strokePaths, strokeStretchPaths)
    reduceParent()
}

recordFill <- function() {
    recordDraw(fillPaths, fillStretchPaths)
    reduceParent()
}

recordFillStroke <- function() {
    recordDraw(fillPaths, fillStretchPaths)
    recordDraw(strokePaths, strokeStretchPaths)
    reduceParent()
}

recordTransform <- function(x) {
    tokens <- as.numeric(strsplit(x, ",")[[1]])
    tm <- rbind(c(tokens[1], tokens[3], tokens[5]),
                c(tokens[2], tokens[4], tokens[6]),
                c(0, 0, 1))
    transform <- get("tikzTransform")
    if (length(transform) == 0) {
        set("tikzTransform", list(tm))
    } else {
        tm <- transform[[1]] %*% tm
        set("tikzTransform", c(list(tm), transform))
    }
    td <- get("tikzTransformDepth")
    td[1] <- td[1] + 1
    set("tikzTransformDepth", td)
    ## TEMPORARILY set h/v (within TikZ picture)
    ## Transform is relative to picture bottom-left
    left <- get("pictureLeft")
    bottom <- get("pictureBottom")
    ## Move to location of text
    x <- convertX(unit(left, "mm"), "pt", valueOnly=TRUE)
    ## Negate y because TikZ is "up" while TeX is "down"
    y <- convertY(unit(-bottom, "mm"), "pt", valueOnly=TRUE)
    tmText <- rbind(c(1,0,0), c(0,-1,0), c(0,0,1)) %*%
        rbind(c(1,0,x), c(0,1,y), c(0,0,1)) %*%
        tm %*%
        rbind(c(1,0,-x), c(0,1,-y), c(0,0,1))
    xy <-  tmText %*% c(x, y, 1) 
    set("h", xtoTeX(convertX(unit(xy[1], "pt"), "mm")))
    set("v", ytoTeX(convertY(unit(xy[2], "pt"), "mm")))
    set("tikzTransformText", tmText)
    set("tikzTransformDecomp", decompose(tm))
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

addTikzObj <- function(x) {
    tikzObj <- x
    class(tikzObj) <- "XDVIRtikzObj"
    addDVIobj(tikzObj)
}

recordBeginScope <- function(x) {
    td <- get("tikzTransformDepth")
    set("tikzTransformDepth", c(0, td))
    addParent(x)
}

recordEndScope <- function(x) {
    td <- get("tikzTransformDepth")
    if (td[1] > 0) {
        tm <- get("tikzTransform")
        set("tikzTransform", tm[-(1:td[1])])
    }
    set("tikzTransformDepth", td[-1])
    reduceParent()
}

## Generate object from TikZ special
recordSpecial <- function(x) {
    ## Ignore "blanks"
    if (grepl("^ *$", x)) return()
    ## Split by ": " (for paths)
    tokens <- strsplit(gsub("^ *| *$", "", x), ":")[[1]]
    if (length(tokens) == 0) {
        warning("Empty special")
    } else if (length(tokens) == 1) {
        tokens <- strsplit(gsub(" *$", "", tokens), " ")[[1]]
        switch(tokens[1],
               `begin-scope`=recordBeginScope(tokens[-1]),
               `end-scope`=recordEndScope(),
               `new-path`=recordNewPath(tokens[-1]),
               `stroke`=recordStroke(),
               `fill`=recordFill(),
               `fill-stroke`=recordFillStroke(), 
               `transform`=recordTransform(tokens[-1]),
               stop("Unsupported TikZ special"))
    } else {
        ## Path
        n <- length(tokens)
        ## Count number of moveto's and close's
        nsub <- length(grep("moveto|close", tokens))
        ## Create subpath for each moveto and close
        set("tikzSubPath", 0)
        ## (record path element i in component i of relevant subpath)
        set("tikzPathX", lapply(1:nsub, function(i) vector("list", n)))
        set("tikzPathY", lapply(1:nsub, function(i) vector("list", n)))
        ## Is each subpath closed ? (FALSE by default)
        set("tikzPathClosed", logical(nsub))
        mapply(recordPathElement, tokens, 1:n)
        invisible()
    }
}

recordBBox <- function(x) {
    tokens <- strsplit(gsub(" *$", "", x), " ")[[1]]
    bbox <- as.numeric(strsplit(gsub("pt|;", "", tokens[-1]), ",")[[1]])
    left <- get("pictureLeft")
    bottom <- get("pictureBottom")
    ## Update DVI bbox for TikZ bbox
    updateBBoxHoriz(xtoTeX(unit(left, "mm") + unit(bbox[1], "pt")))
    updateBBoxVert(ytoTeX(unit(bottom, "mm") - unit(bbox[2], "pt")))
    updateBBoxHoriz(xtoTeX(unit(left, "mm") + unit(bbox[3], "pt")))
    updateBBoxVert(ytoTeX(unit(bottom, "mm") - unit(bbox[4], "pt")))
}

tikzSpecial <- function(specialString) {
    ## Ignore any other specials
    if (grepl("^xdvir:: ", specialString)) {
        special <- gsub("xdvir:: ", "", specialString)
        if (grepl("^begin-picture", special)) {
            h <- get("h")
            v <- get("v")
            set("savedH", h)
            set("savedV", v)
            x <- fromTeX(h)
            y <- fromTeX(v)
            set("pictureLeft", x)
            set("pictureBottom", y)
            set("inPicture", TRUE)
            set("tikzParent", NULL)
            set("tikzTransform", NULL)
            set("tikzTransformDepth", 0)
            set("tikzTransformDecomp", NULL)
            set("tikzTransformText", diag(3))
            set("tikzTextColour", NA)
        } else if (grepl("^end-picture", special)) {
            recordBBox(special)
            set("h", get("savedH"))
            set("v", get("savedV"))        
            set("inPicture", FALSE)
        } else {
            if (get("inPicture")) {
                ## Record special
                ## Output may be multiple specials from
                ## "protocolled" (recorded) output, so split first by ";"
                specials <- strsplit(special, ";")[[1]]
                lapply(specials, recordSpecial)
            }
        }
    }
}

################################################################################
## The basic preamble, etc
## NOTE that the preamble loads the xdvir-specific pgfsysdriver

tikzInit <- function() {
    set("inPicture", FALSE)
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

tikzPackage <- function(packages=NULL) {
    package(preamble=tikzPreamble(packages),
            special=tikzSpecial,
            init=tikzInit)
}

tikzPicture <- function(packages=NULL, bbox=NULL) {
    package(preamble=tikzPreamble(packages),
            prefix=tikzPrefix,
            suffix=tikzSuffix(bbox),
            special=tikzSpecial,
            init=tikzInit)
}

registerPackage(tikzPackage(), "tikz")
registerPackage(tikzPicture(), "tikzPicture")

