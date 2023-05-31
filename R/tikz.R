
################################################################################
## Code to support specials:
## Create objects
## Build grobs from objects

buildGrob.XDVIRtikzPathObj <- function(obj, xoffset, yoffset, ...) {
    ## NEGATE vertical values (because +ve vertical is DOWN in DVI)
    x <- convertX(unit(obj$x, "pt"), "bigpts", valueOnly=TRUE) +
        convertX(unit(xoffset, "in"), "bigpts", valueOnly=TRUE)
    y <- convertY(unit(-obj$y, "pt"), "bigpts", valueOnly=TRUE) +
        convertY(unit(yoffset, "in"), "bigpts", valueOnly=TRUE)
    pathGrob(x, y, default.units="bigpts", gp=gpar(fill=NA))
}

buildGrob.XDVIRtikzPolylineObj <- function(obj, xoffset, yoffset, ...) {
    ## NEGATE vertical values (because +ve vertical is DOWN in DVI)
    x <- convertX(unit(obj$x, "pt"), "bigpts", valueOnly=TRUE) +
        convertX(unit(xoffset, "in"), "bigpts", valueOnly=TRUE)
    y <- convertY(unit(-obj$y, "pt"), "bigpts", valueOnly=TRUE) +
        convertY(unit(yoffset, "in"), "bigpts", valueOnly=TRUE)
    polylineGrob(x, y, default.units="bigpts")
}

buildGrob.XDVIRtikzFillObj <- function(obj, xoffset, yoffset, ...) {
    ## NEGATE vertical values (because +ve vertical is DOWN in DVI)
    x <- convertX(unit(obj$x, "pt"), "bigpts", valueOnly=TRUE) +
        convertX(unit(xoffset, "in"), "bigpts", valueOnly=TRUE)
    y <- convertY(unit(-obj$y, "pt"), "bigpts", valueOnly=TRUE) +
        convertY(unit(yoffset, "in"), "bigpts", valueOnly=TRUE)
    pathGrob(x, y, default.units="bigpts", gp=gpar(col=NA))
}

buildGrob.XDVIRtikzParentObj <- function(obj, xoffset, yoffset, ...) {
    children <- obj$children
    parent <- gTree(gp=obj$gp)
    if (!is.null(children))
        parent <- setChildren(parent,
                              do.call(gList,
                                      lapply(children, buildGrob,
                                             xoffset, yoffset)))
    parent
}

buildGrob.XDVIRtikzObj <- function(obj, xoffset, yoffset, ...) {
    gTree(children=do.call(gList,
                           lapply(obj$children, buildGrob, xoffset, yoffset)))
}

## Based on
## https://math.stackexchange.com/questions/13150/extracting-rotation-scale-values-from-2d-transformation-matrix/13165#13165
decompose <- function(m) {
    a <- m[1]
    b <- m[2]
    c <- m[3]
    d <- m[4]
    e <- m[5]
    f <- m[6]
    
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
    list(tr=translation, rot=rotation, sc=scale, sk=skew)
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
}

reduceParent <- function() {
    parent <- get("tikzParent")
    if (length(parent) == 1) {
        addTikzObj(parent[[1]])
    } else {
        parent[[2]]$children <- c(parent[[2]]$children, parent[1])
    }
    set("tikzParent", parent[-1])
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

recordStroke <- function() {
    pathX <- get("tikzPathX")
    pathY <- get("tikzPathY")
    closed <- get("tikzPathClosed")
    left <- get("pictureLeft")
    bottom <- get("pictureBottom")
    mapply(function(px, py, cl) {
               if (length(unlist(px)) > 1) {
                   if (cl) {
                       child <- list(x=unit(left, "mm") +
                                         unit(unlist(px), "pt"),
                                     y=unit(bottom, "mm") -
                                         unit(unlist(py), "pt"))
                       class(child) <- "XDVIRtikzPathObj"
                   } else {
                       child <- list(x=unit(left, "mm") +
                                         unit(unlist(px), "pt"),
                                     y=unit(bottom, "mm") -
                                         unit(unlist(py), "pt"))
                       class(child) <- "XDVIRtikzPolylineObj"
                   }
                   addChild(child)
               }
           },
           pathX, pathY, closed)
    reduceParent()
}

recordFill <- function() {
    pathX <- get("tikzPathX")
    pathY <- get("tikzPathY")
    closed <- get("tikzPathClosed")
    left <- get("pictureLeft")
    bottom <- get("pictureBottom")
    mapply(function(px, py) {
               if (length(unlist(px)) > 1) {
                   child <- list(x=unit(left, "mm") + unit(unlist(px), "pt"),
                                 y=unit(bottom, "mm") - unit(unlist(py), "pt"))
                   class(child) <- "XDVIRtikzFillObj"
                   addChild(child)
               }
           },
           pathX, pathY)
    reduceParent()
}

recordFillStroke <- function() {
    pathX <- get("tikzPathX")
    pathY <- get("tikzPathY")
    closed <- get("tikzPathClosed")
    left <- get("pictureLeft")
    bottom <- get("pictureBottom")
    mapply(function(px, py) {
               if (length(unlist(px)) > 1) {
                   child <- list(x=unit(left, "mm") + unit(unlist(px), "pt"),
                                 y=unit(bottom, "mm") - unit(unlist(py), "pt"))
                   class(child) <- "XDVIRtikzFillObj"
                   addChild(child)
               }
           },
           pathX, pathY)
    mapply(function(px, py, cl) {
               if (length(unlist(px)) > 1) {
                   if (cl) {
                       child <- list(x=unit(left, "mm") +
                                         unit(unlist(px), "pt"),
                                     y=unit(bottom, "mm") -
                                         unit(unlist(py), "pt"))
                       class(child) <- "XDVIRtikzPathObj"
                   } else {
                       child <- list(x=unit(left, "mm") +
                                         unit(unlist(px), "pt"),
                                     y=unit(bottom, "mm") -
                                         unit(unlist(py), "pt"))
                       class(child) <- "XDVIRtikzPolylineObj"
                   }
                   addChild(child)
               }
           },
           pathX, pathY, closed)
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
        tm <- tm %*% transform[[1]]
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
    xy <- rbind(c(1,0,0), c(0,-1,0), c(0,0,1)) %*% tm %*% c(x, y, 1) 
    set("h", xtoTeX(convertX(unit(xy[1], "pt"), "mm")))
    set("v", ytoTeX(convertY(unit(xy[2], "pt"), "mm")))
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
        mt <- get("tikzTransform")
        set("tikzTransform", mt[-(1:td[1])])
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
## VERY complex specials

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

tikzSuffix <- "\\end{tikzpicture}"

tikz <- package(preamble=tikzPreamble(),
                special=tikzSpecial,
                init=tikzInit)

tikzPicture <- package(preamble=tikzPreamble(),
                       prefix=tikzPrefix,
                       suffix=tikzSuffix,
                       special=tikzSpecial,
                       init=tikzInit)

registerPackage(tikz, "tikz")
registerPackage(tikzPicture, "tikzPicture")

