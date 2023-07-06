
## Record DVI objects on pass through DVI file
## (do not create grobs because need to do that once we have all metric info)

initDVIobjs <- function() {
    set("DVIobjList", list())
}

addDVIobj <- function(x) {
    objs <- get("DVIobjList")
    objs[[length(objs) + 1]] <- x
    set("DVIobjList", objs)
}

addGlyphObj <- function() {
    glyphs <- get("glyphs")
    if (length(glyphs)) {
        glyphObjs <- do.call(rbind, glyphs)
        glyphList <- split(glyphObjs,
                           apply(glyphObjs[c("rotation", "scaleX", "scaleY",
                                             "skewX", "skewY")],
                                 1, paste, collapse=":"))
        lapply(glyphList,
               function(x) {
                   if (x$scaleX[1] == 1 && x$scaleY[1] == 1 &&
                       x$skewX[1] == 0 && x$skewY[1] == 0) {
                       if (x$rotation[1] == 0) {
                           class(x) <- c("XDVIRglyphObj", class(x))
                       } else {
                           class(x) <- c("XDVIRrotatedGlyphObj", class(x))
                       }
                   } else {
                       class(x) <- c("XDVIRtransformedGlyphObj", class(x))
                   }
                   addDVIobj(x)
               })
    }
    ## Empty the "glyphs" list
    set("glyphs", list())
}

addRuleObj <- function(a, b) {
    if (a > 0 && b > 0) {
        x <- get("h")
        y <- get("v")
        width <- b
        height <- a
        ruleObj <- list(x=x, y=y, w=width, h=height)
        class(ruleObj) <- "XDVIRruleObj"
        addDVIobj(ruleObj)
    }
}

