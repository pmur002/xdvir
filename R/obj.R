
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
        glyphObj <- do.call(rbind, glyphs)
        class(glyphObj) <- "XDVIRglyphObj"
        addDVIobj(glyphObj)
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

