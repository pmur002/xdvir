
dviGlyphInfo <- function(x, engine) {
    set("engine", engine)
    invisible(lapply(x, grobDVI))
    glyphs <- do.call(rbind, get("glyphs"))
    metrics <- list(left=fromTeX(get("left")),
                    right=fromTeX(get("right")),
                    top=fromTeX(get("top")),
                    bottom=fromTeX(get("bottom")),
                    baseline=fromTeX(get("baseline")))
    ## NEGATE vertical values (because +ve vertical is DOWN in DVI)
    gx <- convertX(unit(glyphs$x, "mm"), "bigpts", valueOnly=TRUE)
    gy <- convertY(-unit(glyphs$y, "mm"), "bigpts", valueOnly=TRUE)
    textleft <- min(gx)
    textright <- convertX(unit(fromTeX(get("textright")), "mm"), "bigpts",
                          valueOnly=TRUE)
    left <- convertX(unit(fromTeX(get("left")), "mm"), "bigpts",
                     valueOnly=TRUE)
    right <- convertX(unit(fromTeX(get("right")), "mm"), "bigpts",
                      valueOnly=TRUE)
    bottom <- convertY(unit(-fromTeX(get("bottom")), "mm"), "bigpts",
                       valueOnly=TRUE)
    top <- convertY(unit(-fromTeX(get("top")), "mm"), "bigpts",
                    valueOnly=TRUE)
    if (is.finite(get("baseline"))) {
        vAnchor <- glyphAnchor(c(bottom, top, (bottom + top)/2,
                                 convertY(unit(fromTeX(get("baseline")), "mm"),
                                          "bigpts", valueOnly=TRUE)),
                               label=c("bottom", "top", "centre", "baseline"))
    } else {
        vAnchor <- glyphAnchor(c(bottom, top, (bottom + top)/2),
                               label=c("bottom", "top", "centre"))
    }
    fontMap <- unique(glyphs$fontindex)
    fontList <- lapply(get("fonts")[fontMap],
                       function(x) {
                           def <- x$fontdef
                           glyphFont(def$file, def$index,
                                     def$family, def$weight, def$style)
                       })
    glyphInfo(glyphs$index, gx, gy,
              match(glyphs$fontindex, fontMap), ## font
              glyphs$size,
              do.call(glyphFontList, fontList),
              glyphWidth(right - left),
              glyphHeight(bottom - top),
              hAnchor=glyphAnchor(c(textleft, textright,
                                    (textleft + textright)/2,
                                    left, right, (left + right)/2),
                                  label=c("left", "right",
                                          "centre",
                                          "bbleft", "bbright", "bbcentre")),
              vAnchor=vAnchor,
              glyphs$colour)
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
                        device=names(dev.cur()),
                        name=NULL,
                        gp=gpar(),
                        engine=lualatexEngine, ...) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    glyphs <- dviGlyphInfo(dvi, engine)
    glyphGrob(glyphs, x, y, hjust=hjust, vjust=vjust, name=name, gp=gp)   
}

grid.dvi <- function(...) {
    grid.draw(dviGrob(...))
}
