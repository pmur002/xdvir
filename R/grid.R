
dviGlyphInfo <- function(x, engine) {
    set("engine", engine)
    invisible(lapply(x, grobDVI))
    glyphs <- do.call(rbind, get("glyphs"))
    metrics <- list(left=fromTeX(get("left")),
                    right=fromTeX(get("right")),
                    top=fromTeX(get("top")),
                    bottom=fromTeX(get("bottom")),
                    baseline=fromTeX(get("baseline")))
    width <- abs(metrics$right - metrics$left)
    height <- abs(metrics$top - metrics$bottom)
    right <- convertX(unit(width, "mm"), "bigpts", valueOnly=TRUE)
    top <- convertHeight(unit(height, "mm"), "bigpts", valueOnly=TRUE)
    if (is.finite(metrics$baseline)) {
        vAnchor <- glyphAnchor(c(0, top, top/2,
                                 convertHeight(unit(metrics$baseline, "mm"),
                                               "bigpts", valueOnly=TRUE)),
                               label=c("bottom", "top", "centre", "baseline"))
    } else {
        vAnchor <- glyphAnchor(c(0, top, top/2),
                               label=c("bottom", "top", "centre"))
    }
    glyphInfo(glyphs$index,
              convertX(unit(glyphs$x, "mm"), "bigpts", valueOnly=TRUE),
              convertY(unit(height - glyphs$y, "mm"), "bigpts", valueOnly=TRUE),
              glyphs$family,
              glyphs$weight,
              glyphs$style,
              glyphs$size,
              glyphs$filename,
              glyphs$fontindex,
              glyphWidth(convertWidth(unit(width, "mm"),
                                      "bigpts", valueOnly=TRUE)),
              glyphHeight(convertHeight(unit(height, "mm"),
                                        "bigpts", valueOnly=TRUE)),
              hAnchor=glyphAnchor(c(0, right, right/2),
                                  label=c("left", "right", "centre")),
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
