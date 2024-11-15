
objToGrob <- function(obj, ...) {
    UseMethod("objToGrob")
}

objToGrob.XDVIRglyphObj <- function(obj, x, y, hjust, vjust, ..., state) {
    ## NEGATE vertical values (because +ve vertical is DOWN in DVI)
    gx <- convertX(unit(obj$x, "mm"), "bigpts", valueOnly=TRUE)
    gy <- convertY(-unit(obj$y, "mm"), "bigpts", valueOnly=TRUE)
    textleft <- convertX(unit(fromTeX(TeXget("textleft", state), state), "mm"),
                         "bigpts", valueOnly=TRUE)
    textright <- convertX(unit(fromTeX(TeXget("textright", state), state),
                               "mm"),
                          "bigpts", valueOnly=TRUE)
    left <- convertX(unit(fromTeX(TeXget("left", state), state), "mm"),
                     "bigpts", valueOnly=TRUE)
    right <- convertX(unit(fromTeX(TeXget("right", state), state), "mm"),
                      "bigpts", valueOnly=TRUE)
    bottom <- convertY(unit(-fromTeX(TeXget("bottom", state), state), "mm"),
                       "bigpts", valueOnly=TRUE)
    top <- convertY(unit(-fromTeX(TeXget("top", state), state), "mm"),
                    "bigpts", valueOnly=TRUE)
    if (!is.finite(textleft))
        textleft <- left
    if (!is.finite(textright))
        textright <- right
    vAnchorValues <- c(bottom, top, (bottom + top)/2)
    vAnchorLabels <- c("bottom", "top", "centre")
    if (is.finite(TeXget("baseline", state))) {
        vAnchorValues <- c(vAnchorValues,
                           convertY(unit(-fromTeX(TeXget("baseline", state),
                                                  state),
                                         "mm"),
                                    "bigpts", valueOnly=TRUE))
        vAnchorLabels <- c(vAnchorLabels, "baseline")
    }
    anchors <- TeXget("vAnchors", state)
    if (!is.null(anchors)) {
        vAnchorValues <- c(vAnchorValues, anchors$value)
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
        hAnchorValues <- c(hAnchorValues, anchors$value)
        hAnchorLabels <- c(hAnchorLabels, anchors$label)
    }
    hAnchor <- glyphAnchor(hAnchorValues, hAnchorLabels)
    fontMap <- unique(obj$fontindex)
    fontList <- lapply(TeXget("fonts", state)[fontMap],
                       function(x) {
                           glyphFont(x$file, x$index, "", 0, "")
                       })
    info <- glyphInfo(obj$index, gx, gy,
                      match(obj$fontindex, fontMap), ## font
                      obj$size,
                      do.call(glyphFontList, fontList),
                      glyphWidth(maxX - minX),
                      ## Down is bigger in DVI
                      glyphHeight(top - bottom),
                      hAnchor=hAnchor,
                      vAnchor=vAnchor,
                      obj$colour)
    glyphGrob(info, x, y, hjust=hjust, vjust=vjust)
}

objToGrob.XDVIRruleObj <- function(obj, xoffset, yoffset, ..., state) {
    ## NEGATE vertical values (because +ve vertical is DOWN in DVI)
    x <- convertX(unit(fromTeX(obj$x, state), "mm"), "bigpts", valueOnly=TRUE) +
        convertX(unit(xoffset, "in"), "bigpts", valueOnly=TRUE)
    y <- convertY(-unit(fromTeX(obj$y, state), "mm"),
                  "bigpts", valueOnly=TRUE) +
        convertY(unit(yoffset, "in"), "bigpts", valueOnly=TRUE)
    width <- convertWidth(unit(fromTeX(obj$w, state), "mm"),
                          "bigpts", valueOnly=TRUE)
    height <- convertHeight(unit(fromTeX(obj$h, state), "mm"),
                            "bigpts", valueOnly=TRUE)
    ## Below lwd=1, draw a line
    if (width < .75) { ## 1/96 / (1/72)  [ lwd=1 => 1/96 inch ]
        segmentsGrob(x + width/2,
                     y,
                     x + width/2,
                     y + height,
                     default.units="bigpts",
                     gp=gpar(lwd=72*width/96, lineend="butt"))
    } else if (height < .75) {
        segmentsGrob(x,
                     y + height/2,
                     x + width,
                     y + height/2,
                     default.units="bigpts",
                     gp=gpar(lwd=72*height/96, lineend="butt"))
    } else {
        rectGrob(x, y, width, height, default.units="bigpts",
                 just=c("left", "bottom"),
                 gp=gpar(col=NA, fill="black"))
    }
}
