
objToGrob <- function(obj, ...) {
    UseMethod("objToGrob")
}

objToGrob.XDVIRglyphObj <- function(obj, x, y, hjust, vjust, dpi, ..., state) {
    ## NEGATE vertical values (because +ve vertical is DOWN in DVI)
    if (is.na(dpi)) {
        gx <- TeX2pt(obj$x, state)
        gy <- -TeX2pt(obj$y, state)
    } else {
        gx <- TeX2pt(px2TeX(obj$xx, state), state)
        gy <- -TeX2pt(px2TeX(obj$yy, state), state)
    }
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
                      TeX2pt(obj$size, state),
                      do.call(glyphFontList, fontList),
                      glyphWidth(maxX - minX),
                      ## Down is bigger in DVI
                      glyphHeight(top - bottom),
                      hAnchor=hAnchor,
                      vAnchor=vAnchor,
                      obj$colour)
    glyphGrob(info, x, y, hjust=hjust, vjust=vjust)
}

objToGrob.XDVIRruleObj <- function(obj, xoffset, yoffset, dpi, ..., state) {
    ## NEGATE vertical values (because +ve vertical is DOWN in DVI)
    if (is.na(dpi)) {
        x <- TeX2pt(obj$x, state) + xoffset
        y <- -TeX2pt(obj$y, state) + yoffset
        width <- TeX2pt(obj$w, state)
        height <- TeX2pt(obj$h, state)
    } else {
        x <- TeX2pt(px2TeX(obj$xx, state), state) + xoffset
        y <- -TeX2pt(px2TeX(obj$yy, state), state) + yoffset
        width <- TeX2pt(px2TeX(obj$ww, state), state)
        height <- TeX2pt(px2TeX(obj$hh, state), state)
    }
    subrule <- getOption("xdvir.substituteRule")
    if ((width < .75 || height < .75) &&
        (is.null(subrule) || as.logical(subrule))) {
        ## Below lwd=1, draw a line
        if (width < .75) { ## 1/96 / (1/72)  [ lwd=1 => 1/96 inch ]
            segmentsGrob(x + width/2,
                         y,
                         x + width/2,
                         y + height,
                         default.units="bigpts",
                         gp=gpar(lwd=96*width/72, lineend="butt"))
        } else if (height < .75) {
            segmentsGrob(x,
                         y + height/2,
                         x + width,
                         y + height/2,
                         default.units="bigpts",
                         gp=gpar(lwd=96*height/72, lineend="butt"))
        }
    } else {
        rectGrob(x, y, width, height, default.units="bigpts",
                 just=c("left", "bottom"),
                 gp=gpar(col=NA, fill="black"))
    }
}
