
objToGrob <- function(obj, ...) {
    UseMethod("objToGrob")
}

objToGrob.XDVIRglyphObj <- function(obj, hjust, vjust,
                                    width, height, hAnchor, vAnchor,
                                    dpi, ..., state) {
    ## NEGATE vertical values (because +ve vertical is DOWN in DVI)
    if (is.na(dpi)) {
        gx <- TeX2pt(obj$x, state)
        gy <- -TeX2pt(obj$y, state)
    } else {
        gx <- TeX2pt(px2TeX(obj$xx, state), state)
        gy <- -TeX2pt(px2TeX(obj$yy, state), state)
    }
    fontMap <- unique(obj$fontindex)
    fontList <- lapply(TeXget("fonts", state)[fontMap],
                       function(x) {
                           glyphFont(x$file, x$index, "", 0, "")
                       })
    info <- glyphInfo(obj$index, gx, gy,
                      match(obj$fontindex, fontMap), ## font
                      TeX2pt(obj$size, state),
                      do.call(glyphFontList, fontList),
                      width,
                      ## Down is bigger in DVI
                      height,
                      hAnchor=hAnchor,
                      vAnchor=vAnchor,
                      obj$colour)
    glyphGrob(info, .5, .5, hjust=hjust, vjust=vjust)
}

objToGrob.XDVIRruleObj <- function(obj, dx, dy, dpi, ..., state) {
    ## NEGATE vertical values (because +ve vertical is DOWN in DVI)
    if (is.na(dpi)) {
        x <- TeX2pt(obj$x, state) + dx
        y <- -TeX2pt(obj$y, state) + dy
        width <- TeX2pt(obj$w, state)
        height <- TeX2pt(obj$h, state)
    } else {
        x <- TeX2pt(px2TeX(obj$xx, state), state) + dx
        y <- -TeX2pt(px2TeX(obj$yy, state), state) + dy
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
