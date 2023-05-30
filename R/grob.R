
buildGrob <- function(obj, ...) {
    UseMethod("buildGrob")
}

buildGrob.XDVIRglyphObj <- function(obj, x, y, hjust, vjust, ...) {
    ## NEGATE vertical values (because +ve vertical is DOWN in DVI)
    gx <- convertX(unit(obj$x, "mm"), "bigpts", valueOnly=TRUE)
    gy <- convertY(-unit(obj$y, "mm"), "bigpts", valueOnly=TRUE)
    textleft <- convertX(unit(fromTeX(get("textleft")), "mm"), "bigpts",
                         valueOnly=TRUE)
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
    if (!is.finite(textleft))
        textleft <- left
    if (!is.finite(textright))
        textright <- right
    vAnchorValues <- c(bottom, top, (bottom + top)/2)
    vAnchorLabels <- c("bottom", "top", "centre")
    if (is.finite(get("baseline"))) {
        vAnchorValues <- c(vAnchorValues,
                           convertY(unit(-fromTeX(get("baseline")),
                                         "mm"),
                                    "bigpts", valueOnly=TRUE))
        vAnchorLabels <- c(vAnchorLabels, "baseline")
    }
    anchors <- get("vAnchors")
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
    anchors <- get("hAnchors")
    if (!is.null(anchors)) {
        hAnchorValues <- c(hAnchorValues, anchors$value)
        hAnchorLabels <- c(hAnchorLabels, anchors$label)
    }
    hAnchor <- glyphAnchor(hAnchorValues, hAnchorLabels)
    fontMap <- unique(obj$fontindex)
    fontList <- lapply(get("fonts")[fontMap],
                       function(x) {
                           def <- x$fontdef
                           glyphFont(def$file, def$index,
                                     def$family, def$weight, def$style)
                       })
    info <- glyphInfo(obj$index, gx, gy,
                      match(obj$fontindex, fontMap), ## font
                      obj$size,
                      do.call(glyphFontList, fontList),
                      glyphWidth(maxX - minX),
                      glyphHeight(bottom - top),
                      hAnchor=hAnchor,
                      vAnchor=vAnchor,
                      obj$colour)
    glyphGrob(info, x, y, hjust=hjust, vjust=vjust)
}

buildGrob.XDVIRruleObj <- function(obj, xoffset, yoffset, ...) {
    ## NEGATE vertical values (because +ve vertical is DOWN in DVI)
    x <- convertX(unit(fromTeX(obj$x), "mm"), "bigpts", valueOnly=TRUE) +
        convertX(unit(xoffset, "in"), "bigpts", valueOnly=TRUE)
    y <- convertY(-unit(fromTeX(obj$y), "mm"), "bigpts", valueOnly=TRUE) +
        convertY(unit(yoffset, "in"), "bigpts", valueOnly=TRUE)
    width <- convertWidth(unit(fromTeX(obj$w), "mm"), "bigpts", valueOnly=TRUE)
    height <- convertHeight(unit(fromTeX(obj$h), "mm"), "bigpts",
                            valueOnly=TRUE)
    ## Below lwd=1, draw a line
    if (width < 25.4/72) {
        segmentsGrob(x + width/2,
                     y,
                     x + width/2,
                     y - height,
                     default.units="bigpts",
                     gp=gpar(lwd=72*width/25.4, lineend="butt"))
    } else if (height < 25.4/72) {
        segmentsGrob(x,
                     y + height/2,
                     x + width,
                     y - height/2,
                     default.units="bigpts",
                     gp=gpar(lwd=72*height/25.4, lineend="butt"))
    } else {
        rectGrob(x, y, width, -height, default.units="bigpts",
                 just=c("left", "bottom"),
                 gp=gpar(col=NA, fill="black"))
    }
}

################################################################################
## Sweep through operations in DVI file and create grobs from operations

## Default is to do nothing
for (i in 0:255) {
    assign(paste0("grob_op_", i), op_ignore)
}

## 0..127
## set_char_<i>
for (i in 0:127) {
    assign(paste0("grob_op_", i), op_set_char)
}

## 128..131
## set1
## set2
## set3
## set4
grob_op_128 <- op_set
grob_op_129 <- op_set
grob_op_130 <- op_set
grob_op_131 <- op_set

## 132
## set_rule
grob_op_132 <- op_set_rule

## 133..136
## put1
## put2
## put3
## put4
grob_op_133 <- op_put
grob_op_134 <- op_put
grob_op_135 <- op_put
grob_op_136 <- op_put

## 137
## put_rule
grob_op_137 <- op_put_rule

## 138
## nop

## 139
## bop
grob_op_139 <- op_bop

## 140
## eop
grob_op_140 <- op_eop

## 141
## push
grob_op_141 <- op_push

## 142
## pop
grob_op_142 <- op_pop

## 143..146
## right1
## right2
## right3
## right4
for (i in 143:146) {
    assign(paste0("grob_op_", i), op_right)
}

## 147..151
## w0
## w1
## w2
## w3
## w4
for (i in 147:151) {
    assign(paste0("grob_op_", i), op_w)
}

## 152..156
## x0
## x1
## x2
## x3
## x4
for (i in 152:156) {
    assign(paste0("grob_op_", i), op_x)
}

## 157..160
## down1
## down2
## down3
## down4
for (i in 157:160) {
    assign(paste0("grob_op_", i), op_down)
}

## 161..165
## y0
## y1
## y2
## y3
## y4
for (i in 161:165) {
    assign(paste0("grob_op_", i), op_y)
}

## 166..170
## z0
## z1
## z2
## z3
## z4
for (i in 166:170) {
    assign(paste0("grob_op_", i), op_z)
}

## 171..234
## fnt_num_<i-170>
for (i in 171:234) {
    assign(paste0("grob_op_", i), op_fnt_num)
}

## 235..238
## fnt1
## fnt2
## fnt3
## fnt4
for (i in 235:238) {
    assign(paste0("grob_op_", i), op_fnt)
}

## 239..242
## xxx1
## xxx2
## xxx3
## xxx4
for (i in 239:242) {
    assign(paste0("grob_op_", i), op_special)
}

## 243..246
## fnt_def_1
## fnt_def_2
## fnt_def_3
## fnt_def_4
grob_op_243 <- op_font_def
grob_op_244 <- op_font_def
grob_op_245 <- op_font_def
grob_op_246 <- op_font_def

## 247
## pre
grob_op_247 <- op_pre

## 248
## post
grob_op_248 <- op_post

## XeTeX
## 252
grob_op_252 <- op_x_font_def

## 253
grob_op_253 <- op_x_glyph

## 254
grob_op_254 <- op_x_glyph_str

## upTeX
## 255
## dir
grob_op_255 <- op_dir

grobDVI <- function(op) {
    opcode <- blockValue(op$blocks$op.opcode)
    base::get(paste0("grob_op_", opcode))(op)
}

