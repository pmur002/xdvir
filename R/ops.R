
## Functions for sweeping through operations within DVI file

op_ignore <- function(op, state) { }

## Glyph index from raw bytes
glyphIndex <- function(raw) {
    nbytes <- length(raw)
    switch(nbytes,
           ## Single byte from set_char_i or set1
           as.integer(raw),
           ## Two bytes from set2
           sum(as.integer(raw)*16^c(2, 0)),
           ## Three bytes from set3
           sum(as.integer(raw)*16^c(4, 2, 0)),
           ## To be implemented
           ## Have not yet witnessed set4 op
           stop("set4 not yet supported"))
}

moveRight <- function(x, state) {
    h <- TeXget("h", state)
    hh <- TeXget("hh", state)
    ## Avoid hh drifting too far from h
    maxDrift <- 2
    hhh = round(TeX2px(h + x, state))
    if (abs(hhh - hh) > maxDrift) {
        if (hhh > hh) {
            hh = hhh - maxDrift
        } else {
            hh = hhh + maxDrift
        }
        TeXset("hh", hh, state)
    }
    TeXset("h", h + x, state)        
}

moveDown <- function(x, state) {
    v <- TeXget("v", state)
    vv <- TeXget("vv", state)
    ## Avoid vv drifting too far from v
    maxDrift <- 2
    vvv = round(TeX2px(v + x, state))
    if (abs(vvv - vv) > maxDrift) {
        if (vvv > vv) {
            vv = vvv - maxDrift
        } else {
            vv = vvv + maxDrift
        }
        TeXset("vv", vv, state)
    }
    TeXset("v", v + x, state)        
}

## set_char_i and set_char are VERY similar
## (put_char_i is also VERY similar - just does not adjust (h, v)
setChar <- function(raw, put=FALSE, state) {
    if (tikzTransform(state)) {
        setTransformedChar(raw, put=FALSE, state)
        return()
    }
    h <- TeXget("h", state)
    v <- TeXget("v", state)
    hh <- TeXget("hh", state)
    vv <- TeXget("vv", state)
    ## Default baseline to first set char
    ## (may be overridden by, e.g., 'preview')
    if (is.na(TeXget("baseline", state)))
        TeXset("baseline", v, state)
    ## Current font
    fonts <- TeXget("fonts", state)
    f <- TeXget("f", state)
    font <- fonts[[f]]
    colour <- TeXget("colour", state)
    fontLib <- TeXget("fontLib", state)
    ## Lots of things depend on text direction
    dir <- TeXget("dir", state)
    id <- glyphIndex(raw)
    bbox <- TeXglyphBounds(id, font$file, font$size, fontLib, state)
    if (dir == 0) {
        width <- TeXglyphWidth(id, font$file, font$size, fontLib, state)
        ## Position glyph then move
        x <- h
        y <- v
        xx <- hh
        yy <- vv
        glyph <- glyph(x, y, xx, yy, id, f, font$size, colour=colour[1])
        updateBBoxHoriz(h + bbox[1], state) ## left
        updateBBoxHoriz(h + bbox[3], state) ## right
        updateBBoxVert(v - bbox[2], state) ## bottom
        updateBBoxVert(v - bbox[4], state) ## top
        if (!put) {
            TeXset("hh", hh + round(TeX2px(width[1], state)), state)
            moveRight(width[1], state)
        }
        updateTextLeft(h, state)
        updateTextRight(h + width[1], state)
    } else {
        height <- TeXglyphHeight(id, font$file, font$size, fontLib, state)
        ## Position glyph then move
        x <- h
        xx <- hh
        ## y origin is v + bbox[4] (ymax) + height[2] (tsb)
        y <- v + bbox[4] + height[2]
        yy <- vv + round(TeX2px(bbox[4] + height[2], state))
        glyph <- glyph(x, y, xx, yy, id, f, font$size, colour=colour[1])
        updateBBoxHoriz(h + bbox[1], state) ## left
        updateBBoxHoriz(h + bbox[3], state) ## right
        updateBBoxVert(v + bbox[2], state) ## bottom
        updateBBoxVert(v + bbox[4] + height[2], state) ## top
        if (!put) {
            TeXset("vv", vv + round(TeX2px(height[1], state)), state)
            moveDown(height[1], state)
        }
        updateTextLeft(h, state)
        updateTextRight(h + bbox[2], state)
    }
    addGlyph(glyph, state)
}

## 0..127
## set_char_<i>
op_set_char <- function(op, state) {
    setChar(op$blocks$op.opcode$fileRaw, put=FALSE, state)
}

## 128..131
## set1
## set2
## set3
## set4
op_set <- function(op, state) {
    setChar(op$blocks$op.opparams$fileRaw, put=FALSE, state)
}

rulePixels <- function(x, state) {
    pixels <- TeX2px(x, state)
    n = trunc(pixels)
    if (n < pixels) {
        n + 1
    } else {
        n
    }
}

setRule <- function(op, put=FALSE, state) {
    h <- TeXget("h", state)
    v <- TeXget("v", state)
    hh <- TeXget("hh", state)
    updateBBoxHoriz(h, state)
    updateBBoxVert(v, state)
    a <- blockValue(op$blocks$op.opparams.a)
    b <- blockValue(op$blocks$op.opparams.b)
    updateBBoxHoriz(h + b, state)
    updateBBoxVert(v - a, state)
    ## Need to create glyph object if there have been any glyphs prior to this
    addGlyphObjs(state)
    aa <- rulePixels(a, state)
    bb <- rulePixels(b, state)
    addRuleObj(a, b, aa, bb, state)
    if (!put) {
        TeXset("hh", hh + bb, state)
        moveRight(b, state)
    }
}

## 132
## set_rule
op_set_rule <- function(op, state) setRule(op, FALSE, state)

## 133..136
## put1
## put2
## put3
## put4
op_put <- function(op, state) {
    setChar(op$blocks$op.opparams$fileRaw, TRUE, state)
}

## 137
## put_rule
op_put_rule <- function(op, state) setRule(op, TRUE, state)

## 138
## nop

## 139
## bop
op_bop <- function(op, state) {
}

## 140
## eop
op_eop <- function(op, state) {
    ## Need to create glyph object if there have been any glyphs prior to this
    addGlyphObjs(state)
}

## 141
## push
op_push <- function(op, state) {
    ## Maintain stack
    i <- TeXget("i", state)
    stack <- TeXget("stack", state)
    stack[[i + 1]] <- TeXmget(c("h", "v", "w", "x", "y", "z",
                                "hh", "vv"),
                              state)
    TeXset("i", i + 1, state)
    TeXset("stack", stack, state)
}

## 142
## pop
op_pop <- function(op, state) {
    ## Maintain stack
    i <- TeXget("i", state)
    stack <- TeXget("stack", state)
    values <- stack[[i]]
    mapply(TeXset, names(values), values, MoreArgs=list(state))
    TeXset("i", i - 1, state)
    TeXset("stack", stack[-i], state)
}

hSpace <- function(x, state) {
    f <- TeXget("f", state)
    ## Only worry about maintaining hh and vv if we have set a font
    if (!is.na(f)) {
        fonts <- TeXget("fonts", state)
        font <- fonts[[f]]
        fntSpace <- font$fontSpace
        h <- TeXget("h", state)
        hh <- TeXget("hh", state)
        if (x >= fntSpace || x <= (-4 * fntSpace)) {
            hh = round(TeX2px(h + x, state))
        } else {
            hh = hh + round(TeX2px(x, state))
        }
        TeXset("hh", hh, state)
    }
}

vSpace <- function(x, state) {
    f <- TeXget("f", state)
    ## Only worry about maintaining hh and vv if we have set a font
    if (!is.na(f)) {
        fonts <- TeXget("fonts", state)
        font <- fonts[[f]]
        fntSpace <- font$fontSpace
        v <- TeXget("v", state)
        vv <- TeXget("vv", state)
        if (abs(x) >= (5 * fntSpace)) {
            vv = round(TeX2px(v + x, state))
        } else {
            vv = vv + round(TeX2px(x, state))
        }
        TeXset("vv", vv, state)
    }
}

## 143..146
## right1
## right2
## right3
## right4
op_right <- function(op, state) {
    ## Update location
    b <- blockValue(op$blocks$op.opparams)
    dir <- TeXget("dir", state)
    if (dir == 0) {
        hSpace(b, state)
        moveRight(b, state)
    } else {
        vSpace(b, state)
        moveDown(b, state)
    }
}

## 147..151
## w0
## w1
## w2
## w3
## w4
op_w <- function(op, state) {
    ## Update location
    b <- op$blocks$op.opparams
    if (!is.null(b)) {
        TeXset("w", blockValue(b), state)
    }
    w <- TeXget("w", state)
    dir <- TeXget("dir", state)
    if (dir == 0) {
        hSpace(w, state)
        moveRight(w, state)
    } else {
        vSpace(w, state)
        moveDown(w, state)
    }
}

## 152..156
## x0
## x1
## x2
## x3
## x4
op_x <- function(op, state) {
    ## Update location
    b <- op$blocks$op.opparams
    if (!is.null(b)) {
        TeXset("x", blockValue(b), state)
    }
    x <- TeXget("x", state)
    dir <- TeXget("dir", state)
    if (dir == 0) {
        hSpace(x, state)
        moveRight(x, state)
    } else {
        vSpace(x, state)
        moveDown(x, state)
    }
}

## 157..160
## down1
## down2
## down3
## down4
op_down <- function(op, state) {
    ## Update location
    a <- blockValue(op$blocks$op.opparams)
    dir <- TeXget("dir", state)
    if (dir == 0) {
        vSpace(a, state)
        moveDown(a, state)
    } else {
        hSpace(-a, state)
        moveRight(-a, state)
    }
}

## 161..165
## y0
## y1
## y2
## y3
## y4
op_y <- function(op, state) {
    ## Update location
    a <- op$blocks$op.opparams
    if (!is.null(a)) {
        TeXset("y", blockValue(a), state)
    }
    y <- TeXget("y", state)
    dir <- TeXget("dir", state)
    if (dir == 0) {
        vSpace(y, state)
        moveDown(y, state)
    } else {
        hSpace(-y, state)
        moveRight(-y, state)
    }
}

## 166..170
## z0
## z1
## z2
## z3
## z4
op_z <- function(op, state) {
    ## Update location
    a <- op$blocks$op.opparams
    if (!is.null(a)) {
        TeXset("z", blockValue(a), state)
    }
    z <- TeXget("z", state)
    dir <- TeXget("dir", state)
    if (dir == 0) {
        vSpace(z, state)
        moveDown(z, state)
    } else {
        hSpace(-z, state)
        moveRight(-z, state)
    }
}

## 171..234
## fnt_num_<i-170>
op_fnt_num <- function(op, state) {
    ## Maintain font number
    ## + 1 for 1-based indexing
    f <- blockValue(op$blocks$op.opcode) - 171 + 1 
    TeXset("f", f, state)
}

## 235..238
## fnt1
## fnt2
## fnt3
## fnt4
op_fnt <- function(op, state) {
    k <- op$block$op.opparams
    f <- blockValue(k)
    TeXset("f", f, state)
}

## 239..242
## xxx1
## xxx2
## xxx3
## xxx4
op_special <- function(op, state) {
    specialString <- paste(blockValue(op$blocks$op.opparams.string),
                           collapse="")
    packageSpecial(TeXget("packages", state), specialString, state)
}

## 243..246
## fnt_def_1
## fnt_def_2
## fnt_def_3
## fnt_def_4
op_font_def <- function(op, state) {
    mag <- TeXget("mag", state)
    engine <- TeXget("engine", state)
    fontLib <- TeXget("fontLib", state)
    ## Create font definition and save it
    fonts <- TeXget("fonts", state)
    fontnum <- blockValue(op$blocks$op.opparams.k) + 1
    ## Avoid redefining the same font 
    if (is.null(fonts[[fontnum]]) ||
        !(identical_font(op, fonts[[fontnum]]$op))) {
        ## Reduce vector of individual characters to single character value
        fontname <- paste(blockValue(op$blocks$op.opparams.fontname.name),
                          collapse="")
        fontfile <- gsub("[[]|[]].*", "", fontname)
        s <- blockValue(op$blocks$op.opparams.s)
        d <- blockValue(op$blocks$op.opparams.d)
        mag <- TeXget("mag", state)
        fonts[[fontnum]] <- list(file=fontfile,
                                 index=0,
                                 size=s*(mag/1000),
                                 ## For pixel adjustments
                                 fontSpace=s %/% 6, 
                                 op=op)
        TeXset("fonts", fonts, state)
    }
}

## 247
## pre
op_pre <- function(op, state) {
    ## Set up scaling for conversions, e.g., TeX2pt() and TeX2px()
    ## DVI units to (big) pts
    ## num/den is dvi -> decimicrons
    ## decimicrons / 10^4 -> mm
    ## mm / 25.4 -> in
    ## 72 * in -> (big) pts
    ## mag/1000 is magnification
    num <- blockValue(op$blocks$op.opparams.num)
    den <- blockValue(op$blocks$op.opparams.den)
    mag <- blockValue(op$blocks$op.opparams.mag)
    TeXset("num", num, state)
    TeXset("den", den, state)
    TeXset("mag", mag, state)
    scale <- TeXget("scale", state)
    trueFactor <- 72 * (((num/den) / 10^4) / 25.4)
    factor <- (mag/1000) * trueFactor
    TeXset("factor", factor, state)
    TeXset("scaledFactor", scale*factor, state)
    ## DVI units to pixels
    ## num/den is dvi -> decimicrons
    ## decimicrons / 254000 -> in
    ## dpi * in -> pixels
    dpi <- TeXget("dpi", state)
    ## If dpi not specified, use number of DVI units per inch
    ## => hh/vv should end up (basically) the same as h/v
    if (is.na(dpi)) {
        dpi <- 65536 * 72.27
    }
    trueConv <- dpi * ((num/den) / 254000)
    conv <- (mag/1000) * trueConv
    TeXset("conv", conv, state)
    TeXset("scaledConv", scale*conv, state)
    comment <- paste(blockValue(op$blocks$op.opparams.comment.string),
                     collapse="")
    ## Initialise packages
    packageInit(TeXget("packages", state), state)
}

## 248
## post
op_post <- function(op, state) {
    packageFinal(TeXget("packages", state), state)
}

## 249
## post_post

## 250..251
## Undefined

## XeTeX
## 252
## x_fnt_def
op_x_font_def <- function(op, state) {
    mag <- TeXget("mag", state)
    engine <- TeXget("engine", state)
    fontLib <- TeXget("fontLib", state)
    ## Create font definition and save it
    fonts <- TeXget("fonts", state)
    fontnum <- blockValue(op$blocks$op.opparams.fontnum) + 1
    ## Avoid redefining the same font 
    if (is.null(fonts[[fontnum]]) ||
        !(identical_font(op, fonts[[fontnum]]$op))) {
        fontnameChars <-
            blockValue(op$blocks$op.opparams.fontinfo.marker.fontname.block)
        fontname <- paste(fontnameChars, collapse="")
        fontindex <- blockValue(op$blocks$op.opparams.fontinfo.marker.fontindex)
        fontsize <- blockValue(op$block$op.opparams.ptsize)
        fonts[[fontnum]] <- list(file=fontname,
                                 index=fontindex,
                                 size=fontsize*(mag/1000),
                                 ## For pixel adjustments
                                 fontSpace=fontsize %/% 6, 
                                 op=op)
        TeXset("fonts", fonts, state)
    }
}

setGlyphs <- function(op, state) {
    if (tikzTransform(state)) {
        setTransformedGlyphs(op, state)
        return()
    }
    h <- TeXget("h", state)
    v <- TeXget("v", state)
    hh <- TeXget("hh", state)
    vv <- TeXget("vv", state)
    ## Default baseline to first set char
    ## (may be overridden by, e.g., 'preview')
    if (is.na(TeXget("baseline", state)))
        TeXset("baseline", v, state)
    ## Current font
    fonts <- TeXget("fonts", state)
    f <- TeXget("f", state)
    font <- fonts[[f]]
    colour <- TeXget("colour", state)
    fontLib <- TeXget("fontLib", state)
    ## NOTE:
    ##   No concept of text direction (in XDV)
    ##   We have an ARRAY of glyphs
    nGlyphs <- blockValue(op$blocks$op.opparams.n)
    glyphIds <- blockValue(op$blocks$op.opparams.glyphs.id)
    glyphLocs <- paste0("op.opparams.glyphs.xy", 1:(2*nGlyphs))
    glyphWidth <- 0
    for (i in 1:nGlyphs) {
        id <- glyphIds[i]
        glyphX <- blockValue(op$blocks[[glyphLocs[2*i - 1]]])
        glyphY <- blockValue(op$blocks[[glyphLocs[2*i]]])
        x <- h + glyphX
        y <- v - glyphY
        xx <- round(TeX2px(x, state))
        yy <- round(TeX2px(y, state))
        glyph <- glyph(x, y, xx, yy, id, f, font$size, colour=colour[1])
        ## Update bounding box of drawing
        ## BUT do NOT update h/v
        bbox <- TeXglyphBounds(id, font$file, font$size, fontLib, state)
        width <- TeXglyphWidth(id, font$file, font$size, fontLib, state)
        updateBBoxHoriz(x + bbox[1], state) ## left
        updateBBoxHoriz(x + bbox[3], state) ## right
        updateBBoxVert(y - bbox[2], state) ## bottom
        updateBBoxVert(y - bbox[4], state) ## top
        updateTextLeft(x, state)
        updateTextRight(x + width[1], state)
        ## Keep track of total glyph width
        glyphWidth <- glyphWidth + width[1]
        addGlyph(glyph, state)
    }
    ## Update h at the end for all glyphs
    TeXset("hh", hh + round(TeX2px(glyphWidth, state)), state)
    moveRight(glyphWidth, state)
}

## 253
## x_glyph_array
op_x_glyph <- function(op, state) {
    setGlyphs(op, state)
}

## 254
## x_glyph_str
op_x_glyph_str <- function(op, state) {
    ## Just ignore string part of op
    setGlyphs(op, state)
}

## upTeX
## 255
## dir
op_dir <- function(op, state) {
    dir <- blockValue(op$blocks$op.opparams)
    TeXset("dir", dir, state)
}

################################################################################

## Functions for extracting specific pieces of DVI

opCodes <- function(dvi) {
    sapply(dvi,
           function(op) {
               blockValue(op$blocks$op.opcode)
           })
}

opParams <- function(dvi) {
    lapply(dvi,
           function(op) {
               ## Remove code (first component of blocks)
               nocode <- op$blocks[-1]
               params <- lapply(nocode,
                                function(p) {
                                    blockValue(p)
                                })
               names <- names(nocode)
               if (length(names)) {
                   names(params) <- gsub("op.opparams[.]?", "", names)
               }
               params
           })
}

operationNames <- character(256)
operationNames[1:128] <- paste0("set_char_", 0:127)
operationNames[129:132] <- paste0("set", 1:4)
operationNames[133] <- "set_rule"
operationNames[134:137] <- paste0("put", 1:4)
operationNames[138] <- "put_rule"
operationNames[139] <- "nop"
operationNames[140] <- "bop"
operationNames[141] <- "eop"
operationNames[142] <- "push"
operationNames[143] <- "pop"
operationNames[144:147] <- paste0("right", 1:4)
operationNames[148:152] <- paste0("w", 0:4)
operationNames[153:157] <- paste0("x", 0:4)
operationNames[158:161] <- paste0("down", 1:4)
operationNames[162:166] <- paste0("y", 0:4)
operationNames[167:171] <- paste0("z", 0:4)
operationNames[172:235] <- paste0("fnt_num_", 0:63)
operationNames[236:239] <- paste0("fnt", 1:4)
operationNames[240:243] <- paste0("xxx", 1:4)
operationNames[244:247] <- paste0("fnt_def", 1:4)
operationNames[248] <- "pre"
operationNames[249] <- "post"
operationNames[250] <- "post_post"
operationNames[253] <- "x_fnt_def"
operationNames[254] <- "x_glyph"
operationNames[255] <- "x_glyph_str"
operationNames[256] <- "dir"
    
opNames <- function(codes) {
    if (!length(codes) ||
        !all(is.finite(codes)) ||
        any(codes < 0 | codes > 255))
        stop("Invalid codes")
    operationNames[codes + 1]
}

commentString <- function(dvi) {
    codes <- opCodes(dvi)
    params <- opParams(dvi)
    commentParams <- params[[which(codes == 247)]]
    paste(commentParams$comment.string, collapse="")
}

