
## Functions useful for any sweep through operations within DVI file

op_ignore <- function(op) { }

## set_char_i and set_char are VERY similar
## (put_char_i is also VERY similar - just does not adjust (h, v)
setChar <- function(raw, put=FALSE) {
    if (tikzTransform()) {
        setTransformedChar(raw, put=FALSE)
        return()
    }
    h <- get("h")
    v <- get("v")
    ## Default baseline to first set char
    ## (may be overridden by, e.g., 'preview')
    if (is.na(get("baseline")))
        set("baseline", v)
    ## Current font
    fonts <- get("fonts")
    f <- get("f")
    font <- fonts[[f]]
    engine <- get("engine")
    colour <- get("colour")
    fontLib <- get("fontLib")
    ## Lost of things depend on text direction
    dir <- get("dir")
    ## Different engines specify glyphs in different ways
    glyphInfo <- engine$getGlyph(raw, font, dir, fontLib)
    bbox <- fontLib$glyphBounds(glyphInfo$index, font$fontdef$file,
                                font$size, dir)
    if (dir == 0) {
        width <- fontLib$glyphWidth(glyphInfo$index, font$fontdef$file,
                                    font$size)
        ## Position glyph then move
        x <- fromTeX(h)
        y <- fromTeX(v)
        glyph <- glyph(x, y, glyphInfo$char, glyphInfo$index, f, font$size,
                       colour=colour[1])
        updateBBoxHoriz(h + bbox[1]) ## left
        updateBBoxHoriz(h + bbox[2]) ## right
        updateBBoxVert(v - bbox[3]) ## bottom
        updateBBoxVert(v - bbox[4]) ## top
        if (!put)
            set("h", h + width[1])
        updateTextLeft(h)
        updateTextRight(h + width[1])
    } else {
        height <- fontLib$glyphHeight(glyphInfo$index, font$fontdef$file,
                                      font$size)
        ## Position glyph then move
        x <- fromTeX(h)
        ## y origin is v + bbox[4] (ymax) + height[2] (tsb)
        y <- fromTeX(v + bbox[4] + height[2])
        glyph <- glyph(x, y, glyphInfo$char, glyphInfo$index, f, font$size,
                       colour=colour[1])
        updateBBoxHoriz(h + bbox[1]) ## left
        updateBBoxHoriz(h + bbox[2]) ## right
        updateBBoxVert(v + bbox[3]) ## bottom
        updateBBoxVert(v + bbox[4] + height[2]) ## top
        if (!put) 
            set("v", v + height[1])
        updateTextLeft(h)
        updateTextRight(h + bbox[2])
    }
    addGlyph(glyph)
}

## 0..127
## set_char_<i>
op_set_char <- function(op) {
    setChar(op$blocks$op.opcode$fileRaw)
}

## 128..131
## set1
## set2
## set3
## set4
op_set <- function(op) {
    setChar(op$blocks$op.opparams$fileRaw)
}

setRule <- function(op, put=FALSE) {
    h <- get("h")
    v <- get("v")
    updateBBoxHoriz(h)
    updateBBoxVert(v)
    a <- blockValue(op$blocks$op.opparams.a)
    b <- blockValue(op$blocks$op.opparams.b)
    updateBBoxHoriz(h + b)
    updateBBoxVert(v - a)
    ## Need to create glyph object if there have been any glyphs prior to this
    addGlyphObj()
    addRuleObj(a, b)
    if (!put)
        set("h", get("h") + b)
}

## 132
## set_rule
op_set_rule <- function(op) setRule(op)

## 133..136
## put1
## put2
## put3
## put4
op_put <- function(op) {
    setChar(op$blocks$op.opparams$fileRaw, TRUE)
}

## 137
## put_rule
op_put_rule <- function(op) setRule(op, TRUE)

## 138
## nop

## 139
## bop
op_bop <- function(op) {
    ## Initialise locations
    set("h", 0)
    set("v", 0)
    set("w", 0)
    set("x", 0)
    set("y", 0)
    set("z", 0)
    ## Init text left/right
    set("textleft", Inf)
    set("textright", -Inf)
    ## Init bbox
    set("top", Inf)
    set("bottom", -Inf)
    set("left", Inf)
    set("right", -Inf)
    ## Init baseline
    set("baseline", NA)
    ## Init anchors
    set("hAnchors", NULL)
    set("vAnchors", NULL)
    ## Init cumulative structures
    initDVIobjs()
    set("glyphs", list())
    ## Stack for push/pop
    set("stack", list())
    set("i", 0)
    ## Font number
    set("f", NA)
    ## Default colour
    set("colour", NA)
    ## Default text direction
    set("dir", 0)
}

## 140
## eop
op_eop <- function(op) {
    ## Need to create glyph object if there have been any glyphs prior to this
    addGlyphObj()
}

## 141
## push
op_push <- function(op) {
    ## Maintain stack
    i <- get("i")
    stack <- get("stack")
    stack[[i + 1]] <- mget(c("h", "v", "w", "x", "y", "z"))
    set("i", i + 1)
    set("stack", stack)
}

## 142
## pop
op_pop <- function(op) {
    ## Maintain stack
    i <- get("i")
    stack <- get("stack")
    values <- stack[[i]]
    mapply(set, names(values), values)
    set("i", i - 1)
    set("stack", stack[-i])
}

## 143..146
## right1
## right2
## right3
## right4
op_right <- function(op) {
    ## Update location
    b <- blockValue(op$blocks$op.opparams)
    dir <- get("dir")
    if (dir == 0) {
        set("h", get("h") + b)
    } else {
        set("v", get("v") + b)
    }
}

## 147..151
## w0
## w1
## w2
## w3
## w4
op_w <- function(op) {
    ## Update location
    b <- op$blocks$op.opparams
    if (!is.null(b)) {
        set("w", blockValue(b))
    }
    dir <- get("dir")
    if (dir == 0) {
        set("h", get("h") + get("w"))
    } else {
        set("v", get("v") + get("w"))
    }
}

## 152..156
## x0
## x1
## x2
## x3
## x4
op_x <- function(op) {
    ## Update location
    b <- op$blocks$op.opparams
    if (!is.null(b)) {
        set("x", blockValue(b))
    }
    dir <- get("dir")
    if (dir == 0) {
        set("h", get("h") + get("x"))
    } else {
        set("v", get("v") + get("x"))
    }
}

## 157..160
## down1
## down2
## down3
## down4
op_down <- function(op) {
    ## Update location
    a <- blockValue(op$blocks$op.opparams)
    dir <- get("dir")
    if (dir == 0) {
        set("v", get("v") + a)
    } else {
        set("h", get("h") - a)
    }
}

## 161..165
## y0
## y1
## y2
## y3
## y4
op_y <- function(op) {
    ## Update location
    a <- op$blocks$op.opparams
    if (!is.null(a)) {
        set("y", blockValue(a))
    }
    dir <- get("dir")
    if (dir == 0) {
        set("v", get("v") + get("y"))
    } else {
        set("h", get("h") - get("y"))
    }
}

## 166..170
## z0
## z1
## z2
## z3
## z4
op_z <- function(op) {
    ## Update location
    a <- op$blocks$op.opparams
    if (!is.null(a)) {
        set("z", blockValue(a))
    }
    dir <- get("dir")
    if (dir == 0) {
        set("v", get("v") + get("z"))
    } else {
        set("h", get("h") - get("z"))
    }
}

## 171..234
## fnt_num_<i-170>
op_fnt_num <- function(op) {
    ## Maintain font number
    ## + 1 for 1-based indexing
    f <- blockValue(op$blocks$op.opcode) - 171 + 1 
    set("f", f)
}

## 235..238
## fnt1
## fnt2
## fnt3
## fnt4
op_fnt <- function(op) {
    k <- op$block$op.opparams
    f <- blockValue(k)
    set("f", f)
}

## 239..242
## xxx1
## xxx2
## xxx3
## xxx4
op_special <- function(op) {
    specialString <- paste(blockValue(op$blocks$op.opparams.string),
                           collapse="")
    packageSpecial(get("pkgs"), specialString)
}

## 243..246
## fnt_def_1
## fnt_def_2
## fnt_def_3
## fnt_def_4
op_font_def <- function(op) {
    ## Create font definition and save it
    fonts <- get("fonts")
    fontnum <- blockValue(op$blocks$op.opparams.k) + 1
    ## Avoid redefining the same font 
    if (is.null(fonts[[fontnum]]) ||
        !(identical_font(op, fonts[[fontnum]]$op))) {
        ## Reduce vector of individual characters to single character value
        fontname <- paste(blockValue(op$blocks$op.opparams.fontname.name),
                          collapse="")
        ## Different engines specify fonts in different ways
        engine <- get("engine")
        fontLib <- get("fontLib")
        fontdef <- engine$fontDef(fontname, fontLib)
        scale <- blockValue(op$blocks$op.opparams.s)
        design <- blockValue(op$blocks$op.opparams.d)
        mag <- get("mag")
        fonts[[fontnum]] <- list(fontdef=fontdef,
                                 size=fontdef$size*mag*scale/(1000*design),
                                 op=op)
        set("fonts", fonts)
    }
}

## 247
## pre
op_pre <- function(op) {
    ## Set up scaling for conversions, e.g., fromTeX()
    num <- blockValue(op$blocks$op.opparams.num)
    den <- blockValue(op$blocks$op.opparams.den)
    mag <- blockValue(op$blocks$op.opparams.mag)
    set("num", num)
    set("den", den)
    set("mag", mag)
    comment <- paste(blockValue(op$blocks$op.opparams.comment.string),
                     collapse="")
    if (comment != xdvirSignature)
        warning("DVI file was NOT generated by 'xdvir'")
    ## Initialise packages
    packageInit(get("pkgs"))
}

## 248
## post
op_post <- function(op) {
    packageFinal(get("pkgs"))    
}

## 249
## post_post

## 250..251
## Undefined

## XeTeX
## 252
## x_fnt_def
op_x_font_def <- function(op) {
    fontLib <- get("fontLib")
    ## Create font definition and save it
    fonts <- get("fonts")
    fontnum <- blockValue(op$blocks$op.opparams.fontnum) + 1
    ## Avoid redefining the same font 
    if (is.null(fonts[[fontnum]]) ||
        !(x_identical_font(op, fonts[[fontnum]]$op))) {
        fontnameChars <-
            blockValue(op$blocks$op.opparams.fontinfo.marker.fontname.block)
        fontname <- paste(fontnameChars, collapse="")
        fontindex <- blockValue(op$blocks$op.opparams.fontinfo.marker.fontindex)
        fontsize <- 72.27*fromTeX(blockValue(op$block$op.opparams.ptsize))/25.4
        ## Only XeTeX engine generates x_fnt_def
        fontdef <- fontDef(file=fontname,
                           index=fontindex,
                           fontLib$fontFamily(fontname),
                           fontLib$fontWeight(fontname),
                           fontLib$fontStyle(fontname),
                           fontSize(fontname))
        mag <- get("mag")
        fonts[[fontnum]] <- list(fontdef=fontdef,
                                 size=fontsize*mag/1000,
                                 op=op)
        set("fonts", fonts)
    }
}

setGlyphs <- function(op) {
    h <- get("h")
    v <- get("v")
    ## Default baseline to first set char
    ## (may be overridden by, e.g., 'preview')
    if (is.na(get("baseline")))
        set("baseline", v)
    ## Current font
    fonts <- get("fonts")
    f <- get("f")
    font <- fonts[[f]]
    colour <- get("colour")
    fontLib <- get("fontLib")
    ## NOTE differences from setChar():
    ##   Only a XeTeX engine will produce x_glyph_array
    ##   No concept of text direction (in XDV)
    ##   We have an ARRAY of glyphs
    ##   Get glyph ids directly from op
    nGlyphs <- blockValue(op$blocks$op.opparams.n)
    glyphIds <- blockValue(op$blocks$op.opparams.glyphs.id)
    glyphLocs <- paste0("op.opparams.glyphs.xy", 1:(2*nGlyphs))
    for (i in 1:nGlyphs) {
        ## NOTE that we really only need 'index' (?)
        glyphInfo <- list(name="", index=glyphIds[i], char="")
        bbox <- fontLib$glyphBounds(glyphInfo$index, font$fontdef$file,
                                    font$size, 0)
        width <- fontLib$glyphWidth(glyphInfo$index, font$fontdef$file,
                                    font$size)
        ## Position glyph then move
        x <- fromTeX(h)
        y <- fromTeX(v)
        glyph <- glyph(x, y, glyphInfo$char, glyphInfo$index, f, font$size,
                       colour=colour[1])
        updateBBoxHoriz(h + bbox[1]) ## left
        updateBBoxHoriz(h + bbox[2]) ## right
        updateBBoxVert(v - bbox[3]) ## bottom
        updateBBoxVert(v - bbox[4]) ## top
        set("h", h + width[1])
        updateTextLeft(h)
        updateTextRight(h + width[1])
        addGlyph(glyph)
        ## For next iteration
        h <- get("h")
    }
}

## 253
## x_glyph_array
op_x_glyph <- function(op) {
    setGlyphs(op)
}

## 254
## x_glyph_str
op_x_glyph_str <- function(op) {
    ## Just ignore string part of op
    setGlyphs(op)
}

## upTeX
## 255
## dir
op_dir <- function(op) {
    dir <- blockValue(op$blocks$op.opparams)
    set("dir", dir)
}
