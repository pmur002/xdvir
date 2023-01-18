
## Functions useful for any sweep through operations within DVI file

op_ignore <- function(op) { }

## set_char_i and set_char are VERY similar
setChar <- function(raw) {
    h <- get("h")
    v <- get("v")
    ## Default baseline to first set char
    ## (may be overridden by, e.g., 'preview')
    if (is.na(get("baseline")))
        set("baseline", v)
    ## Location before glyph
    x <- fromTeX(h)
    y <- fromTeX(v)
    ## Current font
    fonts <- get("fonts")
    f <- get("f")
    font <- fonts[[f]]
    engine <- get("engine")
    ## Different engines specify glyphs in different ways
    glyphInfo <- engine$getGlyph(raw, font)
    glyph <- glyph(x, y, glyphInfo$char, glyphInfo$index, f, font$size)
    addGlyph(glyph)
    ## Update bbox and location after glyph
    dir <- get("dir")
    bbox <- engine$glyphMetrics(glyphInfo, font)
    if (dir == 0) {
        width <- engine$glyphWidth(glyphInfo, font)
        updateHoriz(h + width[2]) ## left
        updateHoriz(h + width[2] + (bbox[2] - bbox[1])) ## right
        updateVert(v - bbox[3]) ## bottom
        updateVert(v - bbox[4]) ## top
        set("h", h + width[1])
    } else {
        height <- engine$glyphHeight(glyphInfo, font)
        updateHoriz(h - (bbox[2] - bbox[1])/2) ## left
        updateHoriz(h + (bbox[2] - bbox[1])/2) ## right
        updateVert(v - bbox[3]) ## bottom
        updateVert(v - bbox[4]) ## top
        set("v", v + height[1])
    }
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

## 132
## set_rule
op_set_rule <- function(op) {
    b <- blockValue(op$blocks$op.opparams.b)
    set("h", get("h") + b)
}

## 133..136
## put1
## put2
## put3
## put4

## 137
## put_rule

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
    ## Init bbox
    set("top", Inf)
    set("bottom", -Inf)
    set("left", Inf)
    set("right", -Inf)
    ## Init baseline
    set("baseline", NA)
    ## Init cumulative structures
    set("glyphs", list())
    set("glyphNum", 1)
    ## Stack for push/pop
    set("stack", list())
    set("i", 0)
    ## Font number
    set("f", NA)
    ## Default text direction
    set("dir", 0)
}

## 140
## eop

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
    set("h", get("h") + b)
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
    set("h", get("h") + get("w"))
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
    set("h", get("h") + get("x"))
}

## 157..160
## down1
## down2
## down3
## down4
op_down <- function(op) {
    ## Update location
    a <- blockValue(op$blocks$op.opparams)
    set("v", get("v") + a)
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
    set("v", get("v") + get("y"))
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
    set("v", get("v") + get("z"))
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

## 239..242
## xxx1
## xxx2
## xxx3
## xxx4

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
        fontdef <- engine$defineFont(fontname)
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
}

## 248
## post

## 249
## post_post

## 250..251
## Undefined

## XeTeX
## 252
## x_font_def

## 253
## x_glyph_array

## 254
## x_string_glyph_array

## upTeX
## 255
## dir
op_dir <- function(op) {
    dir <- blockValue(op$blocks$op.opparams)
    set("dir", dir)
}
