
## Maintain a list of objects within a TeX state

addDVIobj <- function(x, state) {
    objs <- TeXget("objList", state)
    objs[[length(objs) + 1]] <- x
    TeXset("objList", objs, state)
}

## Add accumulated glyphs
addGlyphObjs <- function(state) {
    glyphs <- TeXget("glyphs", state)
    if (length(glyphs)) {
        glyphObjs <- do.call(rbind, glyphs)
        glyphList <- split(glyphObjs,
                           apply(glyphObjs[c("rotation", "scaleX", "scaleY",
                                             "skewX", "skewY")],
                                 1, paste, collapse=":"))
        lapply(glyphList,
               function(x) {
                   if (x$scaleX[1] == 1 && x$scaleY[1] == 1 &&
                       x$skewX[1] == 0 && x$skewY[1] == 0) {
                       if (x$rotation[1] == 0) {
                           class(x) <- c("XDVIRglyphObj", class(x))
                       } else {
                           class(x) <- c("XDVIRrotatedGlyphObj", class(x))
                       }
                   } else {
                       class(x) <- c("XDVIRtransformedGlyphObj", class(x))
                   }
                   addDVIobj(x, state)
               })
    }
    ## Empty the "glyphs" list
    TeXset("glyphs", list(), state)
}

## Add a rule
addRuleObj <- function(a, b, aa, bb, state) {
    if (a > 0 && b > 0) {
        ruleObj <- list(x=TeXget("h", state), y=TeXget("v", state),
                        w=b, h=a,
                        xx=TeXget("hh", state), yy=TeXget("vv", state),
                        ww=bb, hh=aa)
        class(ruleObj) <- "XDVIRruleObj"
        addDVIobj(ruleObj, state)
    } 
}

################################################################################
## Objects from ops

## Default is to do nothing
for (i in 0:255) {
    assign(paste0("obj_op_", i), function(...) op_ignore(...))
}

## Many functions should only do something if we are on the relevant page.
## This includes all drawing operations like set and rule
## BUT does not include positionining operations like
## push/pop/down/right/w/x etc in case they carry across pages
## ALSO does not include font definitions or settings for same reason
do_op <- function(op_fun, op, state) {
    if (TeXget("currentPage", state) == TeXget("whichPage", state))
        op_fun(op, state)
}

## 0..127
## set_char_<i>
for (i in 0:127) {
    assign(paste0("obj_op_", i), function(...) do_op(op_set_char, ...))
}

## 128..131
## set1
## set2
## set3
## set4
obj_op_128 <- function(...) do_op(op_set, ...)
obj_op_129 <- function(...) do_op(op_set, ...)
obj_op_130 <- function(...) do_op(op_set, ...)
obj_op_131 <- function(...) do_op(op_set, ...)

## 132
## set_rule
obj_op_132 <- function(...) do_op(op_set_rule, ...)

## 133..136
## put1
## put2
## put3
## put4
obj_op_133 <- function(...) do_op(op_put, ...)
obj_op_134 <- function(...) do_op(op_put, ...)
obj_op_135 <- function(...) do_op(op_put, ...)
obj_op_136 <- function(...) do_op(op_put, ...)

## 137
## put_rule
obj_op_137 <- function(...) do_op(op_put_rule, ...)

## 138
## nop

## 139
## bop
obj_op_139 <- function(...) op_bop(...)

## 140
## eop
obj_op_140 <- function(...) op_eop(...)

## 141
## push
obj_op_141 <- function(...) op_push(...)

## 142
## pop
obj_op_142 <- function(...) op_pop(...)

## 143..146
## right1
## right2
## right3
## right4
for (i in 143:146) {
    assign(paste0("obj_op_", i), function(...) op_right(...))
}

## 147..151
## w0
## w1
## w2
## w3
## w4
for (i in 147:151) {
    assign(paste0("obj_op_", i), function(...) op_w(...))
}

## 152..156
## x0
## x1
## x2
## x3
## x4
for (i in 152:156) {
    assign(paste0("obj_op_", i), function(...) op_x(...))
}

## 157..160
## down1
## down2
## down3
## down4
for (i in 157:160) {
    assign(paste0("obj_op_", i), function(...) op_down(...))
}

## 161..165
## y0
## y1
## y2
## y3
## y4
for (i in 161:165) {
    assign(paste0("obj_op_", i), function(...) op_y(...))
}

## 166..170
## z0
## z1
## z2
## z3
## z4
for (i in 166:170) {
    assign(paste0("obj_op_", i), function(...) op_z(...))
}

## 171..234
## fnt_num_<i-170>
for (i in 171:234) {
    assign(paste0("obj_op_", i), function(...) op_fnt_num(...))
}

## 235..238
## fnt1
## fnt2
## fnt3
## fnt4
for (i in 235:238) {
    assign(paste0("obj_op_", i), function(...) op_fnt(...))
}

## 239..242
## xxx1
## xxx2
## xxx3
## xxx4
for (i in 239:242) {
    ## NOTE that this could cause trouble (not performing a "special" op
    ## if we are not on the right page) if, for example, colour is set
    ## on one page and carried over to next page.
    assign(paste0("obj_op_", i), function(...) do_op(op_special, ...))
}

## 243..246
## fnt_def_1
## fnt_def_2
## fnt_def_3
## fnt_def_4
obj_op_243 <- function(...) op_font_def(...)
obj_op_244 <- function(...) op_font_def(...)
obj_op_245 <- function(...) op_font_def(...)
obj_op_246 <- function(...) op_font_def(...)

## 247
## pre
obj_op_247 <- function(...) op_pre(...)

## 248
## post
obj_op_248 <- function(...) op_post(...)

## XeTeX
## 252
obj_op_252 <- function(...) op_x_font_def(...)

## 253
obj_op_253 <- function(...) do_op(op_x_glyph, ...)

## 254
obj_op_254 <- function(...) do_op(op_x_glyph_str, ...)

## upTeX
## 255
## dir
obj_op_255 <- function(...) op_dir(...)

DVItoObj <- function(op, state) {
    opcode <- blockValue(op$blocks$op.opcode)
    base::get(paste0("obj_op_", opcode))(op, state)
}
