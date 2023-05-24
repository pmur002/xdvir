
## Sweep through operations in DVI file and print each operation

for (i in 0:255) {
    assign(paste0("print_op_", i), op_ignore)
}

## set_char_<i>
print_set_char <- function(op) {
    int <- sprintf("%sd", blockValue(op$blocks$op.opcode))
    char <- rawToChar(op$blocks$op.opcode$fileRaw)
    str <- paste0("set_char_",
                  blockValue(op$blocks$op.opcode),
                  paste(rep(" ", 5 - nchar(int)), collapse=""),
                  "'", char, "'",
                  "\n")
    cat(str)
}
for (i in 0:127) {
    assign(paste0("print_op_", i), print_set_char)
}

print_set_longchar <- function(op) {
    str <- paste0("set",
                  blockValue(op$blocks$op.opcode) - 127,
                  "         ",
                  paste(op$blocks$op.opparams$fileRaw, collapse=" "),
                  "\n")
    cat(str)
}

for (i in 128:131) {
    assign(paste0("print_op_", i), print_set_longchar)
}

## set_rule
printRule <- function(op, opname) {
    a <- blockValue(op$blocks$op.opparams.a)
    b <- blockValue(op$blocks$op.opparams.b)
    str <- paste0(opname, "     ",
                  "a=", a, ", ",
                  "b=", b, "\n")
    cat(str)
}

print_op_132 <- function(op) {
    printRule(op, "set_rule")
}

## put_rule
print_op_137 <- function(op) {
    printRule(op, "put_rule")
}

## bop
print_op_139 <- function(op) {
    counters <- blockValue(op$blocks$op.opparams.counters)
    p <- blockValue(op$blocks$op.opparams.p)
    str <- paste0("bop          ",
                  "counters=", paste0(counters, collapse=" "), ", ",
                  "p=", p, "\n")
    cat(str)
}

## eop
print_op_140 <- function(op) {
    cat("eop\n")
}

## push
print_op_141 <- function(op) {
    cat("push\n")
}

## pop
print_op_142 <- function(op) {
    cat("pop\n")
}

## right<i>
printRight <- function(op, i) {
    b <- blockValue(op$blocks$op.opparams)
    str <- paste0("right", i, "       ",
                  "b=", b, "\n")
    cat(str)
}

print_op_143 <- function(op) {
    printRight(op, 1)
}
print_op_144 <- function(op) {
    printRight(op, 2)
}
print_op_145 <- function(op) {
    printRight(op, 3)
}
print_op_146 <- function(op) {
    printRight(op, 4)
}

## w<i>
printW <- function(op, i) {
    str <- paste0("w", i)
    if (i > 0) {
        b <- blockValue(op$blocks$op.opparams)
        str <- paste0(str, "           ",
                      "b=", b)
    }
    cat(str, "\n")
}

print_op_147 <- function(op) {
    printW(op, 0)
}
print_op_148 <- function(op) {
    printW(op, 1)
}
print_op_149 <- function(op) {
    printW(op, 2)
}
print_op_150 <- function(op) {
    printW(op, 3)
}
print_op_151 <- function(op) {
    printW(op, 4)
}

## x<i>
printX <- function(op, i) {
    str <- paste0("x", i)
    if (i > 0) {
        b <- blockValue(op$blocks$op.opparams)
        str <- paste0(str, "           ",
                      "b=", b)
    }
    cat(str, "\n")
}

print_op_152 <- function(op) {
    printX(op, 0)
}
print_op_153 <- function(op) {
    printX(op, 1)
}
print_op_154 <- function(op) {
    printX(op, 2)
}
print_op_155 <- function(op) {
    printX(op, 3)
}
print_op_156 <- function(op) {
    printX(op, 4)
}

## down<i>
printDown <- function(op, i) {
    a <- blockValue(op$blocks$op.opparams)
    str <- paste0("down", i, "        ",
                  "a=", a, "\n")
    cat(str)
}

print_op_157 <- function(op) {
    printDown(op, 1)
}
print_op_158 <- function(op) {
    printDown(op, 2)
}
print_op_159 <- function(op) {
    printDown(op, 3)
}
print_op_160 <- function(op) {
    printDown(op, 4)
}

## y<i>
printY <- function(op, i) {
    str <- paste0("y", i)
    if (i > 0) {
        a <- blockValue(op$blocks$op.opparams)
        str <- paste0(str, "           ",
                      "a=", a)
    }
    cat(str, "\n")
}

print_op_161 <- function(op) {
    printY(op, 0)
}
print_op_162 <- function(op) {
    printY(op, 1)
}
print_op_163 <- function(op) {
    printY(op, 2)
}
print_op_164 <- function(op) {
    printY(op, 3)
}
print_op_165 <- function(op) {
    printY(op, 4)
}

## z<i>
printZ <- function(op, i) {
    str <- paste0("z", i)
    if (i > 0) {
        a <- blockValue(op$blocks$op.opparams)
        str <- paste0(str, "           ",
                      "a=", a)
    }
    cat(str, "\n")
}

print_op_166 <- function(op) {
    printZ(op, 0)
}
print_op_167 <- function(op) {
    printZ(op, 1)
}
print_op_168 <- function(op) {
    printZ(op, 2)
}
print_op_169 <- function(op) {
    printZ(op, 3)
}
print_op_170 <- function(op) {
    printZ(op, 4)
}

## fnt_num_<i>
print_fnt_num <- function(op) {
    str <- paste0("fnt_num_",
                  blockValue(op$blocks$op.opcode) - 171,
                  "\n")
    cat(str)    
}
for (i in 171:234) {
    assign(paste0("print_op_", i), print_fnt_num)
}

## xxx<i> (specials)
print_xxx <- function(op) {
    str <- paste0("xxx",
                  blockValue(op$blocks$op.opcode) - 238,
                  "         ",
                  "k=", blockValue(op$blocks$op.opparams.length), "\n",
                  "             ",
                  "x=",
                  paste(blockValue(op$blocks$op.opparams.string), collapse=""),
                  "\n")
    cat(str)
}
for (i in 239:242) {
    assign(paste0("print_op_", i), print_xxx)
}

## font_def<i>
print_op_243 <- function(op) {
    fontnum <- blockValue(op$blocks$op.opparams.k)
    checksum <- blockValue(op$blocks$op.opparams.c)
    scale <- blockValue(op$blocks$op.opparams.s)
    design <- blockValue(op$blocks$op.opparams.d)
    fontname <- paste(blockValue(op$blocks$op.opparams.fontname.name),
                      collapse="")
    str <- paste0("fnt_def_1    ",
                  "fontnum=", fontnum, ", ",
                  "checksum=", checksum, ", ",
                  "scale=", scale, ", ",
                  "design=", design, ",\n             ",
                  "fontname=", fontname, "\n")
    cat(str)
}

## pre
print_op_247 <- function(op) {
    i <- blockValue(op$blocks$op.opparams.i)
    num <- blockValue(op$blocks$op.opparams.num)
    den <- blockValue(op$blocks$op.opparams.den)
    mag <- blockValue(op$blocks$op.opparams.mag)
    comment <- blockValue(op$blocks$op.opparams.comment.string)
    str <- paste0("pre          ",
                  "version=", i, ", ",
                  "num=", num, ", ",
                  "den=", den, ", ",
                  "mag=", mag, ",\n             ",
                  "comment=", paste(comment, collapse=""), "\n")
    cat(str)
}

## post
print_op_248 <- function(op) {
    cat("post\n")
}

## post_post
print_op_249 <- function(op) {
    cat("post_post\n")
}

## XFontDef (XeTeX)
print_op_252 <- function(op) {
    ## print(op)
    fontnum <- blockValue(op$blocks$op.opparams.fontnum)
    ptsize <- blockValue(op$blocks$op.opparams.ptsize)
    fontname <-
        paste(blockValue(op$blocks$op.opparams.fontinfo.marker.fontname.block),
              collapse="")
    fontindex <- blockValue(op$blocks$op.opparams.fontinfo.marker.fontindex)
    str <- paste0("x_fnt_def    ",
                  "fontnum=", fontnum, ", ",
                  "ptsize=", ptsize, "\n             ",
                  "fontname=", fontname, " [", fontindex, "]", "\n")
    cat(str)
}

## XGlyphArray (XeTeX) 
print_op_253 <- function(op) {
    ## print(op)
    n <- blockValue(op$blocks$op.opparams.n)
    xIndex <- 2*(1:n) - 1
    yIndex <- xIndex + 1
    glyphX <- unlist(lapply(xIndex,
                            function(i) {
                                name <- paste0("op.opparams.glyphs.xy", i)
                                blockValue(op$blocks[[name]])
                            }))
    glyphY <- unlist(lapply(yIndex,
                            function(i) {
                                name <- paste0("op.opparams.glyphs.xy", i)
                                blockValue(op$blocks[[name]])
                            }))
    str <- paste0("x_glyph      ",
                  "id=", blockValue(op$blocks$op.opparams.glyphs.id), ", ",
                  "x=", glyphX, ", y=", glyphY, collapse="\n")
    cat(str, "\n")
}

## XStringGlyphArray (XeTeX)
print_op_254 <- function(op) {
    ## print(op)
    n <- blockValue(op$blocks$op.opparams.n)
    xIndex <- 2*(1:n) - 1
    yIndex <- xIndex + 1
    glyphX <- unlist(lapply(xIndex,
                            function(i) {
                                name <- paste0("op.opparams.glyphs.xy", i)
                                blockValue(op$blocks[[name]])
                            }))
    glyphY <- unlist(lapply(yIndex,
                            function(i) {
                                name <- paste0("op.opparams.glyphs.xy", i)
                                blockValue(op$blocks[[name]])
                            }))
    str <- paste0(paste0("x_glyph_str  '",
                         paste(blockValue(op$blocks$op.opparams.text),
                               collapse=""),
                         "'\n"),
                  paste0("x_glyph      ",
                         "id=", blockValue(op$blocks$op.opparams.glyphs.id),
                         ", ", "x=", glyphX, ", y=", glyphY, 
                         collapse="\n"),
                  collapse="\n")
    cat(str, "\n")
}

## upTeX
print_op_255 <- function(op) {
    dir <- blockValue(op$blocks$op.opparams)
    str <- paste0("dir          ", dir, "\n")
    cat(str)
}

printDVI <- function(op) {
    opcode <- blockValue(op$blocks$op.opcode)
    base::get(paste0("print_op_", opcode))(op)
}

################################################################################
## User API

print.DVI <- function(x, ...) {
    invisible(lapply(x, printDVI))
}
