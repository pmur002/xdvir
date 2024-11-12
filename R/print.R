
## Sweep through operations in DVI file and print each operation

print_ignore_op <- function(op, code, params) { }

for (i in 0:255) {
    assign(paste0("print_op_", i), print_ignore_op)
}

## set_char_<i>
print_set_char <- function(op, code, params) {
    int <- sprintf("%sd", code)
    str <- paste0("set_char_",
                  code,
                  paste(rep(" ", 5 - nchar(int)), collapse=""),
                  op$blocks$op.opcode$fileRaw,
                  "\n")
    cat(str)
}
for (i in 0:127) {
    assign(paste0("print_op_", i), print_set_char)
}

## set1
## set2
## set3
## set4
print_longchar_op <- function(op, code, params, put=FALSE) {
    if (put) {
        opname <- "put"
        offset <- 132
    } else {
        opname <- "set"
        offset <- 127
    }
    str <- paste0(opname,
                  code - offset,
                  "         ",
                  paste(op$blocks$op.opparams$fileRaw, collapse=" "),
                  "\n")
    cat(str)
}

print_set_longchar <-
    function(op, code, params) print_longchar_op(op, code, params)

for (i in 128:131) {
    assign(paste0("print_op_", i), print_set_longchar)
}

## set_rule
printRule <- function(op, code, params, opname) {
    a <- params$a
    b <- params$b
    str <- paste0(opname, "     ",
                  "a=", a, ", ",
                  "b=", b, "\n")
    cat(str)
}

print_op_132 <- function(op, code, params) {
    printRule(op, code, params, "set_rule")
}

## put1
## put2
## put3
## put4
print_put_longchar <-
    function(op, code, params) print_longchar_op(op, code, params, TRUE)

for (i in 133:136) {
    assign(paste0("print_op_", i), print_put_longchar)
}

## put_rule
print_op_137 <- function(op, code, params) {
    printRule(op, code, params, "put_rule")
}

## bop
print_op_139 <- function(op, code, params) {
    counters <- params$counters
    p <- params$p
    str <- paste0("bop          ",
                  "counters=", paste0(counters, collapse=" "), ", ",
                  "p=", p, "\n")
    cat(str)
}

## eop
print_op_140 <- function(op, code, params) {
    cat("eop\n")
}

## push
print_op_141 <- function(op, code, params) {
    cat("push\n")
}

## pop
print_op_142 <- function(op, code, params) {
    cat("pop\n")
}

## right<i>
printRight <- function(op, code, params, i) {
    b <- params[[1]]
    str <- paste0("right", i, "       ",
                  "b=", b, "\n")
    cat(str)
}

print_op_143 <- function(op, code, params) {
    printRight(op, code, params, 1)
}
print_op_144 <- function(op, code, params) {
    printRight(op, code, params, 2)
}
print_op_145 <- function(op, code, params) {
    printRight(op, code, params, 3)
}
print_op_146 <- function(op, code, params) {
    printRight(op, code, params, 4)
}

## w<i>
printW <- function(op, code, params, i) {
    str <- paste0("w", i)
    if (i > 0) {
        b <- params[[1]]
        str <- paste0(str, "           ",
                      "b=", b)
    }
    cat(str, "\n")
}

print_op_147 <- function(op, code, params) {
    printW(op, code, params, 0)
}
print_op_148 <- function(op, code, params) {
    printW(op, code, params, 1)
}
print_op_149 <- function(op, code, params) {
    printW(op, code, params, 2)
}
print_op_150 <- function(op, code, params) {
    printW(op, code, params, 3)
}
print_op_151 <- function(op, code, params) {
    printW(op, code, params, 4)
}

## x<i>
printX <- function(op, code, params, i) {
    str <- paste0("x", i)
    if (i > 0) {
        b <- params[[1]]
        str <- paste0(str, "           ",
                      "b=", b)
    }
    cat(str, "\n")
}

print_op_152 <- function(op, code, params) {
    printX(op, code, params, 0)
}
print_op_153 <- function(op, code, params) {
    printX(op, code, params, 1)
}
print_op_154 <- function(op, code, params) {
    printX(op, code, params, 2)
}
print_op_155 <- function(op, code, params) {
    printX(op, code, params, 3)
}
print_op_156 <- function(op, code, params) {
    printX(op, code, params, 4)
}

## down<i>
printDown <- function(op, code, params, i) {
    a <- params[[1]]
    str <- paste0("down", i, "        ",
                  "a=", a, "\n")
    cat(str)
}

print_op_157 <- function(op, code, params) {
    printDown(op, code, params, 1)
}
print_op_158 <- function(op, code, params) {
    printDown(op, code, params, 2)
}
print_op_159 <- function(op, code, params) {
    printDown(op, code, params, 3)
}
print_op_160 <- function(op, code, params) {
    printDown(op, code, params, 4)
}

## y<i>
printY <- function(op, code, params, i) {
    str <- paste0("y", i)
    if (i > 0) {
        a <- params[[1]]
        str <- paste0(str, "           ",
                      "a=", a)
    }
    cat(str, "\n")
}

print_op_161 <- function(op, code, params) {
    printY(op, code, params, 0)
}
print_op_162 <- function(op, code, params) {
    printY(op, code, params, 1)
}
print_op_163 <- function(op, code, params) {
    printY(op, code, params, 2)
}
print_op_164 <- function(op, code, params) {
    printY(op, code, params, 3)
}
print_op_165 <- function(op, code, params) {
    printY(op, code, params, 4)
}

## z<i>
printZ <- function(op, code, params, i) {
    str <- paste0("z", i)
    if (i > 0) {
        a <- params[[1]]
        str <- paste0(str, "           ",
                      "a=", a)
    }
    cat(str, "\n")
}

print_op_166 <- function(op, code, params) {
    printZ(op, code, params, 0)
}
print_op_167 <- function(op, code, params) {
    printZ(op, code, params, 1)
}
print_op_168 <- function(op, code, params) {
    printZ(op, code, params, 2)
}
print_op_169 <- function(op, code, params) {
    printZ(op, code, params, 3)
}
print_op_170 <- function(op, code, params) {
    printZ(op, code, params, 4)
}

## fnt_num_<i>
print_fnt_num <- function(op, code, params) {
    str <- paste0("fnt_num_",
                  code - 171,
                  "\n")
    cat(str)    
}
for (i in 171:234) {
    assign(paste0("print_op_", i), print_fnt_num)
}

## xxx<i> (specials)
print_xxx <- function(op, code, params) {
    str <- paste0("xxx",
                  code - 238,
                  "         ",
                  "k=", params$length, "\n",
                  "             ",
                  "x=",
                  paste(params$string, collapse=""),
                  "\n")
    cat(str)
}
for (i in 239:242) {
    assign(paste0("print_op_", i), print_xxx)
}

## font_def<i>
print_op_243 <- function(op, code, params) {
    fontnum <- params$k
    checksum <- params$c
    scale <- params$s
    design <- params$d
    fontname <- paste(params$fontname.name,
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
print_op_247 <- function(op, code, params) {
    i <- params$i
    num <- params$num
    den <- params$den
    mag <- params$mag
    comment <- params$comment.string
    str <- paste0("pre          ",
                  "version=", i, ", ",
                  "num=", num, ", ",
                  "den=", den, ", ",
                  "mag=", mag, ",\n             ",
                  "comment=", paste(comment, collapse=""), "\n")
    cat(str)
}

## post
print_op_248 <- function(op, code, params) {
    cat("post\n")
}

## post_post
print_op_249 <- function(op, code, params) {
    cat("post_post\n")
}

## XFontDef (XeTeX)
print_op_252 <- function(op, code, params) {
    fontnum <- params$fontnum
    ptsize <- params$ptsize
    fontname <-
        paste(params$fontinfo.marker.fontname.block,
              collapse="")
    fontindex <- params$fontinfo.marker.fontindex
    str <- paste0("x_fnt_def    ",
                  "fontnum=", fontnum, ", ",
                  "ptsize=", ptsize, "\n             ",
                  "fontname=", fontname, " [", fontindex, "]", "\n")
    cat(str)
}

## XGlyphArray (XeTeX) 
print_op_253 <- function(op, code, params) {
    n <- params$n
    xIndex <- 2*(1:n) - 1
    yIndex <- xIndex + 1
    glyphX <- unlist(lapply(xIndex,
                            function(i) {
                                name <- paste0("glyphs.xy", i)
                                params[[name]]
                            }))
    glyphY <- unlist(lapply(yIndex,
                            function(i) {
                                name <- paste0("glyphs.xy", i)
                                params[[name]]
                            }))
    str <- paste0("x_glyph      ",
                  "id=", params$glyphs.id, ", ",
                  "x=", glyphX, ", y=", glyphY, collapse="\n")
    cat(str, "\n")
}

## XStringGlyphArray (XeTeX)
print_op_254 <- function(op, code, params) {
    n <- params$n
    xIndex <- 2*(1:n) - 1
    yIndex <- xIndex + 1
    glyphX <- unlist(lapply(xIndex,
                            function(i) {
                                name <- paste0("glyphs.xy", i)
                                params[[name]]
                            }))
    glyphY <- unlist(lapply(yIndex,
                            function(i) {
                                name <- paste0("glyphs.xy", i)
                                params[[name]]
                            }))
    str <- paste0(paste0("x_glyph_str  '",
                         paste(params$text,
                               collapse=""),
                         "'\n"),
                  paste0("x_glyph      ",
                         "id=", params$glyphs.id,
                         ", ", "x=", glyphX, ", y=", glyphY, 
                         collapse="\n"),
                  collapse="\n")
    cat(str, "\n")
}

## upTeX
print_op_255 <- function(op, code, params) {
    dir <- params[[1]]
    str <- paste0("dir          ", dir, "\n")
    cat(str)
}

printDVI <- function(op, code, params) {
    base::get(paste0("print_op_", code))(op, code, params)
}

################################################################################
## User API

print.DVI <- function(x, ...) {
    codes <- opCodes(x)
    params <- opParams(x)
    invisible(mapply(printDVI, x, codes, params))
}
