
## A DVI file consists of operations
## Each operation starts with a one-byte opcode

opcode <- atomicBlock("int", size=1, signed=FALSE)

int1 <- integer1
int2 <- atomicBlock("int", size=2, endian="big")
int3 <- integer3(endian="big")
int4 <- atomicBlock("int", size=4, endian="big")

uint1 <- atomicBlock("int", size=1, signed=FALSE)
uint2 <- atomicBlock("int", size=2, endian="big", signed=FALSE)

################################################################################
## operation parameters

## set_char_<i>
for (i in 0:127) {
    assign(paste0("op", i), NULL)
}

## set<i>
op128 <- uint1
op129 <- uint2
op130 <- int3
op131 <- int4
    
## set_rule
op132 <- mixedBlock(a=int4,
                    b=int4)

## put<i>
op133 <- uint1
op134 <- uint2
op135 <- int3
op136 <- int4

## put_rule
op137 <- mixedBlock(a=int4,
                    b=int4)

## nop
op138 <- NULL

## bop
op139 <- mixedBlock(counters=vectorBlock(int4, 10),
                    p=int4)

## eop
op140 <- NULL

## push
op141 <- NULL

## pop
op142 <- NULL

## right<i>
op143 <- int1
op144 <- int2
op145 <- int3
op146 <- int4

## w<i>
op147 <- NULL
op148 <- int1
op149 <- int2
op150 <- int3
op151 <- int4

## x<i>
op152 <- NULL
op153 <- int1
op154 <- int2
op155 <- int3
op156 <- int4

## down<i>
op157 <- int1
op158 <- int2
op159 <- int3
op160 <- int4

## y<i>
op161 <- NULL
op162 <- int1
op163 <- int2
op164 <- int3
op165 <- int4

## z<i>
op166 <- NULL
op167 <- int1
op168 <- int2
op169 <- int3
op170 <- int4

## fnt_num_<i>
for (i in 171:234) {
    assign(paste0("op", i), NULL)
}

## fnt<i>
op235 <- uint1
op236 <- uint2
op237 <- int3
op238 <- int4
    
## xxx<i> (specials)
op239 <- lengthBlock(uint1, ASCIIchar, blockLabel="string")
op240 <- lengthBlock(uint2, ASCIIchar, blockLabel="string")
op241 <- lengthBlock(int3, ASCIIchar, blockLabel="string")
op242 <- lengthBlock(int4, ASCIIchar, blockLabel="string")

## font_def<i>
font_def <- function(i) {
    k <- switch(i, int1, int2, int3, int4)
    mixedBlock(k=k,
               c=int4,
               s=int4,
               d=int4,
               fontname=markedBlock(mixedBlock(a=uint1, l=uint1),
                                    function(marker) {
                                        vectorBlock(ASCIIchar,
                                                    blockValue(marker$a) +
                                                    blockValue(marker$l))
                                    },
                                    markerLabel="length",
                                    blockLabel="name"))
}
op243 <- font_def(1)
op244 <- font_def(2)
op245 <- font_def(3)
op246 <- font_def(4)

## pre
op247 <- mixedBlock(i=uint1,
                    num=int4,
                    den=int4,
                    mag=int4,
                    comment=lengthBlock(uint1, ASCIIchar, blockLabel="string"))

## post
op248 <- mixedBlock(p=int4,
                    num=int4,
                    den=int4,
                    mag=int4,
                    l=int4,
                    u=int4,
                    s=uint2,
                    t=uint2)

## post_post
op249 <- mixedBlock(q=int4,
                    i=uint1,
                    ## There could be up to 7 of these to pad the file
                    ## to a multiple of four bytes, but I will ignore
                    ## that for now
                    sig=vectorBlock(ASCIIchar, 4))

## XeTeX
## x_fnt_def
fontInfoMarker <- mixedBlock(flags=uint2,
                             fontname=lengthBlock(uint1, ASCIIchar),
                             fontindex=int4)
xeReadFontInfo <- function(marker) {
    if (!blockValue(marker$flags))
        NULL
    else {
        blocks <- NULL
        flags <- marker$flags
        if (bitwAnd(0x0200, flags))
            blocks <- c(blocks, colour=int4)
        if (bitwAnd(0x1000, flags))
            blocks <- c(blocks, extend=int4)
        if (bitwAnd(0x2000, flags))
            blocks <- c(blocks, slant=int4)
        if (bitwAnd(0x4000, flags))
            blocks <- c(blocks, bold=int4)
        if (bitwAnd(0x0800, flags))
            blocks <- c(blocks, variations=lengthBlock(int2, int4))
        do.call(mixedBlock, blocks)
    }
}

op252 <- mixedBlock(fontnum=int4,
                    ptsize=int4,
                    fontinfo=markedBlock(fontInfoMarker, xeReadFontInfo))

## XeTeX
## x_glyph
op253 <- mixedBlock(w=int4,
                    markedBlock(uint2,
                                function(marker) {
                                    n <- blockValue(marker)
                                    mixedBlock(xy=vectorBlock(mixedBlock(int4,
                                                                         int4),
                                                              n),
                                               id=vectorBlock(uint2, n))
                                },
                                markerLabel="n",
                                blockLabel="glyphs"))

## XeTeX
## x_string
op254 <- mixedBlock(markedBlock(uint2,
                                function(marker) {
                                    n <- blockValue(marker)
                                    vectorBlock(ASCIIchar, 2*n)
                                },
                                markerLabel="nchar",
                                blockLabel="text"),
                    w=int4,
                    markedBlock(uint2,
                                function(marker) {
                                    n <- blockValue(marker)
                                    mixedBlock(xy=vectorBlock(mixedBlock(int4,
                                                                         int4),
                                                              n),
                                               id=vectorBlock(uint2, n))
                                },
                                markerLabel="n",
                                blockLabel="glyphs"))

## upTeX
## dir
op255 <- uint1

opparams <- function(marker) {
    opcode <- blockValue(marker)
    base::get(paste0("op", opcode))
}

opBlock <- markedBlock(marker=opcode,
                       switch=opparams,
                       markerLabel="opcode",
                       blockLabel="opparams")

opFormat <- memFormat(op=opBlock)

