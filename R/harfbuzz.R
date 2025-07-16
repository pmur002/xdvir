
## Different decoding of glyphs and font names for
## HarfBuzz renderer with LuaTeX

hbGlyphIndex <- function(raw, fontname, fontLib) {
    ## raw is UNICODE
    ## OR glyph index if non-unicode char
    nbytes <- length(raw)
    if (nbytes < 3) {
        code <- switch(nbytes,
                       ## Single byte from set_char_i or set1
                       as.integer(raw),
                       ## Two bytes from set2
                       sum(as.integer(raw)*16^c(2, 0)))
        fontLib$glyphIndex(code, fontname)
    } else if (nbytes == 3) {
        ## Three bytes from set3
        if (as.numeric(raw[1]) >= 18) {
            ## UNLESS first byte is 0x12, then ...
            ## Three bytes is assumed to be non-UNICODE char from set3 op
            ## Second two bytes are glyph index
            sum(as.integer(raw[2:3])*16^c(4, 2, 0))
        } else {
            code <- sum(as.integer(raw)*16^c(4, 2, 0))
            fontLib$glyphIndex(code, fontname)
        }
    } else {
        ## To be implemented
        ## Have not yet witnessed set4 op
        stop("set4 not yet supported")
    }
}

hbFontFile <- function(fontname) {
    filename <- gsub("[[]|[]].*", "", fontname)
    ## Font variations encoded after file name
    features <- gsub(".+]", "", fontname)
    if (nchar(features)) {
        if (grepl("[+]axis", features)) {
            axes <- strsplit(strsplit(gsub("[}].*", "",
                                           gsub(".+[{]", "", features)),
                                      ",")[[1]],
                             "=")
            names <- sapply(axes, function(x) x[1])
            values <- as.numeric(sapply(axes, function(x) x[2]))
            names(values) <- names
            attr(filename, "variations") <- glyphFontVariation(values)
        }
    }
    filename
}

