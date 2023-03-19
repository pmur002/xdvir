
initUpTeX <- function() {
}

################################################################################
## Internal functions

vfRealFont <- function(op) {
    opcode <- blockValue(op$blocks$op.opcode)
    if (opcode == 243) {
        paste(blockValue(op$blocks$op.opparams.fontname.name), collapse="")
    } else {
        NULL
    }
}

## Return first real font 
getRealFontFromVF <- function(vf) {
    fonts <- lapply(vf, vfRealFont)
    fonts[!sapply(fonts, is.null)][[1]]
}

mapFontName <- function(fontname) {
    pdftexMap <- get("pdftexMap")
    pdftexMatch <- grep(fontname, pdftexMap)
    if (length(pdftexMatch)) {
        ## Work with rows of pdftex.map
    } else {
        kanjixMap <- get("kanjixMap")
        kanjixMatch <- grep(fontname, kanjixMap)
        if (length(kanjixMatch)) {
            ## Work with rows of kanjix.map
            strsplit(kanjixMap[kanjixMatch], " ")[[1]][3]
        } else {
            character(0)
        }
    }
}

findMappedFile <- function(fontname) {
    ## If name includes suffix, e.g., .ttf, just search for that font
    ## with kpsewhich
    
    ## Otherwise, try fc-match, which WILL find a match, even if it is terrible
    if (grep("^!", fontname)) {
        ## ! in front of name indicates that font will NOT be embedded
        ## Remove that before attempting match
        fontname <- gsub("^!", "", fontname)
    }
    system(paste0("fc-match -f %{file} ", fontname), intern=TRUE)
}

upGlyphName <- function(raw, fontfile) {
    nbytes <- length(raw)
    code <- switch(nbytes,
                   ## Single byte is either set_char_i or set1 op
                   stop("set_char_i and set1 not yet supported"),
                   ## Two bytes is assumed to be UTF16BE from set2 op
                   paste(toupper(as.character(raw)), collapse=""),
                   stop("set3 not yet supported"),
                   stop("set4 not yet supported"))
    ## Format code for cmap
    cmapCode <- tolower(gsub("^0*", "0x", code))
    ## May generate more than one option
    switch(nbytes,
           c(AdobeName(code),
             getGlyphNameFromUNICODE(cmapCode, fontfile)),
           c(AdobeName(code),
             paste0("uni", code),
             getGlyphNameFromUNICODE(cmapCode, fontfile)),
           ## Find non-UNICODE glyph name
           c(AdobeName(code),
             paste0("uni", code),
             paste0("u", gsub("^0", "", code)),
             getGlyphNameFromUNICODE(cmapCode, fontfile)),
           stop("set4 not yet supported"))
}

upGlyphChar <- function(raw) {
    nbytes <- length(raw)
    switch(nbytes,
           ## Single byte is either set_char_i or set1 op
           stop("set_char_i and set1 not yet supported"),
           ## Two bytes is assumed to be UTF16BE from set2 op
           iconv(list(raw), from="UTF16BE", to="UTF-8"),
           stop("set3 not yet supported"),
           stop("set4 not yet supported"))
}

################################################################################

## Create font definition from upTeX DVI font def
upDefineFont <- function(fontname) {
    mappedFont <- mapFontName(fontname)
    if (length(mappedFont)) {
        ## Work with mapped file
    } else {
        ## If no match, try .vf (virtual font)
        file <- suppressWarnings(system(paste0("kpsewhich ", fontname, ".vf"),
                                        intern=TRUE))
        if (length(file)) {
            ## Work with .vf file
            vf <- readVF(file)
            ## Get real font name
            ## (assume for now that there is only one)
            realFont <- getRealFontFromVF(vf)
            mappedFont <- mapFontName(realFont)
            if (length(mappedFont)) {
                ## Work with mapped file
                fontfile <- findMappedFile(mappedFont)
            } else {
                ## Give up (for now)
                stop("Font not found")
            }
        } else {
            ## Give up (for now)
            stop("Font not found")
        }
    }
    fontDef(file=fontfile,
            index=0,
            getFontFamily(fontfile),
            getFontWeight(fontfile),
            getFontStyle(fontfile))
}

## Get glyph info from raw bytes (and current font)
upGetGlyph <- function(raw, font) {
    ## Assume only Japanese set2 ops for now
    glyphName <- upGlyphName(raw, font$fontdef$file)
    index <- getGlyphIndex(glyphName, font$fontdef$file)
    char <- upGlyphChar(raw)
    list(name=glyphName, index=index, char=char)
}

################################################################################
## User interface

uptexEngine <- function(engine="uplatex", 
                        fontDef=upDefineFont,
                        getGlyph=upGetGlyph) {
    TeXengine(engine, options, fontDef, getGlyph)
}

uplatexEngine <- uptexEngine()
