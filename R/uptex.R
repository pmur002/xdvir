
initUpTeX <- function() {
    uptex <- Sys.which("uptex")
    if (nchar(uptex) == 0)
        stop("Failed to find uptex; please install TeX (e.g., TeX Live)")
    versText <- system("uptex --version", intern=TRUE)[1]
    versLine <- grep("^upTeX", versText)
    version <- gsub("upTeX | [(].+", "", versText[versLine])
    set("upVersion", version)
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
    
    ## Otherwise, try fontMap
    if (grep("^!", fontname)) {
        ## ! in front of name indicates that font will NOT be embedded
        ## Remove that before attempting match
        fontname <- gsub("^!", "", fontname)
    }
    map <- get("fontMap")
    if (fontname %in% names(map)) {
        mappedFont <- map[fontname]
        match_font(mappedFont)$path
    } else {
        ## Give up (for now)
        stop("Font not found")
    }
}

upGlyphName <- function(raw, fontfile, dir) {
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
             getGlyphNameFromUNICODE(cmapCode, fontfile, dir)),
           c(AdobeName(code),
             paste0("uni", code),
             getGlyphNameFromUNICODE(cmapCode, fontfile, dir)),
           ## Find non-UNICODE glyph name
           c(AdobeName(code),
             paste0("uni", code),
             paste0("u", gsub("^0", "", code)),
             getGlyphNameFromUNICODE(cmapCode, fontfile, dir)),
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
            getFontStyle(fontfile),
            fontSize(fontname))
}

## Get glyph info from raw bytes (and current font)
upGetGlyph <- function(raw, font, dir) {
    ## Assume only Japanese set2 ops for now
    glyphName <- upGlyphName(raw, font$fontdef$file, dir)
    index <- getGlyphIndex(glyphName, font$fontdef$file)
    char <- upGlyphChar(raw)
    list(name=glyphName, index=index, char=char)
}

################################################################################
## User interface

uptexEngine <- function(packages=NULL) {
    TeXengine(command="uplatex",
              fontDef=upDefineFont,
              getGlyph=upGetGlyph)
}

uplatexEngine <- uptexEngine()
