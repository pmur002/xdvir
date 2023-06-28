
initLuaTeX <- function() {
    luatex <- Sys.which("luatex")
    if (nchar(luatex) == 0) {
        message("   luatex:  not found")
    } else {
        versText <- system("luatex --version", intern=TRUE)[1]
        version <- gsub(" .+", "", gsub("^[^0-9]+", "", versText))
        set("luaVersion", version)
        message(paste0("   luatex:  ", version))
        ## https://mirror.cse.unsw.edu.au/pub/CTAN/macros/luatex/generic/luaotfload/luaotfload-latex.pdf
        ## [page 4]
        ## "New in version 3.15"
        ## "Write glyph ids instead of internal identifiers to DVI files."
        loadToolText <- system("luaotfload-tool --version", intern=TRUE)
        loadToolVersLine <- grep("luaotfload-tool version", loadToolText)
        loadToolVers <- gsub('.+"', "",
                             gsub('"$', "", loadToolText[loadToolVersLine]))
        set("luaOTFloadToolVersion", loadToolVers)
    }
}

################################################################################
## Internal functions

## Find a font file from Lua DVI font def
findFontFile <- function(fontname) {
    result <- system(paste0("luaotfload-tool --find='", fontname, "'"),
                     intern=TRUE)
    gsub('[^"]+ "|"$', "", result[2])
}

## Luaotf cache (to avoid calling luaotfload-tool over and over again)
getLuaOTFcache <- function() {
    cache <- get("luaOTFcache")
    if (is.null(cache))
        set("luaOTFcache", list())
    cache
}

cacheLuaOTF <- function(fontname, fontfile) {
    cache <- getLuaOTFcache()
    cache[[fontname]] <- fontfile
    set("luaOTFcache", cache)
}

## We just retain the font name and discard the extra font information
## about font options (for now) because R graphics does not make use
## of that extra information (for now)
luaFontName <- function(fontname) {
    ## Allow for ...
    ##   filename="fontname:...;..."       = system font
    ##   filename="file:fontname:...;..."  = TeX font
    ##   filename="[fontname]:...;..."     = TeX font
    ##   filename="[fontfile.ttf]:...;..." = local font
    name <- gsub('^"(file:)?|:.+', "", fontname)
    if (grepl("^[[]", name)) {
        ## Check for a local font
        ## We only support fonts in the current working directory so far
        name <- gsub("^[[]|[]]$", "", name)
        if (grepl("[.]", name)) {
            fontfile <- list.files(pattern=name, ignore.case=TRUE)
        } else {
            fontfile <- list.files(pattern=paste0(name, "[.]"),
                                   ignore.case=TRUE)
        }
        if (length(fontfile)) {
            ## A local (rather than system) font has square brackets
            ## and a suffix
            suffix <- gsub(".+[.]", "", fontfile[1])
            ## Generate appropriate format for luaotfload-tool call
            ## This happens to work for different luaTeX versions
            ## because in one, name has no suffix and adding suffix works,
            ## while in other, name has suffix, but adding superfluous suffix
            ## is necessary (!)
            name <- paste0("file:", name, ".", suffix)
            ## Keep information needed for FontConfig
            attr(name, "dir") <- getwd()
        } else {
            ## Otherwise assume this is a TeX font
            ## (that luaotfload-tool will find)
            ## Remove square brackets and any file suffix
            name <- gsub("[[]|[]]|[.]ttf$|[.]otf$", "", name)
        }
    }
    name
}

## Glyph index from raw bytes
luaGlyphIndex <- function(raw) {
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

## Get glyph name of ith non-UNICODE glyph
getNonUnicodeGlyph <- function(code, fontfile, fontLib) {
    index <- as.numeric(as.hexmode(code))
    glyphNames <- fontLib$glyphNames(fontfile)
    notUNICODE <- grep("^glyph", glyphNames)
    ## Allow user to tweak magic offset if hard-coded tweak below is wrong
    magic <- getOption("xdvir.luatexMagicOffset")
    if (is.null(magic)) {
        ## The adjustment to glyph numbering is because,
        ## at some point, LuaTeX counted differently (by one)
        ## Choosing this to be at version 1.0.0 for now;
        ## May need refining ...
        if (compareVersion(get("luaVersion"), "1.0.0") < 0) {
            magic <- 1
        } else {
            magic <- 0
        }
    }
    glyphNames[notUNICODE[index - magic]]
}

## Glyph name from raw bytes
luaGlyphName <- function(raw, fontfile, dir, fontLib) {
    nbytes <- length(raw)
    code <- switch(nbytes,
                   ## Single byte is either set_char_i or set1 op
                   paste0("00", toupper(as.character(raw))),
                   ## Two bytes is assumed to be UTF16BE from set2 op
                   paste(toupper(as.character(raw)), collapse=""),
                   ## Three bytes is assumed to be UTF32BE from set3 op
                   if (as.numeric(raw[1]) >= 15) {
                       ## UNLESS first byte is 0x0F, then ...
                       ## Three bytes is assumed to be non-UNICODE char
                       ##   from set3 op
                       ## Second two bytes are integer index into
                       ##   non-UNICODE glyphs
                       paste(raw[2:3], collapse="")
                   } else {
                       paste(toupper(as.character(raw)), collapse="")
                   },
                   ## Have not yet witnessed set4 op
                   stop("set4 not yet supported"))
    ## Format code for cmap
    cmapCode <- tolower(gsub("^0*", "0x", code))
    ## May generate more than one option
    switch(nbytes,
           c(AdobeName(code),
             fontLib$glyphName(cmapCode, fontfile, 0)),
           c(AdobeName(code),
             paste0("uni", code),
             fontLib$glyphName(cmapCode, fontfile, 0)),
           ## Find non-UNICODE glyph name
           if (as.numeric(raw[1]) >= 15) {
               getNonUnicodeGlyph(code, fontfile, fontLib)
           } else {
               c(AdobeName(code),
                 paste0("uni", code),
                 paste0("u", gsub("^0", "", code)),
                 fontLib$glyphName(cmapCode, fontfile, 0))
           },
           stop("set4 not yet supported"))
}

luaGlyphChar <- function(raw) {
    nbytes <- length(raw)
    switch(nbytes,
           ## Single byte is either set_char_i or set1 op
           if (as.numeric(raw) < 128) {
               rawToChar(raw)
           } else {
               ## Single byte is assumed to be UTF16BE 
               ## (first byte assumed 0) from set1 op
               iconv(list(c(as.raw(0), raw)),
                     from="UTF16BE", to="UTF-8")
           },
           ## Two bytes is assumed to be UTF16BE from set2 op
           iconv(list(raw), from="UTF16BE", to="UTF-8"),
           ## Three bytes is assumed to be UTF32BE from set3 op
           if (as.numeric(raw[1]) >= 15) {
               ## UNLESS first byte is 0x0F, then ...
               ## Three bytes is assumed to be non-UNICODE char from set3 op
               ## Second two bytes are integer index into non-UNICODE glyphs
               NA
           } else {
               iconv(list(c(as.raw(0), raw)), from="UTF32BE", to="UTF-8")
           },
           ## Have not yet witnessed set4 op
           stop("set4 not yet supported"))
}

################################################################################

## Create font definition from Lua DVI font def
luaDefineFont <- function(fontname, fontLib) {
    version <- get("luaOTFloadToolVersion")
    if (as.numeric(version) >= 3.15) {
        fontfile <- gsub("[[]|[]].*", "", fontname)
        fontDef(file=fontfile,
                index=0,
                fontLib$fontFamily(fontfile),
                fontLib$fontWeight(fontfile),
                fontLib$fontStyle(fontfile),
                fontSize(fontfile))
    } else {
        ## Extract just font name from DVI fontname value
        fontFullName <- luaFontName(fontname)
        fontfile <- findFontFile(fontFullName)
        fontDef(file=fontfile,
                index=0,
                fontLib$fontFamily(fontfile),
                fontLib$fontWeight(fontfile),
                fontLib$fontStyle(fontfile),
                fontSize(fontFullName))
    }
}

## Get glyph info from raw bytes (and current font)
luaGetGlyph <- function(raw, font, dir, fontLib) {
    version <- get("luaOTFloadToolVersion")
    if (as.numeric(version) >= 3.15) {
        index <- luaGlyphIndex(raw)
        list(name="", index=index, char="")
    } else {
        glyphName <- luaGlyphName(raw, font$fontdef$file, dir, fontLib)
        index <- fontLib$glyphIndex(glyphName, font$fontdef$file)
        char <- luaGlyphChar(raw)
        list(name=glyphName, index=index, char=char)
    }
}

################################################################################
## User interface

## Ensure non-Type1 math font
lualatexPreamble <- "\\usepackage{unicode-math}"

luatexEngine <- function(packages=NULL) {
    TeXengine(command="lualatex",
              options="--output-format=dvi",
              preamble=lualatexPreamble,
              fontDef=luaDefineFont,
              getGlyph=luaGetGlyph)
}

lualatexEngine <- luatexEngine()

lualatexGrob <- function(tex,
                         x=0.5, y=0.5, default.units="npc",
                         hjust="centre", vjust="centre",
                         packages=NULL,
                         tinytex=getOption("xdvir.tinytex"),
                         fontLib=getOption("xdvir.fontLibrary")) {
    texDoc <- author(tex, engine=lualatexEngine, packages=packages)
    dviFile <- typeset(texDoc, engine=lualatexEngine, tinytex=tinytex)
    dvi <- readDVI(dviFile)
    dviGrob(dvi, 
            x=x, y=y, default.units=default.units,
            hjust=hjust, vjust=vjust,
            engine=lualatexEngine, package=packages,
            fontLib=fontLib)
}

grid.lualatex <- function(...) {
    grid.draw(lualatexGrob(...))
}

    
