
initXeTeX <- function() {
    xetex <- Sys.which("xetex")
    if (nchar(xetex) == 0)
        stop("Failed to find xetex; please install TeX (e.g., TeX Live)")
    versText <- system("xetex --version", intern=TRUE)
    versLine <- grep("^XeTeX", versText)
    version <- gsub("XeTeX | [(].+", "", versText[versLine])
    set("xeVersion", version)
}


################################################################################
## Internal functions

## XeTeX should generate x_fnt_def instead of fnt_def
xeDefineFont <- function(fontname) {
    stop("fnt_def op in .xdv file!")
}

## XeTeX should generate x_glyph (or x_string) instead of set_char
xeGetGlyph <- function(raw, font, dir) {
    glyphName <- luaGlyphName(raw, font$fontdef$file)
    index <- getGlyphIndex(glyphName, font$fontdef$file)
    char <- luaGlyphChar(raw)
    list(name=glyphName, index=index, char=char)
}

################################################################################
## User interface

xetexEngine <- function(engine="xelatex", options="--no-pdf",
                        fontDef=xeDefineFont,
                        getGlyph=xeGetGlyph) {
    TeXengine(engine, options, fontDef, getGlyph)
}

xelatexEngine <- xetexEngine()


