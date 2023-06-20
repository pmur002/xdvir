
initXeTeX <- function() {
    xetex <- Sys.which("xetex")
    if (nchar(xetex) == 0) {
        message("xetex:  not found")
    } else {
        versText <- system("xetex --version", intern=TRUE)
        versLine <- grep("^XeTeX", versText)
        version <- gsub("XeTeX | [(].+", "", versText[versLine])
        message(paste0("xetex:  ", version))
        set("xeVersion", version)
    }
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

## Ensure non-Type1 math font
xelatexPreamble <- "\\usepackage{unicode-math}"

xetexEngine <- function() {
    TeXengine(command="xelatex",
              options="--no-pdf",
              preamble=xelatexPreamble,
              fontDef=xeDefineFont,
              getGlyph=xeGetGlyph,
              dviSuffix=".xdv")
}

xelatexEngine <- xetexEngine()

xelatexGrob <- function(tex, 
                        x=0.5, y=0.5, default.units="npc",
                        hjust="centre", vjust="centre",
                        packages=NULL,
                        tinytex=getOption("xdvir.tinytex"),
                        fontLib=getOption("xdvir.fontLibrary")) {
    texDoc <- author(tex, engine=xelatexEngine, packages=packages)
    dviFile <- typeset(texDoc, engine=xelatexEngine, tinytex=tinytex)
    dvi <- readDVI(dviFile)
    dviGrob(dvi,
            x=x, y=y, default.units=default.units,
            hjust=hjust, vjust=vjust,
            engine=xelatexEngine, package=packages,
            fontLib=fontLib)
}

grid.xelatex <- function(...) {
    grid.draw(xelatexGrob(...))
}

    
