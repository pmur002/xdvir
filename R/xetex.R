
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

xelatexPreamble <- function(packages) {
    preamble <- "\\usepackage{unicode-math}"
    if (!is.null(packages)) {
        preamble <- c(preamble, packagePreamble(packages))
    }
    preamble
}

xelatexPrefix <- function(packages) {
    prefix <- NULL
    if (!is.null(packages)) {
        prefix <- c(prefix, packagePrefix(packages))
    }
    prefix
}

xelatexSuffix <- function(packages) {
    suffix <- NULL
    if (!is.null(packages)) {
        suffix <- c(suffix, packageSuffix(packages))
    }
    suffix
}

xetexEngine <- function(packages=NULL) {
    if (inherits(packages, "xdvirPackage"))
        packages <- packageList(packages)
    if (!is.null(packages) && !inherits(packages, "xdvirPackageList"))
        stop("Invalid packages")
    TeXengine(command="xelatex --no-pdf",
              fontDef=xeDefineFont,
              getGlyph=xeGetGlyph,
              preamble=xelatexPreamble(packages),
              prefix=xelatexPrefix(packages),
              suffix=xelatexSuffix(packages),
              dviSuffix=".xdv")
}

xelatexEngine <- xetexEngine()

xelatexGrob <- function(tex) {
    texDoc <- author(tex, engine=xelatexEngine)
    dviFile <- typeset(texDoc, engine=xelatexEngine)
    dvi <- readDVI(dviFile)
    dviGrob(dvi, engine=xelatexEngine)
}

grid.xelatex <- function(...) {
    grid.draw(xelatexGrob(...))
}

    
