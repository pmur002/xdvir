
initXeTeX <- function() {
    xetex <- Sys.which("xelatex")
    if (nchar(xetex) > 0) {
        versText <- system("xelatex --version", intern=TRUE)
        set("xetexVersionStr", versText)
        if (length(versText)) {
            version <- versText[1]
        } else {
            version <- "unknown version"
        }
        set("xetexVersion", version)
    }
}

xetexVersion <- function() {
    get("xetexVersion")
}

xetexAvailable <- function() {
    !is.null(xetexVersion())
}

## Ensure non-Type1 math font
xelatexPreamble <- "\\usepackage{unicode-math}"

isXeTeX <- function(dvi) {
    commentStr <- commentString(dvi)
    grepl(xetexVersion(), commentStr)
}

xeGlyphIndex <- function(raw) {
    glyphIndex(raw)
}

xeFontFile <- function(fontname) {
    gsub("[[]|[]].*", "", fontname)
}

xelatexGrob <- function(tex, ...) {
    if (!xetexAvailable())
        stop("XeTeX not available")
    latexGrob(tex, engine=getEngine("xetex"), ...)
}

grid.xelatex <- function(...) {
    grid.draw(xelatexGrob(...))
}
