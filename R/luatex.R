
initLuaTeX <- function() {
    luatex <- Sys.which("luatex")
    if (nchar(luatex) > 0) {
        versText <- system("luatex --version", intern=TRUE)[1]
        version <- gsub(" .+", "", gsub("^[^0-9]+", "", versText))
        set("luatexVersion", version)
    }
    luaotfloadtool <- Sys.which("luaotfload-tool")
    if (nchar(luaotfloadtool) > 0) {
        if (nchar(Sys.getenv("TERM"))) {
            TERM <- ""
        } else {
            TERM <- "TERM=dumb"
        }
        loadToolText <- system(paste(TERM, "luaotfload-tool --version"),
                               intern=TRUE)
        loadToolVersLine <- grep("luaotfload-tool version", loadToolText)
        loadToolVers <- gsub('.+"', "",
                             gsub('"$', "", loadToolText[loadToolVersLine]))
        set("luaOTFloadToolVersion", loadToolVers)
    }
}

luatexVersion <- function() {
    get("luatexVersion")
}

luatexAvailable <- function() {
    !is.null(luatexVersion())
}

luaOTFloadToolVersion <- function() {
    get("luaOTFloadToolVersion")
}

luaOTFloadToolAvailable <- function() {
    !is.null(luaOTFloadToolVersion())
}

luaOTFloadToolSufficient <- function() {
    ## https://mirror.cse.unsw.edu.au/pub/CTAN/macros/luatex/generic/luaotfload/luaotfload-latex.pdf
    ## [page 4]
    ## "New in version 3.15"
    ## "Write glyph ids instead of internal identifiers to DVI files."
    ## Released on Sep 3 2020
    ## https://github.com/latex3/luaotfload/commit/4c09fe264c1644792d95182280be259449e7da02
    luaOTFloadToolAvailable() && as.numeric(luaOTFloadToolVersion()) >= 3.15
}

## Ensure non-Type1 math font
lualatexPreamble <- "\\usepackage{unicode-math}"

isLuaTeX <- function(dvi) {
    commentStr <- commentString(dvi)
    grepl("LuaTeX", commentStr)
}

luaGlyphIndex <- function(raw) {
    glyphIndex(raw)
}

luaFontFile <- function(fontname) {
    gsub("[[]|[]].*", "", fontname)
}

lualatexGrob <- function(tex, ...) {
    if (!luatexAvailable())
        stop("LuaTeX not available")
    latexGrob(tex, engine=getEngine("luatex"), ...)
}

grid.lualatex <- function(...) {
    grid.draw(lualatexGrob(...))
}
