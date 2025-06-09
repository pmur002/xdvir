
initLuaTeX <- function() {
    luatex <- Sys.which("luatex")
    if (nchar(luatex) > 0) {
        versText <- system("luatex --version", intern=TRUE)
        set("luatexVersionStr", versText)
        if (length(versText)) {
            version <- versText[1]
        } else {
            version <- "unknown version"
        }
        set("luatexVersion", version)
    }
    luaotfloadtool <- Sys.which("luaotfload-tool")
    if (nchar(luaotfloadtool) > 0) {
        if (nchar(Sys.getenv("TERM"))) {
            TERM <- ""
        } else {
            if (.Platform$OS.type == "windows") {
                Sys.setenv("TERM"="dumb")
                TERM <- ""
            } else {
                TERM <- "TERM=dumb"
            }
        }
        loadToolText <- system(paste(TERM, "luaotfload-tool --version"),
                               intern=TRUE)
        set("luaOTFloadToolVersionStr", loadToolText)
        loadToolVersLine <- grep("luaotfload-tool version", loadToolText)
        if (length(loadToolVersLine)) {
            loadToolVers <- gsub('.+"', "",
                                 gsub('"$', "", loadToolText[loadToolVersLine]))
        } else {
            loadToolVers <- NA
        }
        set("luaOTFloadToolVersion", as.numeric(loadToolVers))
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
    luaOTFloadToolAvailable() &&
        !is.na(luaOTFloadToolVersion()) &&
        luaOTFloadToolVersion() >= 3.15
}

## Ensure non-Type1 math font
lualatexPreamble <- "\\usepackage{unicode-math}"

isLuaTeX <- function(dvi) {
    commentStr <- commentString(dvi)
    grepl(luatexVersion(), commentStr)
}

luaGlyphIndex <- function(raw, fontname, fontLib) {
    glyphIndex(raw, fontname, fontLib)
}

luaFontFile <- function(fontname) {
    gsub("[[]|[]].*", "", fontname)
}

lualatexGrob <- function(tex, harfbuzz=FALSE, ...) {
    if (!luatexAvailable())
        stop("LuaTeX not available")
    if (harfbuzz) {
        latexGrob(tex, engine=getEngine("luahbtex"), ...)
    } else {
        latexGrob(tex, engine=getEngine("luatex"), ...)
    }
}

grid.lualatex <- function(...) {
    grid.draw(lualatexGrob(...))
}
