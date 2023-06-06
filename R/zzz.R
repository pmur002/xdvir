
.onLoad <- function(libname, pkgname) {
    ## Dependencies
    initTeX()
    initTTX()
    initFontForge()
    ## Suggestions
    initTinyTeX()
    ## Engines
    initLuaTeX()
    initXeTeX()
    initUpTeX()
}

