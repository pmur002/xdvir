
.onLoad <- function(libname, pkgname) {
    ## Dependencies
    initTeX()
    ## Suggestions
    initFontLib()
    initTinyTeX()
    ## Engines
    initLuaTeX()
    initXeTeX()
    initUpTeX()
}

