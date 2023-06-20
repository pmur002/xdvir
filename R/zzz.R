
.onLoad <- function(libname, pkgname) {
    ## Font libraries
    initFontLib()
    ## TeX
    initTeX()
    initLuaTeX()
    initXeTeX()
    initUpTeX()
    initTinyTeX()
}

