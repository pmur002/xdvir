
## Null TeX engine that CANNOT be used for typesetting
nullEngine <- TeXengine(name="null",
                        version=packageVersion("xdvir"),
                        command=NULL,
                        isEngine=function(dvi) FALSE,
                        glyphIndex=function(raw, fontname)
                            glyphIndex(raw, fontname),
                        fontFile=function(fontname)
                            gsub("[[]|[]].*", "", fontname))
registerEngine(nullEngine)

.onLoad <- function(libname, pkgname) {
    options(xdvir.engine=nullEngine)
    ## Init {tinytex}
    initTinyTeX()
    ## Define and register XeTeX engine 
    initXeTeX()
    ## XeTeX wants tikz pgf driver path quoted
    tikzQuote <- TRUE
    if (xetexAvailable()) {
        XeTeXengine <- TeXengine(name="XeTeX",
                                 version=xetexVersion(),
                                 command="xelatex",
                                 isEngine=isXeTeX,
                                 glyphIndex=xeGlyphIndex,
                                 fontFile=xeFontFile,
                                 options="--no-pdf",
                                 preamble=xelatexPreamble,
                                 dviSuffix=".xdv")   
        registerEngine(XeTeXengine)
        options(xdvir.engine=XeTeXengine)
    }
    ## Define and register LuaTeX engine and LuaHBTeX engine
    initLuaTeX()
    if (luatexAvailable() && luaOTFloadToolSufficient()) {
        LuaTeXengine <- TeXengine(name="LuaTeX",
                                  version=luatexVersion(),
                                  command="lualatex",
                                  isEngine=isLuaTeX,
                                  glyphIndex=luaGlyphIndex,
                                  fontFile=luaFontFile,
                                  options="--output-format=dvi",
                                  preamble=lualatexPreamble)
        registerEngine(LuaTeXengine)
        LuaHBTeXengine <- TeXengine(name="LuaHBTeX",
                                    version=luatexVersion(),
                                    command="lualatex",
                                    isEngine=isLuaTeX,
                                    glyphIndex=hbGlyphIndex,
                                    fontFile=hbFontFile,
                                    options="--output-format=dvi",
                                    preamble=lualatexPreamble)
        registerEngine(LuaHBTeXengine)
        options(xdvir.engine=LuaTeXengine)
        ## LuaTeX does NOT want tikz pgf driver path quoted
        tikzQuote <- FALSE
    }
    ## Define and register packages
    registerPackage(fontspecPackage())
    registerPackage(xcolorPackage())
    registerPackage(previewPackage())
    registerPackage(tikzPackage(quote=tikzQuote))
    registerPackage(tikzPicture(quote=tikzQuote))
    registerPackage(zrefPackage())
    ## Default Font Library
    options(xdvir.fontLib=FTfontLibrary)
    options(xdvir.quiet=TRUE)
    ## Use DVI cache by default
    options(xdvir.useDVIcache=TRUE)

    ## For ggplot2 integration
    run_on_load()
}

.onAttach <- function(libname, pkgname) {
    msg <- NULL
    width <- 3
    line <- function(prompt="", x="") {
        sprintf(paste0("%", width, "s:  %s"), prompt, x)
    }
    if (!haveTeX()) {
        msg <- c(msg,
                 line("TeX", "Not found."),
                 line("",
                      "No TeX installation detected (see ?tinytex::install_tinytex)."))
        if (!(any(sapply(get("engines"), canTypeset)))) {
            msg <- c(msg,
                     line("", "Typesetting is NOT available."))
        }
        packageStartupMessage(paste(msg, collapse="\n"))
    }
}

