
## Null TeX engine that CANNOT be used for typesetting
nullEngine <- TeXengine(name="null",
                        version=packageVersion("xdvir"),
                        command=NULL,
                        isEngine=function(dvi) FALSE,
                        glyphIndex=function(raw) glyphIndex(raw),
                        fontFile=function(fontname)
                            gsub("[[]|[]].*", "", fontname))
registerEngine(nullEngine)

.onLoad <- function(libname, pkgname) {
    options(xdvir.engine=nullEngine)
    ## Init {tinytex}
    initTinyTeX()
    ## Define and register XeTeX engine 
    initXeTeX()
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
    ## Define and register LuaTeX engine
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
        options(xdvir.engine=LuaTeXengine)
    }
    ## Define and register packages
    registerPackage(fontspecPackage())
    registerPackage(xcolorPackage())
    registerPackage(previewPackage())
    registerPackage(tikzPackage())
    registerPackage(tikzPicture())
    registerPackage(zrefPackage())
    ## Default Font Library
    options(xdvir.fontLib=FTfontLibrary)
    options(xdvir.quiet=TRUE)

    ## For ggplot2 integration
    run_on_load()
}

.onAttach <- function(libname, pkgname) {
    msg <- NULL
    width <- 15
    line <- function(prompt="", x="") {
        sprintf(paste0("%", width, "s:  %s"), prompt, x)
    }
    if (haveTeX()) {
        msg <- c(msg,
                 line("TeX", texVersion()))
        if (xetexAvailable()) {
            msg <- c(msg,
                     line("xetex", xetexVersion()))
        } else {
            msg <- c(msg,
                     line("xetex", "Not found"),
                     line("", "The XeTeX engine is NOT available."))
        }
        if (luatexAvailable()) {
            msg <- c(msg,
                     line("luatex", luatexVersion()))
            if (luaOTFloadToolAvailable()) {
                msg <- c(msg,
                         line("luaotfload-tool", luaOTFloadToolVersion()))
                if (!luaOTFloadToolSufficient()) {
                    msg <- c(msg,
                             line("",
                                  "The luaotfload-tool version is too low (< 3.15)."))
                    msg <- c(msg,
                             line("",
                                  "The LuaTeX engine is NOT available."))
                }
            } 
        } else {
            msg <- c(msg,
                     line("luatex", "Not found."),
                     line("", "The LuaTeX engine is NOT available."))
        }
    } else {
        msg <- c(msg,
                 line("TeX", "Not found."),
                 line("",
                      "No TeX installation detected (see ?tinytex::install_tinytex)."))
    }
    if (!(haveTeX() &&
          any(sapply(get("engines"), canTypeset)))) {
        msg <- c(msg,
                 line("", "Typesetting is NOT available."))
    }
    packageStartupMessage(paste(msg, collapse="\n"))
}

