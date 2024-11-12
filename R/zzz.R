
## Null TeX engine that CANNOT be used for typesetting
nullEngine <- TeXengine(name="null",
                        version=packageVersion("xdvir"),
                        command=NULL,
                        isEngine=function(dvi) FALSE)
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
                                  options="--output-format=dvi",
                                  preamble=lualatexPreamble)
        registerEngine(LuaTeXengine)
        options(xdvir.engine=LuaTeXengine)
    }
    ## Define and register packages
    registerPackage(fontspecPackage())
    registerPackage(xcolorPackage())
    registerPackage(previewPackage())
    ## Default Font Library
    options(xdvir.fontLib=FTfontLibrary)
    options(xdvir.quiet=TRUE)
}

.onAttach <- function(libname, pkgname) {
    if (haveTeX()) {
        packageStartupMessage(paste0("            TeX:  ", texVersion()))
        if (xetexAvailable()) {
            packageStartupMessage(paste0("          xetex:  ", xetexVersion()))
        } else {
            packageStartupMessage("          xetex:  not found")
            packageStartupMessage(paste0("               :  ",
                                         "The XeTeX engine is NOT available."))
        }
        if (luatexAvailable()) {
            packageStartupMessage(paste0("         luatex:  ", luatexVersion()))
            if (luaOTFloadToolAvailable()) {
                packageStartupMessage(paste0("luaotfload-tool:  ",
                                             luaOTFloadToolVersion()))
                if (!luaOTFloadToolSufficient()) {
                    packageStartupMessage(paste0("               :  ",
                                                 "luaotfload-tool version ",
                                                 "is too low (< 3.15)")) 
                    packageStartupMessage(paste0("               :  ",
                                                 "The LuaTeX engine is ",
                                                 "NOT available."))
                }
            } 
        } else {
            packageStartupMessage("         luatex:  not found")
            packageStartupMessage(paste0("               :  ",
                                         "The LuaTeX engine is NOT available."))
        }
    } else {
        packageStartupMessage("            TeX:  not found")
        packageStartupMessage(paste0("               :  ",
                                     "No TeX installation detected",
                                     " (see ?tinytex::install_tinytex)"))
    }
    if (!(haveTeX() &&
          any(sapply(get("engines"), canTypeset)))) {
        packageStartupMessage(paste0("         :  ",
                                     "Typesetting is NOT available."))
    }
}

