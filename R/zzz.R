
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
    ## Define and register packages
    registerPackage(fontspecPackage())
    registerPackage(xcolorPackage())
    registerPackage(previewPackage())
    ## Default Font Library
    options(xdvir.fontLib=FTfontLibrary)
    options(xdvir.quiet=TRUE)
}

haveTeX <- function() {
    ## Either TinyTeX or another TeX installation that {tinytex} should
    ## be able to see, like TeX-Live
    nchar(tinytex_root(error=FALSE)) ||
        nchar(Sys.which("latex"))
}

.onAttach <- function(libname, pkgname) {
    if (!(tinytexAvailable() && haveTeX())) {
        packageStartupMessage(paste0("         :",
                                     "  No TeX installation detected",
                                     " (see ?tinytex::install_tinytex)"))
    }
    if (xetexAvailable()) {
        packageStartupMessage(paste0("    xetex:  ", xetexVersion()))
    } else {
        packageStartupMessage("    xetex:  not found")
        packageStartupMessage(paste0("         :",
                                     "  The XeTeX engine is NOT available."))
    }
    if (!(tinytexAvailable() &&
          haveTeX() &&
          any(sapply(get("engines"), canTypeset)))) {
        packageStartupMessage(paste0("         :",
                                     "  Typesetting is NOT available."))
    }
}

