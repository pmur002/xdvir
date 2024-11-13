
library(grid)
library(xdvir)

## Make debugging information available
options(tinytex.verbose=TRUE, xdvir.quiet=FALSE)

## Existing DVI (so engine unknown AND packages unknown)
dviXeTeX <- readDVI(system.file("DVI", "test-xetex.xdv", package="xdvir"))

## Font file paths based on my machine
if (Sys.getenv("USER") == "pmur002") {
    
    ## Fall back to dummy fontLib
    ## (glyph positioning is compromised)
    grid.newpage()
    tools::assertWarning(grid.dvi(dviXeTeX))

    grid.newpage()
    ## Warn about guessing DVI engine
    tools::assertWarning(grid.dvi(dviXeTeX))
    
}

## Generate DVI

if (nchar(Sys.getenv("GITHUB_RUN_ID"))) {
    ## For testing on github Windows runners, avoid tmp dir
    ## for files that a TeX engine will run on
    texFile <- "test-dvi.tex"
} else {
    texFile <- NULL
}

if (xdvir:::haveTeX()) {

    tex <- author("This is a test: $x - \\mu$")
    dviFile <- typeset(tex, texFile=texFile)
    dvi <- readDVI(dviFile)
    grid.newpage()
    grid.dvi(dvi)

    ## Explicit render engine that does NOT match typeset() engine
    tex <- author("This is a test: $x - \\mu$", engine="xetex")
    dviFile <- typeset(tex, engine="xetex", texFile=texFile)
    dvi <- readDVI(dviFile)
    grid.newpage()
    tools::assertWarning(grid.dvi(dvi, engine="null"))

}

