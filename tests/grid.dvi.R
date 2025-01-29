
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

## Create .tex files within .Rcheck directory so that we can more easily
## debug problems (or return them as artifacts from github runners)
texFile <- "test-dvi.tex"

if (Sys.info()["sysname"] == "Darwin") {
    png("grid-dvi-%02d.png", type="quartz")
} else {
    ## "Windows" or "Linux"
    png("grid-dvi-%02d.png", type="cairo")
}

if (xdvir:::haveTeX()) {

    str <- r"(This is a test: $\frac{x - \mu}{2}$)"
    
    tex <- author(str)
    dvi <- typeset(tex, texFile=texFile)
    grid.newpage()
    grid.dvi(dvi)

    ## Explicit render engine that does NOT match typeset() engine
    tex <- author(str, engine="xetex")
    dvi <- typeset(tex, engine="xetex", texFile=texFile)
    grid.newpage()
    tools::assertWarning(grid.dvi(dvi, engine="null"))

}

dev.off()
