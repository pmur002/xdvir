
library(grid)
library(xdvir)

## Make debugging information available
options(tinytex.verbose=TRUE, xdvir.quiet=FALSE)

## Create .tex files within .Rcheck directory so that we can more easily
## debug problems (or return them as artifacts from github runners)
texFile <- "test-lualatex.tex"

if (Sys.info()["sysname"] == "Darwin") {
    png("luatex-%02d.png", type="quartz")
} else {
    ## "Windows" or "Linux"
    png("luatex-%02d.png", type="cairo")
}

if (xdvir:::haveTeX()) {

    str <- r"(This is a test: $\frac{x - \mu}{2}$)"

    if (xdvir:::luatexAvailable() && xdvir:::luaOTFloadToolSufficient()) {
        grid.newpage()
        grid.lualatex(str, texFile=texFile)
    }

}

dev.off()

