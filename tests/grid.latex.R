
library(grid)
library(xdvir)

## Make debugging information available
options(tinytex.verbose=TRUE, xdvir.quiet=FALSE)

## Create .tex files within .Rcheck directory so that we can more easily
## debug problems (or return them as artifacts from github runners)
texFile <- "test-latex.tex"

if (Sys.info()["sysname"] == "Darwin") {
    png("grid-latex-%02d.png", type="quartz")
} else {
    ## "Windows" or "Linux"
    png("grid-latex-%02d.png", type="cairo")
}

if (xdvir:::haveTeX()) {

    grid.newpage()
    grid.latex("This is a test: $x - \\mu$", texFile=texFile)

}
    
dev.off()
