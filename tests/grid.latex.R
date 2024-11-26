
library(grid)
library(xdvir)

## Make debugging information available
options(tinytex.verbose=TRUE, xdvir.quiet=FALSE)

if (nchar(Sys.getenv("GITHUB_RUN_ID"))) {
    ## For testing on github runners, create files within .Rcheck
    ## directory so that we can easily return them as artifacts
    texFile <- "test-latex.tex"
} else {
    texFile <- NULL
}

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
