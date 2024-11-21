
library(grid)
library(xdvir)

## Make debugging information available
options(tinytex.verbose=TRUE, xdvir.quiet=FALSE)

if (nchar(Sys.getenv("GITHUB_RUN_ID"))) {
    ## For testing on github runners, create files within .Rcheck
    ## directory so that we can easily return them as artifacts
    texFile <- "test-lualatex.tex"
} else {
    texFile <- NULL
}

png("luatex.png", type="cairo")

if (xdvir:::haveTeX()) {

    str <- r"(This is a test: $\frac{x - \mu}{2}$)"

    if (xdvir:::luatexAvailable() && xdvir:::luaOTFloadToolSufficient()) {
        grid.newpage()
        grid.lualatex(str, texFile=texFile)
    }

}

dev.off()

