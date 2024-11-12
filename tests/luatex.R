
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

if (xdvir:::luatexAvailable() && xdvir:::luaOTFloadToolSufficient()) {
    grid.newpage()
    grid.lualatex("This is a test: $x - \\mu$", texFile=texFile)
}

