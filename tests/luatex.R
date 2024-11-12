
library(grid)
library(xdvir)

## Make debugging information available
options(tinytex.verbose=TRUE, xdvir.quiet=FALSE)

if (.Platform$OS.type == "windows") {
    ## For testing on github Windows runners, avoid tmp dir
    ## for files that a TeX engine will run on
    texFile <- "test-lualatex.tex"
} else {
    texFile <- NULL
}

if (xdvir:::luatexAvailable() && xdvir:::luaOTFloadToolSufficient()) {
    grid.newpage()
    grid.lualatex("This is a test: $x - \\mu$", texFile=texFile)
}

