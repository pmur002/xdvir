
library(grid)
library(xdvir)

## Make debugging information available
options(tinytex.verbose=TRUE, xdvir.quiet=FALSE)

if (.Platform$OS.type == "windows") {
    ## For testing on github Windows runners, avoid tmp dir
    ## for files that a TeX engine will run on
    texFile <- "test-latex.tex"
} else {
    texFile <- NULL
}
    
grid.newpage()
grid.latex("This is a test: $x - \\mu$", texFile=texFile)
    

