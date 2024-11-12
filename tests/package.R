
library(grid)
library(xdvir)

## Make debugging information available
options(tinytex.verbose=TRUE, xdvir.quiet=FALSE)

fontpath <- system.file("Fonts", "Montserrat", package="xdvir")

tex <- paste0("\\setmainfont{Montserrat-Regular.ttf}",
              ## On Windows, the path may contain ~
              "[Path=", gsub("~", "\\\\string~", fontpath), "/]\n",
              "This is a test")

if (nchar(Sys.getenv("GITHUB_RUN_ID"))) {
    ## For testing on github runners, create files within .Rcheck
    ## directory so that we can easily return them as artifacts
    texFile <- "test-package.tex"
} else {
    texFile <- NULL
}
    
## Package as LaTeXpackage object
grid.newpage()
grid.latex(tex, packages=fontspecPackage(), texFile=texFile)

## Package as package alias
grid.newpage()
grid.latex(tex, packages="fontspec", texFile=texFile)
        
## TODO:
## Package in author, but not in render
## Package in render, but not in author
## ...

