
library(grid)
library(xdvir)

## Make debugging information available
options(tinytex.verbose=TRUE, xdvir.quiet=FALSE)

fontpath <- system.file("fonts", "Montserrat", "static", package="grDevices")

tex <- paste0("\\setmainfont{Montserrat-Medium.ttf}",
              ## On Windows, the path may contain ~
              "[Path=", gsub("~", "\\\\string~", fontpath), "/]\n",
              "This is a test")

if (.Platform$OS.type == "windows") {
    ## For testing on github Windows runners, avoid tmp dir
    ## for files that a TeX engine will run on
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

