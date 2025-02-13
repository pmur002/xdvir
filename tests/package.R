
library(grid)
library(xdvir)

## Make debugging information available
options(tinytex.verbose=TRUE, xdvir.quiet=FALSE)

fontpath <- system.file("Fonts", "Montserrat", package="xdvir")

tex <- paste0("\\setmainfont{Montserrat-Regular.ttf}",
              ## On Windows, the path may contain ~
              "[Path=", gsub("~", "\\\\string~", fontpath), "/]\n",
              "This is a test")

## Create .tex files within .Rcheck directory so that we can more easily
## debug problems (or return them as artifacts from github runners)
texFile <- "test-package.tex"

if (Sys.info()["sysname"] == "Darwin") {
    png("package-%02d.png", type="quartz")
} else {
    ## "Windows" or "Linux"
    png("package-%02d.png", type="cairo")
}

if (xdvir:::haveTeX()) {

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

}

dev.off()
