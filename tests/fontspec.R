
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
texFile <- "test-fontspec.tex"

if (Sys.info()["sysname"] == "Darwin") {
    png("fontspec-%02d.png", type="quartz")
} else {
    ## "Windows" or "Linux"
    png("fontspec-%02d.png", type="cairo")
}

if (xdvir:::haveTeX()) {
    grid.newpage()
    grid.latex(tex, packages=fontspecPackage(), texFile=texFile, gp=NULL)
}

dev.off()
