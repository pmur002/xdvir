
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
    texFile <- "test-fontspec.tex"
} else {
    texFile <- NULL
}

grid.newpage()
grid.latex(tex, packages=fontspecPackage(), texFile=texFile)

        
