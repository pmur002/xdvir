
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
    texFile <- "test-fontspec.tex"
} else {
    texFile <- NULL
}

if (Sys.info()["sysname"] == "Darwin") {
    png("fontspec-%02d.png", type="quartz")
} else {
    ## "Windows" or "Linux"
    png("fontspec-%02d.png", type="cairo")
}

if (xdvir:::haveTeX()) {
    grid.newpage()
    grid.latex(tex, packages=fontspecPackage(), texFile=texFile)
}

dev.off()
