
library(xdvir)
library(grid)

## Make debugging information available
options(tinytex.verbose=TRUE, xdvir.quiet=FALSE)

if (nchar(Sys.getenv("GITHUB_RUN_ID"))) {
    ## For testing on github runners, create files within .Rcheck
    ## directory so that we can easily return them as artifacts
    texFile <- "test-size.tex"
} else {
    texFile <- NULL
}

if (Sys.info()["sysname"] == "Darwin") {
    png("size-%02d.png", type="quartz")
} else {
    ## "Windows" or "Linux"
    png("size-%02d.png", type="cairo")
}

if (xdvir:::haveTeX()) {
    grid.newpage()
    x <- latexGrob("\\Huge test", texFile=texFile)
    grid.draw(x)
    grid.rect(width=grobWidth(x), height=grobHeight(x), gp=gpar(fill=NA))
    grid.circle(grobX(x, "west"), grobY(x, "west"), r=unit(1, "mm"),
                gp=gpar(fill="black"))
}

dev.off()


