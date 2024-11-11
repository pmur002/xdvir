
library(xdvir)

## Make debugging information available
options(tinytex.verbose=TRUE, xdvir.quiet=FALSE)

if (.Platform$OS.type == "windows") {
    ## For testing on github Windows runners, avoid tmp dir
    ## for files that a TeX engine will run on
    texFile <- "test-typeset.tex"
} else {
    texFile <- NULL
}

## author() engine defaults 
## Typeset engine taken from author() engine
tex <- author("This is a test: $x - \\mu$")
typeset(tex, texFile=texFile)

## Explicit typeset engine that matches author() engine
tex <- author("test", engine="xetex")
typeset(tex, engine="xetex", texFile=texFile)

## Explicit typeset engine that does NOT match author() engine
tex <- author("test", engine="null")
tools::assertWarning(typeset(tex, engine="xetex", texFile=texFile))
        
## Manual TeX (so author engine unknown)
## Typeset engine defaults
tex <- readLines(system.file("TeX", "manual.tex", package="latex"))
tools::assertWarning(typeset(tex, texFile=texFile))

## Manual TeX (so author engine unknown)
## AND explicit typeset engine 
tex <- readLines(system.file("TeX", "manual.tex", package="latex"))
tools::assertWarning(typeset(tex, engine="xetex", texFile=texFile))
