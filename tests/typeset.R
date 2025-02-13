
library(xdvir)

## Make debugging information available
options(tinytex.verbose=TRUE, xdvir.quiet=FALSE)

## Create .tex files within .Rcheck directory so that we can more easily
## debug problems (or return them as artifacts from github runners)
texFile <- "test-typeset.tex"

if (xdvir:::haveTeX()) {

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
    tex <- readLines(system.file("TeX", "test.tex", package="xdvir"))
    tools::assertWarning(typeset(tex, texFile=texFile))

    ## Manual TeX (so author engine unknown)
    ## AND explicit typeset engine 
    tex <- readLines(system.file("TeX", "test.tex", package="xdvir"))
    tools::assertWarning(typeset(tex, engine="xetex", texFile=texFile))

}
