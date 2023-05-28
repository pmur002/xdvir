
## Generate DVI from TeX string

typeset <- function(x,
                    engine,
                    ...) {
    UseMethod("typeset")
}

## 'x' is a "TeXdocument" from author()
typeset.TeXdocument <- function(x,
                                engine,
                                texFile=NULL,
                                ...) {
    if (is.null(texFile)) {
        texFile <- tempfile(fileext=".tex")
    }
    texDir <- dirname(texFile)
    dviFile <- paste0(gsub("[.]tex", "", texFile), engine$dviSuffix)
    writeLines(x, texFile)
    system(paste0(engine$command, 
                  " --output-directory=", texDir,
                  " ", texFile))
    invisible(dviFile)
}

## 'x' is the name of a file containing a TeX document
typeset.character <- function(x,
                              engine,
                              texFile=NULL) {
    texDir <- dirname(x)
    ## TeX file may not have .tex suffix
    dviFile <- paste0(gsub("[.]tex", "", texFile), engine$dviSuffix)
    system(paste0(engine$command, 
                  " --output-directory=", texDir,
                  " ", x))
    invisible(dviFile)
}
                    

