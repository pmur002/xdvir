
## Generate LaTeX file from TeX string

comment <- function(engine, packages) {
    paste0("%% ", buildSignature(engine, packages))
}

## Was the TeX code authored by this package?
commentLine <- function(tex) {
    grep(commentHeader, tex, fixed=TRUE, value=TRUE)
}

author <- function(tex,
                   engine=getOption("xdvir.engine"),
                   packages=NULL) {
    engine <- getEngine(engine)
    pkgs <- resolvePackages(packages)
    if (length(pkgs)) {
        pkgNames <- paste(sapply(pkgs, function(x) x$name), collapse=", ")
    } else {
        pkgNames <- ""
    }
    texDoc <- c(## Record engine used for authoring
                comment(engine, pkgNames),
                "\\documentclass{standalone}",
                engine$preamble,
                packagePreamble(pkgs),
                "\\begin{document}",
                packagePrefix(pkgs),
                tex,
                packageSuffix(pkgs),
                "\\end{document}")
    attr(texDoc, "engine") <- engine
    attr(texDoc, "packages") <- pkgNames
    class(texDoc) <- "TeXdocument"
    texDoc
}

## What engine was used to author the TeX code?
authorEngine <- function(tex) {
    UseMethod("authorEngine")
}

authorEngine.character <- function(tex) {
    commentLine <- commentLine(tex)
    if (length(commentLine)) {
        engine <- signatureEngine(commentLine)
        getEngine(engine$name)
    } else {
        NULL
    }
}

authorEngine.TeXdocument <- function(tex) {
    attr(tex, "engine")
}

authorPackages <- function(tex) {
    UseMethod("authorPackages")
}

authorPackages.character <- function(tex) {
    commentLine <- commentLine(tex)
    if (length(commentLine)) {
        signaturePackages(commentLine)
    } else {
        ## FIXME:  could look for \usepackage{} commands ?
        NULL
    }
}

authorPackages.TeXdocument <- function(tex) {
    attr(tex, "packages")
}

print.TeXdocument <- function(x, ...) {
    cat(as.character(x), sep="\n")
}
