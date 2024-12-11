
## Generate LaTeX file from TeX string

comment <- function(engine, packages) {
    paste0("%% ", buildSignature(engine, packages))
}

## Was the TeX code authored by this package?
commentLine <- function(tex) {
    grep(commentHeader, tex, fixed=TRUE, value=TRUE)
}

## Generate LaTeX code that sets font family, face, size, and lineheight.
## Return LaTeX code and set of required packages.
## Assume that all values are non-NULL, though family could be "" (default font)
## Assume that all arguments are the same length
##   If they come from element_latex or geom_late they are from a data frame
##   If they come from grid.latex they are already rep'ed
## This function MUST be evaluated in the context within which the
## LaTeX code will be rendered (otherwise, e.g., default fonts may be wrong).
TeXfaces <- c("", ## "\\mdseries ",
              "\\bfseries ",
              "\\itshape ",
              "\\bfseries \\itshape ")
    
preset <- function(family, face, size, lineheight, colour) {
    packages <- list("fontspec")
    
    if (is.null(family))
        stop("No font family specified")
    defaultFonts <- !nchar(family) | family %in% c("sans", "serif", "mono")
    if (any(defaultFonts)) {
        family[defaultFonts] <- currentFont(family, face)
    }
    ## NOTE that this returns path with "/" separator even on Windows
    dirs <- dirname(family)
    local <- dirs == "."
    fontfamily <- ifelse(local,
                         paste0("\\setmainfont{", family, "}\n"),
                         paste0("\\setmainfont{", basename(family), "}",
                                ## NOTE that this adds trailing slash to path
                                "[Path=", dirs, "/]\n"))
    
    if (is.null(face))
        stop("No font face specified")
    faces <- TeXfaces[match(face, c("plain", "bold", "italic", "bold-italic"))]
    fontface <- ifelse(nchar(faces), paste0(faces, "\n"), "")
    
    if (is.null(size))
        stop("No font size specified")
    if (is.null(lineheight))
        stop("No line height specified")
    fontsize <- paste0("\\fontsize{", size, "}{", size * lineheight, "}\n",
                       "\\selectfont{}\n")
    
    if (is.null(colour))
        stop("No colour specified")
    rgb <- col2rgb(colour)
    col <- paste0("\\definecolor{xdvir}{RGB}{",
                  rgb[1,], ",", rgb[2,], ",", rgb[3,], "}\n",
                  "\\color{xdvir}\n")
    black <- apply(rgb, 2, function(x) all(x == 0))
    col[black] <- ""
    if (any(!black)) {
        packages <- c(packages, list("xcolor"))
    }
    
    tex <- paste(fontfamily, fontsize, fontface, col, sep="")
    attr(tex, "packages") <- packages
    tex
}

author <- function(tex,
                   width=NA,
                   engine=getOption("xdvir.engine"),
                   packages=NULL) {
    if (!is.character(tex))
        stop("'tex' should be a character value containing a LaTeX fragment")
    if (length(tex) < 1)
        stop("No LaTeX fragment to author")
    if (length(width) < 1) {
        width <- NA
    }
    if (length(width) > 1) {
        warning("Only using first width")
        width <- width[1]
    }
    if (is.na(width)) {
        varwidth <- "varwidth"
    } else {
        varwidth <- paste0("varwidth=",
                           as.numeric(width),
                           "in")
    }
    engine <- getEngine(engine)
    pkgs <- resolvePackages(packages)
    if (length(pkgs)) {
        pkgNames <- paste(sapply(pkgs, function(x) x$name), collapse=", ")
    } else {
        pkgNames <- ""
    }
    texDoc <- c(## Record engine used for authoring
                comment(engine, pkgNames),
                paste0("\\documentclass[", varwidth, "]{standalone}"),
                engine$preamble,
                packagePreamble(pkgs),
                "\\begin{document}",
                packagePrefix(pkgs),
                tex,
                packageSuffix(pkgs),
                "\\end{document}")
    attr(texDoc, "engine") <- engine
    attr(texDoc, "packages") <- pkgNames
    class(texDoc) <- "LaTeXdocument"
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

authorEngine.LaTeXdocument <- function(tex) {
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

authorPackages.LaTeXdocument <- function(tex) {
    attr(tex, "packages")
}

print.LaTeXdocument <- function(x, ...) {
    cat(as.character(x), sep="\n")
}
