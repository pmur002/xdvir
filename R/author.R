
## Generate LaTeX file from TeX string

author <- function(tex,
                   engine,
                   packages=NULL,
                   texFile=NULL) {
    texDoc <- c("\\documentclass{standalone}",
                engine$preamble,
                packagePreamble(packages),
                "\\begin{document}",
                packagePrefix(packages),
                tex,
                packageSuffix(packages),
                "\\end{document}")
    class(texDoc) <- "TeXdocument"
    if (!is.null(texFile)) {
        writeLines(texDoc, texFile)
        attr(texDoc, "texFile") <- texFile
    }
    invisible(texDoc)
}
