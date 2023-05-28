
## Generate LaTeX file from TeX string

author <- function(tex,
                   engine,
                   texFile=NULL) {
    texDoc <- c("\\documentclass{standalone}",
                engine$preamble,
                "\\begin{document}",
                engine$prefix,
                tex,
                engine$suffix,
                "\\end{document}")
    class(texDoc) <- "TeXdocument"
    if (!is.null(texFile)) {
        writeLines(texDoc, texFile)
        attr(texDoc, "texFile") <- texFile
    }
    invisible(texDoc)
}
