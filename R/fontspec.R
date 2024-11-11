
fontspecPreamble <- function(font=NULL) {
    preamble <- "\\usepackage{fontspec}"
    if (!is.null(font)) {
        preamble <- c(preamble,
                      paste0("\\setmainfont{", font, "}"))
    }
    preamble
}

fontspecPackage <- function(font=NULL, name=font) {
    if (is.null(font)) {
        LaTeXpackage(name="fontspec", preamble=fontspecPreamble())
    } else {
        LaTeXpackage(name=name, preamble=fontspecPreamble(font))
    }
}

