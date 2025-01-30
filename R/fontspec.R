
fontspecPreamble <- function(font=NULL) {
    preamble <- "\\usepackage{fontspec}"
    if (!is.null(font)) {
        if (dirname(font) == ".") {
            setfont <- paste0("\\setmainfont{", font, "}")
        } else {
            setfont <- paste0("\\setmainfont{", basename(font), "}",
                              ## Must add trailing slash
                              "[Path=", paste0(dirname(font), "/"), "]")
        }
        preamble <- c(preamble, setfont)
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

