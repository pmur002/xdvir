
fontspecPreamble <- function(font=NULL) {
    preamble <- "\\usepackage{fontspec}"
    if (!is.null(font)) {
        preamble <- c(preamble,
                      paste0("\\setmainfont{", font, "}"))
    }
    preamble
}

fontspecPackage <- function(font=NULL) {
    package(preamble=fontspecPreamble(font))
}

registerPackage(fontspecPackage(), "fontspec")
