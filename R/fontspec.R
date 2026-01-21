
fontspecPreamble <- function(font=NULL, renderer=NULL, axes=NULL) {
    preamble <- "\\usepackage{fontspec}"
    defaults <- NULL
    if (!is.null(renderer) && tolower(renderer) == "harfbuzz")
        defaults <- c(defaults, "Renderer=Harfbuzz")
    if (!is.null(axes)) {
        variations <- xdvirGlyphFontVariation(axes)
        if (is.null(variations)) {
            warning("Ignoring unsupported font axes")
        } else {
            defaults <- c(defaults,
                          paste0("RawFeature={axis={",
                                 paste(names(variations), variations, sep="="),
                                 "}}"))
        }
    }
    if (!is.null(defaults)) {
        preamble <- c(preamble,
                      paste0("\\defaultfontfeatures{",
                             paste(defaults, collapse=","),
                             "}"))
    }
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

fontspecPackage <- function(font=NULL, name=font, renderer=NULL, axes=NULL) {
    if (is.null(font)) {
        LaTeXpackage(name="fontspec",
                     preamble=fontspecPreamble(renderer=renderer))
    } else {
        LaTeXpackage(name=name,
                     preamble=fontspecPreamble(font=font,
                                               renderer=renderer,
                                               axes=axes))
    }
}

