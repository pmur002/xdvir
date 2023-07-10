
previewPreamble <-
    c("\\usepackage[active,tightpage,lyx]{preview}",
      "\\makeatletter",
      "\\g@addto@macro\\pr@ship@end{%",
      "\\setbox\\pr@box\\hbox{%",
      "\\special{xdvir-preview:: %",
      "Snippet \\number\\pr@snippet\\space%",
      "\\pr@bbadjust\\space%",
      "\\number\\ht\\pr@box\\space%",
      "\\number\\dp\\pr@box\\space%",
      "\\number\\wd\\pr@box%",
      "}%",
      "\\box\\pr@box%",
      "}%",
      "}",
      "\\makeatother")

previewPrefix <- "\\begin{preview}"

previewSuffix <- "\\end{preview}"

previewInit <- function() {
    set("preview-baseline", NA)
}

previewSpecial <- function(specialString) {
    ## Ignore any other specials
    if (grepl("^xdvir-preview:: ", specialString)) {
        depth <- strsplit(specialString, " ")[[1]][9]
        set("preview-baseline", as.numeric(depth))
    }
}

previewFinal <- function() {
    baseline <- get("preview-baseline")
    if (!is.na(baseline)) {
        bottom <- get("bottom")
        addAnchor(convertY(unit(-fromTeX(bottom - baseline), "mm"),
                           "bigpts", valueOnly=TRUE),
                  "preview-baseline", type="v")
    }
}

previewPackage <- function(font=NULL) {
    package(preamble=previewPreamble,
            prefix=previewPrefix,
            suffix=previewSuffix,
            special=previewSpecial,
            init=previewInit,
            final=previewFinal)
}

registerPackage(previewPackage(), "preview")
