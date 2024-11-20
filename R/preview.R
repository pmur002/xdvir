
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

previewInit <- function(state) {
    TeXset("preview-baseline", NA, state)
}

previewSpecial <- function(specialString, state) {
    ## Ignore any other specials
    if (grepl("^xdvir-preview:: ", specialString)) {
        depth <- strsplit(specialString, " ")[[1]][9]
        TeXset("preview-baseline", as.numeric(depth), state)
    }
}

previewFinal <- function(state) {
    baseline <- TeXget("preview-baseline", state)
    if (!is.na(baseline)) {
        bottom <- TeXget("bottom", state)
        addAnchor(bottom - baseline,
                  "preview-baseline", type="v",
                  state)
    }
}

previewPackage <- function() {
    LaTeXpackage(name="preview",
                 preamble=previewPreamble,
                 prefix=previewPrefix,
                 suffix=previewSuffix,
                 special=previewSpecial,
                 init=previewInit,
                 final=previewFinal)
}


