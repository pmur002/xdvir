
xcolorPreamble <- "\\usepackage{xcolor}"

xcolorSpecial <- function(specialString, state) {
    ## Ignore any other specials
    if (grepl("^color push ", specialString)) {
        token <- strsplit(specialString, " ")[[1]]
        colourspace <- token[3]
        if (colourspace == "gray") {
            colourspec <- as.numeric(token[4])
            colour <- gray(colourspec)
        } else if (colourspace == "rgb") {
            colourspec <- as.numeric(token[4:6])
            colour <- rgb(colourspec[1], colourspec[2], colourspec[3])
        } else {
            warning("Unsupported colourspace - colour not set")
            colour <- NA
        }            
        TeXset("colour", c(colour, TeXget("colour", state)), state)
    }
    if (grepl("^color pop", specialString)) {
        TeXset("colour", TeXget("colour", state)[-1], state)
    }
}

xcolorPackage <- function() {
    LaTeXpackage(name="xcolor",
                 preamble=xcolorPreamble,
                 special=xcolorSpecial)
}
