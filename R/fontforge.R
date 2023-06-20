
initFontForge <- function() {
    fontforge <- Sys.which("fontforge")
    if (nchar(fontforge) == 0) {
        message("fontforge:  not found")
    } else {
        versText <- system("fontforge --version", intern=TRUE,
                           ignore.stderr=TRUE)
        versLine <- grep("fontforge", versText)
        version <- gsub(".+ ", "", versText[versLine])
        message(paste0("fontforge:  ", version))
        set("ffVersion", version)
    }
}

otf2ttf <- function(otfFile, ttfFile) {
    message(paste0("Converting ", otfFile, " to ", ttfFile))
    system(paste0("fontforge -lang=ff -c 'Open($1); Reencode(\"original\"); Generate($2); Close();' ",
                  otfFile, " ", ttfFile))
}
