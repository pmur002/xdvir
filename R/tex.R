
initTeX <- function() {
    kpsewhich <- Sys.which("kpsewhich")
    if (nchar(kpsewhich) == 0) {
        message("kpsewhich:  not found")
    } else {
        versText <- system("kpsewhich --version", intern=TRUE)
        versLine <- grep("kpathsea version", versText)
        version <- gsub(".+ ", "", versText[versLine])
        set("kpathseaVersion", version)
        message(paste0("kpsewhich:  ", version))
        ## Set font mappings
        set("pdftexMap", readLines(system("kpsewhich pdftex.map", intern=TRUE)))
        set("kanjixMap", readLines(system("kpsewhich kanjix.map", intern=TRUE)))
    }
}
