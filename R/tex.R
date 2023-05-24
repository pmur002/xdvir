
initTeX <- function() {
    kpsewhich <- Sys.which("kpsewhich")
    if (nchar(kpsewhich) == 0)
        stop("Failed to find kpsewhich; please install TeX")
    versText <- system("kpsewhich --version", intern=TRUE)
    versLine <- grep("kpathsea version", versText)
    version <- gsub(".+ ", "", versText[versLine])
    set("kpathseaVersion", version)
    ## Set font mappings
    set("pdftexMap", readLines(system("kpsewhich pdftex.map", intern=TRUE)))
    set("kanjixMap", readLines(system("kpsewhich kanjix.map", intern=TRUE)))
}
