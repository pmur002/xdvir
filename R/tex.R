
initTeX <- function() {
    kpsewhich <- Sys.which("kpsewhich")
    if (nchar(kpsewhich) == 0)
        stop("Failed to find kpsewhich; please install TeX")
    ## Set font mappings
    set("pdftexMap", readLines(system("kpsewhich pdftex.map", intern=TRUE)))
    set("kanjixMap", readLines(system("kpsewhich kanjix.map", intern=TRUE)))
}
