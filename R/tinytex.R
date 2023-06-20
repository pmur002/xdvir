
initTinyTeX <- function() {
    if (nchar(system.file(package="tinytex"))) {
        message(paste0("tinytex:  ", packageVersion("tinytex")))
        options("xdvir.tinytex"=TRUE)
    } else {
        message("tinytex:  not installed")
        options("xdvir.tinytex"=FALSE)
    }
}
