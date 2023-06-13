
initTinyTeX <- function() {
    if (nchar(system.file(package="tinytex"))) {
        options("xdvir.tinytex"=TRUE)
    } else {
        options("xdvir.tinytex"=FALSE)
    }
}
