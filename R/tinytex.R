
initTinyTeX <- function() {
    if (nchar(system.file("tinytex"))) {
        options("xdvir.tinytex"=TRUE)
    } else {
        options("xdvir.tinytex"=FALSE)
    }
}
