
initTinyTeX <- function() {
    version <- packageVersion("tinytex")
    set("tinytexVersion", version)
}

tinytexVersion <- function() {
    get("tinytexVersion")
}

tinytexAvailable <- function() {
    !is.null(tinytexVersion())
}
