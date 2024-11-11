
initTinyTeX <- function() {
    version <- packageVersion("tinytex")
    set("tinytexVersion", version)
    if (is_tinytex()) {
        set("texVersion", tinytex_root(error=FALSE))
    } else {
        ## Assume that, if 'latex' is on the PATH, then {tinytex} can see it
        TeX <- Sys.which("latex")
        if (nchar(TeX > 0)) {
            set("texVersion", TeX)
        }
    }
}

tinytexVersion <- function() {
    get("tinytexVersion")
}

tinytexAvailable <- function() {
    !is.null(tinytexVersion())
}

texVersion <- function() {
    get("texVersion")
}

haveTeX <- function() {
    !is.null(texVersion())
}
