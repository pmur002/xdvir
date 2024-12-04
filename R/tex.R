
################################################################################
## TeX state for keeping track of values during DVI sweep

TeXstate <- function() {
    state <- new.env()
    state
}

TeXget <- function(name, state) {
    base::get0(name, envir=state, inherits=FALSE)
}

TeXmget <- function(names, state) {
    base::mget(names, envir=state, inherits=FALSE)
}

TeXset <- function(name, value, state) {
    assign(name, value, envir=state)
}

