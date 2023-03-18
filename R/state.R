
## Parsing DVI requires maintaining various state parameters
## like current location and current font

xdvir_state <- new.env()

get <- function(name) {
    base::get0(name, envir=xdvir_state, inherits=FALSE)
}

mget <- function(names) {
    base::mget(names, envir=xdvir_state, inherits=FALSE)
}

set <- function(name, value) {
    assign(name, value, envir=xdvir_state)
}

## Default state
set("scale", 1)
## Extra slot for dummy font
set("fonts", vector("list", 256))
set("glyphs", list())
set("dir", 0)
