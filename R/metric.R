
## Maintain text left/right

updateTextLeft <- function(x, state) {
    left <- TeXget("textleft", state)
    if (!is.finite(left) || x < left) {
        TeXset("textleft", x, state)
    }
}

updateTextRight <- function(x, state) {
    right <- TeXget("textright", state)
    if (!is.finite(right) || x > right) {
        TeXset("textright", x, state)
    }
}

## Maintain "ink" bbox

updateBBoxHoriz <- function(x, state) {
    right <- TeXget("right", state)
    if (!is.finite(right) || x > right) {
        TeXset("right", x, state)
    }
    left <- TeXget("left", state)
    if (!is.finite(left) || x < left) {
        TeXset("left", x, state)
    }
}

updateBBoxVert <- function(x, state) {
    top <- TeXget("top", state)
    if (!is.finite(top) || x < top) {
        TeXset("top", x, state)
    }
    bottom <- TeXget("bottom", state)
    if (!is.finite(bottom) || x > bottom) {
        TeXset("bottom", x, state)
    }
}
