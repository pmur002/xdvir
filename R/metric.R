
updateHoriz <- function(x) {
    right <- get("right")
    if (!is.finite(right) || x > right) {
        set("right", x)
    }
    left <- get("left")
    if (!is.finite(left) || x < left) {
        set("left", x)
    }
}

updateVert <- function(x) {
    top <- get("top")
    if (!is.finite(top) || x < top) {
        set("top", x)
    }
    bottom <- get("bottom")
    if (!is.finite(bottom) || x > bottom) {
        set("bottom", x)
    }
}
