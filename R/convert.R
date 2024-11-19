
## Conversions
## to mm
fromTeX <- function(x, state) {
    factor <- TeXget("factor", state)
    x*factor
}

## from 'grid' units
toTeX <- function(unit, state) {
    factor <- TeXget("factor", state)
    convertX(unit, "mm", valueOnly=TRUE)/factor
}

