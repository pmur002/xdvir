
## Conversions
## to (big) points
fromTeX <- function(x, state) {
    factor <- TeXget("scaledFactor", state)
    x*factor
}

## from (big) points
toTeX <- function(x, state) {
    factor <- TeXget("scaledFactor", state)
    x/factor
}

