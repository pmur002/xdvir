
## Conversions

## to (big) points
TeX2pt <- function(x, state) {
    factor <- TeXget("scaledFactor", state)
    x*factor
}

## from (big) points
pt2TeX <- function(x, state) {
    factor <- TeXget("scaledFactor", state)
    x/factor
}

## to pixels (round or floor or ceiling yourself if you want to)
TeX2px <- function(x, state) {
    conv <- TeXget("scaledConv", state)
    x*conv
}

## from pixels
px2TeX <- function(x, state) {
    conv <- TeXget("scaledConv", state)
    x/conv
}
