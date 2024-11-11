
identical_font <- function(op1, op2) {
    all(mapply(identical,
               op1$blocks,
               op2$blocks))
}
