
readDVI <- function(file) {
    ## Create a list that is known to be too long
    ops <- vector("list", file.size(file))
    done <- FALSE
    offset <- 0
    length <- 1
    guessEngine <- FALSE
    while (!done) {
        op <- readFormat(file, opFormat, offset=offset)
        if (length == 1 &&
            blockValue(op$blocks$op.opcode) != 247) {
            warning("DVI file does not start with a 'pre' op")
        }
        if (blockValue(op$blocks$op.opcode) == 249) {
            done <- TRUE
        }
        offset <- offset + op$nbytes
        ops[[length]] <- op
        length <- length + 1
    }
    result <- ops[1:(length - 1)]
    class(result) <- "DVI"
    result
}


