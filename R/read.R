
readVF <- function(f) {
    ## Create a list that is known to be too long
    ops <- vector("list", file.size(f))
    done <- FALSE
    offset <- 0
    length <- 1
    while (!done) {
        op <- readFormat(f, vfFormat, offset=offset)
        ## Just read as far as first 242 or 241
        ## in order to get the font definitions
        if (blockValue(op$blocks$op.opcode) < 243) {
            done <- TRUE
        }
        offset <- offset + op$nbytes
        ops[[length]] <- op
        length <- length + 1
    }
    result <- ops[1:(length - 1)]
    class(result) <- "VF"
    result
}

readDVI <- function(f) {
    ## Create a list that is known to be too long
    ops <- vector("list", file.size(f))
    done <- FALSE
    offset <- 0
    length <- 1
    while (!done) {
        op <- readFormat(f, opFormat, offset=offset)
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


