
opCodes <- function(dvi) {
    sapply(dvi,
           function(op) {
               blockValue(op$blocks$op.opcode)
           })
}

opParams <- function(dvi) {
    lapply(dvi,
           function(op) {
               ## Remove code (first component of blocks)
               nocode <- op$blocks[-1]
               params <- lapply(nocode,
                                function(p) {
                                    blockValue(p)
                                })
               names <- names(nocode)
               if (length(names)) {
                   names(params) <- gsub("op.opparams[.]?", "", names)
               }
               params
           })
}

operationNames <- character(256)
operationNames[1:128] <- paste0("set_char_", 0:127)
operationNames[129:132] <- paste0("set", 1:4)
operationNames[133] <- "set_rule"
operationNames[134:137] <- paste0("put", 1:4)
operationNames[138] <- "put_rule"
operationNames[139] <- "nop"
operationNames[140] <- "bop"
operationNames[141] <- "eop"
operationNames[142] <- "push"
operationNames[143] <- "pop"
operationNames[144:147] <- paste0("right", 1:4)
operationNames[148:152] <- paste0("w", 0:4)
operationNames[153:157] <- paste0("x", 0:4)
operationNames[158:161] <- paste0("down", 1:4)
operationNames[162:166] <- paste0("y", 0:4)
operationNames[167:171] <- paste0("z", 0:4)
operationNames[172:235] <- paste0("fnt_num_", 0:63)
operationNames[236:239] <- paste0("fnt", 1:4)
operationNames[240:243] <- paste0("xxx", 1:4)
operationNames[244:247] <- paste0("fnt_def", 1:4)
operationNames[248] <- "pre"
operationNames[249] <- "post"
operationNames[250] <- "post_post"
operationNames[253] <- "x_fnt_def"
operationNames[254] <- "x_glyph"
operationNames[255] <- "x_glyph_str"
operationNames[256] <- "dir"
    
opNames <- function(codes) {
    if (!length(codes) ||
        !all(is.finite(codes)) ||
        any(codes < 0 | codes > 255))
        stop("Invalid codes")
    operationNames[codes + 1]
}
