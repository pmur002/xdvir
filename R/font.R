
## Generic functions for generating and working with font structures
## (regardless of which engine we are using)

identical_font <- function(op1, op2) {
    identical(blockValue(op1$blocks$op.opcode),
              blockValue(op2$blocks$op.opcode)) &&
    identical(blockValue(op1$blocks$op.opparams.k),
              blockValue(op2$blocks$op.opparams.k)) &&
    identical(blockValue(op1$blocks$op.opparams.c),
              blockValue(op2$blocks$op.opparams.c)) &&
    identical(blockValue(op1$blocks$op.opparams.s),
              blockValue(op2$blocks$op.opparams.s)) &&
    identical(blockValue(op1$blocks$op.opparams.d),
              blockValue(op2$blocks$op.opparams.d)) &&
    identical(blockValue(op1$blocks$op.opparams.fontname.name),
              blockValue(op2$blocks$op.opparams.fontname.name))
}

fontDef <- function(file, index,
                    family, weight, style,
                    size=10) {
    def <- list(file=file, index=index,
                family=family, weight=weight, style=style,
                size=size)
    class(def) <- "TeXfontDef"
    def
}
