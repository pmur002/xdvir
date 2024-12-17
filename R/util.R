
currentFamily <- function(family) {
    defaultFonts <- !nchar(family)
    if (any(defaultFonts)) {
        family[defaultFonts] <- "sans"
    }
    dev <- names(dev.cur())
    if (dev %in% c("postscript", "pdf")) {
        family <- ifelse(family == "serif", "Times",
                  ifelse(family == "sans", "Helvetica",
                  ifelse(family == "mono", "Courier",
                         family)))
    } else if (grepl("cairo", dev)) {
        if (.Platform$OS.type == "windows") {
            family <- ifelse(family == "serif", "Times New Roman",
                      ifelse(family == "sans", "Arial",
                      ifelse(family == "mono", "courier",
                             family)))
        } else {
            family <- ifelse(family == "serif", "times",
                      ifelse(family == "sans", "Helvetica",
                      ifelse(family == "mono", "courier",
                             family)))
        }
    } else if (dev == "quartz") {
        family <- ifelse(family == "serif", "Times-Roman",
                  ifelse(family == "sans", "Helvetica",
                  ifelse(family == "mono", "Courier",
                         family)))
    } else if (dev == "windows") {
        family <- ifelse(family == "serif", "Times New Roman",
                  ifelse(family == "sans", "Arial",
                  ifelse(family == "mono", "Courier New",
                         family)))
    } else {
        ## Just a wild guess really
        family <- ifelse(family == "serif", "Times",
                  ifelse(family == "sans", "Helvetica",
                  ifelse(family == "mono", "Courier",
                         family)))
    }
    family
}

## Does typesetting need to be delayed
delayTypeset <- function(width, gp) {
    ## One reason is that the width is NOT NA
    ## AND width has been set to a relative unit,
    ## which will have to be evaulated when drawing occurs
    relativeWidth <- FALSE
    naWidth <- is.na(width)
    if (any(!naWidth)) {
        relativeWidth <- any(unitType(absolute.size(width[!naWidth])) == "null")
    }
    ## One reason is that gp is NOT NULL, so will require the
    ## fontfamily, fontface, fontsize, and cex in effect when drawing occurs
    nullGPar <- is.null(gp)
    relativeWidth || !nullGPar
}
