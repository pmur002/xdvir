
currentFont <- function() {
    family <- get.gpar("fontfamily")$fontfamily
    if (!nchar(family)) {
        family <- "sans"
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
    systemfonts::match_fonts(family)$path
}

currentFace <- function() {
    face <- get.gpar("font")$font
    c("plain", "bold", "italic", "bold-italic")[face]
}
