
FTglyphMetrics <- function(index, font) {
    result <- .Call(C_glyphMetrics, as.integer(index)[1], as.character(font)[1])
    names(result) <- c("unitsPerEm", "width", "left", "right", "top", "bottom")
    result
}

FTglyphWidth <- function(index, font) {
    metrics <- FTglyphMetrics(index, font)
    width <- metrics["width"]
    attr(width, "unitsPerEm") <- metrics["unitsPerEm"]
    width
}

FTglyphBounds <- function(index, font) {
    metrics <- FTglyphMetrics(index, font)
    bounds <- metrics[c("left", "bottom", "right", "top")]
    attr(bounds, "unitsPerEm") <- metrics["unitsPerEm"]
    bounds
}

FTfontLibrary <- FontLibrary(glyphWidth=FTglyphWidth,
                             glyphHeight=NULL,
                             glyphBounds=FTglyphBounds)
