
set("FTcache", list())

addFont <- function(fontName) {
    font <- .Call(C_glyphMetrics, as.character(fontName)[1])
    names(font) <- c("unitsPerEm", "metrics")
    font$metrics <- lapply(font$metrics,
                           function(x) {
                               names(x) <- c("width",
                                             "left", "right", "top", "bottom")
                               x
                           })
    cache <- get("FTcache")
    cache[[fontName]] <- font
    set("FTcache", cache)
    font
}

getFont <- function(fontName) {
    cache <- get("FTcache")
    font <- cache[[fontName]]
    if (is.null(font)) {
        addFont(fontName)
    } else {
        font
    }
}

FTglyphMetrics <- function(index, subset, fontName) {
    font <- getFont(fontName)
    ## +1 because glyph indices are zero-based, but
    ## we are accessing a one-based R vector
    metrics <- font$metrics[[index + 1]][subset]
    attr(metrics, "unitsPerEm") <- font$unitsPerEm
    metrics
}

FTglyphWidth <- function(index, fontName) {
    FTglyphMetrics(index, "width", fontName)
}

FTglyphBounds <- function(index, fontName) {
    FTglyphMetrics(index,
                   c("left", "bottom", "right", "top"),
                   fontName)
}

FTglyphIndex <- function(code, fontName) {
    .Call(C_glyphIndex, as.integer(code), as.character(fontName)[1])
}

FTfontLibrary <- FontLibrary(glyphWidth=FTglyphWidth,
                             glyphHeight=NULL,
                             glyphBounds=FTglyphBounds,
                             glyphIndex=FTglyphIndex)
