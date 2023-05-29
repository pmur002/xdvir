

growAnchors <- function(value, label, which) {
    anchors <- get(which)
    if (is.null(anchors)) {
        anchors <- list(value=value, label=label)
    } else {
        anchors <- list(value=c(anchors$value, value),
                        label=c(anchors$label, label))
    }
    set(which, anchors)
}

addAnchor <- function(value, label, type="h") {
    if (!is.numeric(value) || length(value) != 1)
        stop("Invalid anchor value")
    if (!is.character(label) || length(label) != 1)
        stop("Invalid anchor label")
    if (!type %in% c("h", "v"))
        stop("Invalid anchor type")
    if (type == "h") {
        growAnchors(value, label, "hAnchors")
    } else {
        growAnchors(value, label, "vAnchors")
    }
}

