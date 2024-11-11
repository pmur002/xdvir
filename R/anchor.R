

growAnchors <- function(value, label, which, state) {
    anchors <- TeXget(which, state)
    if (is.null(anchors)) {
        anchors <- list(value=value, label=label)
    } else {
        anchors <- list(value=c(anchors$value, value),
                        label=c(anchors$label, label))
    }
    TeXset(which, anchors, state)
}

addAnchor <- function(value, label, type="h", state) {
    if (!is.numeric(value) || length(value) != 1)
        stop("Invalid anchor value")
    if (!is.character(label) || length(label) != 1)
        stop("Invalid anchor label")
    if (!type %in% c("h", "v"))
        stop("Invalid anchor type")
    if (type == "h") {
        growAnchors(value, label, "hAnchors", state)
    } else {
        growAnchors(value, label, "vAnchors", state)
    }
}

