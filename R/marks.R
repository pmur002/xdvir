
## Maintain a database of marks on the page

## Null the database on a new page
initMarks <- function() {
    set("marks", NULL)
}

setHook("grid.newpage", initMarks)

## Add a mark
addMark <- function(name, devx, devy, vpx=NA, vpy=NA, vpPath=NULL) {
    if (!is.character(name) || length(name) != 1)
        stop("Invalid mark name")
    if (!(is.na(devx) || is.numeric(devx)) || length(devx) != 1 ||
        !(is.na(devy) || is.numeric(devy)) || length(devy) != 1 ||
        !(is.na(vpx) || is.numeric(vpx)) || length(vpx) != 1 ||
        !(is.na(vpy) || is.numeric(vpy)) || length(vpy) != 1)
        stop("Invalid mark location")
    if (!(is.null(vpPath) || inherits(vpPath, "vpPath"))) 
        stop("Invalid mark vpPath")
    marks <- get("marks")
    marks[[name]] <- list(devx=devx, devy=devy, vpx=vpx, vpy=vpy, vpPath=vpPath)
    set("marks", marks)
}

## Get a mark location   
getMark <- function(name) {
    marks <- get("marks")
    if (!(name %in% names(marks)))
        stop(sprintf("Unknown mark (%s).", name))
    marks[[name]]
}
    

