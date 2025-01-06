
initXeTeX <- function() {
    xetex <- Sys.which("xelatex")
    if (nchar(xetex) > 0) {
        versText <- system("xelatex --version", intern=TRUE)
        versLine <- grep("^XeTeX", versText)
        version <- gsub(".+ ([0-9.-]+) .+", "\\1", versText[versLine])
        set("xetexVersion", version)
    }
}

xetexVersion <- function() {
    get("xetexVersion")
}

xetexAvailable <- function() {
    !is.null(xetexVersion())
}

## Ensure non-Type1 math font
xelatexPreamble <- "\\usepackage{unicode-math}"

isXeTeX <- function(commentStr) {
    grepl("XeTeX", commentStr)
}


xelatexGrob <- function(tex, ...) {
    if (!xetexAvailable())
        stop("XeTeX not available")
    latexGrob(tex, engine=getEngine("xetex"), ...)
}

grid.xelatex <- function(...) {
    grid.draw(xelatexGrob(...))
}
