
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

isXeTeX <- function(dvi) {
    commentStr <- commentString(dvi)
    grepl("XeTeX", commentStr)
}


xelatexGrob <- function(tex,
                        x=0.5, y=0.5,
                        margin=0,
                        default.units="npc",
                        hjust="centre", vjust="centre",
                        dpi=NA,
                        packages=NULL,
                        fontLib=getOption("xdvir.fontLib"),
                        texFile=NULL,
                        name=NULL,
                        gp=gpar(),
                        vp=NULL) {
    if (!xetexAvailable())
        stop("XeTeX not available")
    engine <- getEngine("xetex")
    lib <- resolveFontLib(fontLib)
    pkgs <- resolvePackages(packages)
    texDoc <- author(tex, engine=engine, packages=pkgs)
    dviFile <- typeset(texDoc, engine=engine, texFile=texFile)
    dvi <- readDVI(dviFile)
    dviGrob(dvi,
            x=x, y=y, default.units=default.units,
            hjust=hjust, vjust=vjust,
            dpi=dpi,
            engine=engine, package=pkgs, fontLib=lib,
            name=name, gp=gp, vp=vp)
}

grid.xelatex <- function(...) {
    grid.draw(xelatexGrob(...))
}
