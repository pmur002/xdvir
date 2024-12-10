
## MUST be run within, e.g., makeContent(), so that grob gp slot has
## already been enforced (so get.gpar() result is relevant)
buildTeX <- function(tex) {
    gp <- get.gpar()
    n <- length(tex)
    family <- rep(gp$fontfamily, length.out=n)
    face <- rep(c("plain", "bold", "italic", "bold-italic")[gp$font],
                length.out=n)
    size <- rep(gp$fontsize, length.out=n)
    lineheight <- rep(gp$lineheight, length.out=n)
    colour <- rep(gp$col, length.out=n)
    prefix <- preset(family, face, size, lineheight, colour)
    tex <- paste(prefix, tex, sep="")
    attr(tex, "packages") <- attr(prefix, "packages")
    tex
}

buildDVI <- function(tex, packages, engine, texFile) {
    ## Only author/typeset unique values
    uniq <- unique(tex)
    index <- match(tex, uniq)
    texDocs <- lapply(uniq, author, engine=engine, packages=packages)
    dviFiles <- lapply(texDocs, typeset, engine=engine, texFile=texFile)
    dvi <- lapply(dviFiles, readDVI)
    ## Re-expand dvis
    dvi[index]
}

makeContent.LaTeXgrob <- function(x, ...) {
    tex <- buildTeX(x$tex)
    packages <- c(x$packages, attr(tex, "packages"))
    dvi <- buildDVI(tex, packages, x$engine, x$texFile)
    setChildren(x,
                gList(dviGrob(dvi,
                              x=x$x, y=x$y, margin=x$margin, rot=x$rot,
                              hjust=x$hjust, vjust=x$vjust,
                              dpi=x$dpi,
                              engine=x$engine, packages=x$packages,
                              fontLib=x$fontLib,
                              name=x$name, gp=x$gp, vp=x$vp)))
}

xDetails.LaTeXgrob <- function(x, theta) {
    tex <- buildTeX(x$tex)
    packages <- c(x$packages, attr(tex, "packages"))
    dvi <- buildDVI(tex, packages, x$engine, x$texFile)
    xDetails(dviGrob(dvi,
                     x=x$x, y=x$y, margin=x$margin, rot=x$rot,
                     hjust=x$hjust, vjust=x$vjust,
                     dpi=x$dpi,
                     engine=x$engine, packages=x$packages,
                     fontLib=x$fontLib,
                     name=x$name, gp=x$gp, vp=x$vp),
             theta)
}

yDetails.LaTeXgrob <- function(x, theta) {
    tex <- buildTeX(x$tex)
    packages <- c(x$packages, attr(tex, "packages"))
    dvi <- buildDVI(tex, packages, x$engine, x$texFile)
    yDetails(dviGrob(dvi,
                     x=x$x, y=x$y, margin=x$margin, rot=x$rot,
                     hjust=x$hjust, vjust=x$vjust,
                     dpi=x$dpi,
                     engine=x$engine, packages=x$packages,
                     fontLib=x$fontLib,
                     name=x$name, gp=x$gp, vp=x$vp),
             theta)
}

widthDetails.LaTeXgrob <- function(x) {
    tex <- buildTeX(x$tex)
    packages <- c(x$packages, attr(tex, "packages"))
    dvi <- buildDVI(tex, packages, x$engine, x$texFile)
    widthDetails(dviGrob(dvi,
                         x=x$x, y=x$y, margin=x$margin, rot=x$rot,
                         hjust=x$hjust, vjust=x$vjust,
                         dpi=x$dpi,
                         engine=x$engine, packages=x$packages,
                         fontLib=x$fontLib,
                         name=x$name, gp=x$gp, vp=x$vp))
}

heightDetails.LaTeXgrob <- function(x) {
    tex <- buildTeX(x$tex)
    packages <- c(x$packages, attr(tex, "packages"))
    dvi <- buildDVI(tex, packages, x$engine, x$texFile)
    heightDetails(dviGrob(dvi,
                          x=x$x, y=x$y, margin=x$margin, rot=x$rot,
                          hjust=x$hjust, vjust=x$vjust,
                          dpi=x$dpi,
                          engine=x$engine, packages=x$packages,
                          fontLib=x$fontLib,
                          name=x$name, gp=x$gp, vp=x$vp))
}

################################################################################
## User API

latexGrob <- function(tex,
                      x=0.5, y=0.5,
                      margin=0,
                      rot=0,
                      default.units="npc",
                      hjust="centre", vjust="centre",
                      dpi=NA,
                      packages=NULL,
                      engine=getOption("xdvir.engine"),
                      fontLib=getOption("xdvir.fontLib"),
                      texFile=NULL,
                      name=NULL,
                      gp=gpar(),
                      vp=NULL) {
    if (length(tex) < 1)
        stop("No LaTeX fragment to render")
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    if (!is.unit(margin))
        margin <- unit(margin, default.units)
    margin <- rep(margin, length.out=4)
    ## Resolve args
    engine <- getEngine(engine)
    lib <- resolveFontLib(fontLib)
    pkgs <- resolvePackages(packages)
    if (delayTypeset(width=NA, gp=gp)) {
        ## 'gp' could be NULL
        if (is.null(gp)) {
            gp <- gpar()
        } 
        gTree(tex=tex,
              x=x, y=y, margin=margin, rot=rot,
              hjust=hjust, vjust=vjust,
              dpi=dpi,
              engine=engine, packages=pkgs, fontLib=lib,
              name=name, gp=gp, vp=vp,
              cl="LaTeXgrob")
    } else {
        dvi <- buildDVI(tex, pkgs, engine, texFile)
        dviGrob(dvi,
                x=x, y=y, margin=margin, rot=rot,
                hjust=hjust, vjust=vjust,
                dpi=dpi,
                engine=engine, packages=pkgs, fontLib=lib,
                ## 'gp' has to be NULL to get here.
                name=name, gp=gpar(), vp=vp)
    }
}

grid.latex <- function(...) {
    grid.draw(latexGrob(...))
}
