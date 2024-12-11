
## MUST be run within, e.g., makeContent(), so that grob gp slot has
## already been enforced (so get.gpar() result is relevant)
buildTeX <- function(tex, gp) {
    ## 'gp' could be NULL
    ## In which case we do NOT want to pick up current font family etc
    ## (must be here because width is non-NA and relative)
    if (is.null(gp)) {
        tex
    } else {
        gp <- get.gpar()
        n <- length(tex)
        family <- rep(gp$fontfamily, length.out=n)
        face <- rep(c("plain", "bold", "italic", "bold-italic")[gp$font],
                    length.out=n)
        size <- rep(gp$fontsize, length.out=n)
        lineheight <- rep(gp$lineheight, length.out=n)
        colour <- rep(gp$col, length.out=n)
        prefix <- preset(family, face, size, lineheight, colour)
        ## \n to complete the paragraph
        tex <- paste(prefix, tex, "\n", sep="")
        attr(tex, "packages") <- attr(prefix, "packages")
        tex
    } 
}

## MUST be run within, e.g., makeContent(), so that width conversion is correct
buildDVI <- function(tex, width, packages, engine, texFile) {
    ## Only author/typeset unique combinations of tex and width
    width <- convertWidth(width, "in", valueOnly=TRUE)
    uniq <- unique(cbind(tex, width))
    index <- match(paste(tex, width), apply(uniq, 1, paste, collapse=" "))   
    texDocs <- mapply(author, tex=uniq[,1], width=uniq[,2],
                      MoreArgs=list(engine=engine, packages=packages),
                      SIMPLIFY=FALSE)
    dvi <- lapply(texDocs, typeset, engine=engine, texFile=texFile)
    ## Re-expand dvis
    dvi[index]
}

makeContent.LaTeXgrob <- function(x, ...) {
    tex <- buildTeX(x$tex, x$gpar)
    packages <- c(x$packages, attr(tex, "packages"))
    dvi <- buildDVI(tex, x$width, packages, x$engine, x$texFile)
    setChildren(x,
                gList(dviGrob(dvi,
                              x=x$x, y=x$y, margin=x$margin, rot=x$rot,
                              hjust=x$hjust, vjust=x$vjust,
                              dpi=x$dpi,
                              engine=x$engine, packages=x$packages,
                              fontLib=x$fontLib,
                              texFile=x$texFile,
                              name=x$name, vp=x$vp)))
}

xDetails.LaTeXgrob <- function(x, theta) {
    tex <- buildTeX(x$tex, x$gpar)
    packages <- c(x$packages, attr(tex, "packages"))
    dvi <- buildDVI(tex, x$width, packages, x$engine, x$texFile)
    xDetails(dviGrob(dvi,
                     x=x$x, y=x$y, margin=x$margin, rot=x$rot,
                     hjust=x$hjust, vjust=x$vjust,
                     dpi=x$dpi,
                     engine=x$engine, packages=x$packages,
                     fontLib=x$fontLib,
                     texFile=x$texFile,
                     name=x$name, vp=x$vp),
             theta)
}

yDetails.LaTeXgrob <- function(x, theta) {
    tex <- buildTeX(x$tex, x$gpar)
    packages <- c(x$packages, attr(tex, "packages"))
    dvi <- buildDVI(tex, x$width, packages, x$engine, x$texFile)
    yDetails(dviGrob(dvi,
                     x=x$x, y=x$y, margin=x$margin, rot=x$rot,
                     hjust=x$hjust, vjust=x$vjust,
                     dpi=x$dpi,
                     engine=x$engine, packages=x$packages,
                     fontLib=x$fontLib,
                     texFile=x$texFile,
                     name=x$name, vp=x$vp),
             theta)
}

widthDetails.LaTeXgrob <- function(x) {
    tex <- buildTeX(x$tex, x$gpar)
    packages <- c(x$packages, attr(tex, "packages"))
    dvi <- buildDVI(tex, x$width, packages, x$engine, x$texFile)
    widthDetails(dviGrob(dvi,
                         x=x$x, y=x$y, margin=x$margin, rot=x$rot,
                         hjust=x$hjust, vjust=x$vjust,
                         dpi=x$dpi,
                         engine=x$engine, packages=x$packages,
                         fontLib=x$fontLib,
                         texFile=x$texFile,
                         name=x$name, vp=x$vp))
}

heightDetails.LaTeXgrob <- function(x) {
    tex <- buildTeX(x$tex, x$gpar)
    packages <- c(x$packages, attr(tex, "packages"))
    dvi <- buildDVI(tex, x$width, packages, x$engine, x$texFile)
    heightDetails(dviGrob(dvi,
                          x=x$x, y=x$y, margin=x$margin, rot=x$rot,
                          hjust=x$hjust, vjust=x$vjust,
                          dpi=x$dpi,
                          engine=x$engine, packages=x$packages,
                          fontLib=x$fontLib,
                          texFile=x$texFile,
                          name=x$name, vp=x$vp))
}

################################################################################
## User API

latexGrob <- function(tex,
                      x=0.5, y=0.5,
                      margin=0,
                      rot=0,
                      default.units="npc",
                      hjust="centre", vjust="centre",
                      width=NA,
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
    if (!is.unit(width)) 
        width <- unit(width, default.units)
    ## Resolve args
    engine <- getEngine(engine)
    lib <- resolveFontLib(fontLib)
    pkgs <- resolvePackages(packages)
    if (delayTypeset(width, gp)) {
        gTree(tex=tex,
              x=x, y=y, margin=margin, rot=rot,
              hjust=hjust, vjust=vjust,
              width=width,
              dpi=dpi,
              engine=engine, packages=pkgs, fontLib=lib,
              texFile=texFile,
              ## retain 'gp' as 'gpar' for child DVIgrob
              gpar=gp, 
              name=name, gp=if (is.null(gp)) gpar() else gp, vp=vp,
              cl="LaTeXgrob")
    } else {
        dvi <- buildDVI(tex, width, pkgs, engine, texFile)
        dviGrob(dvi,
                x=x, y=y, margin=margin, rot=rot,
                hjust=hjust, vjust=vjust,
                dpi=dpi,
                engine=engine, packages=pkgs, fontLib=lib,
                texFile=texFile,
                ## 'gp' has to be NULL to get here.
                name=name, gp=gpar(), vp=vp)
    }
}

grid.latex <- function(...) {
    grid.draw(latexGrob(...))
}
