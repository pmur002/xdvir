
## Package zref-savepos for recording positions

zrefSpecialPrefix <- "xdvir-zref:: "

objToGrob.XDVIRzrefObj <- function(obj, dx, dy, ..., state) {
    xpt <- unit(TeX2pt(obj$x, state) + dx, "bigpts")
    ypt <- unit(-TeX2pt(obj$y, state) + dy, "bigpts")
    ## Add a mark
    devLoc <- deviceLoc(xpt, ypt)
    addMark(obj$name,
            devx=devLoc$x, devy=devLoc$y,
            vpx=xpt, vpy=ypt, vpPath=current.vpPath())
    ## Return null grob
    nullGrob(x=xpt, y=ypt, name=obj$name)
}

zrefRecordObj <- function(x, state) {
    tokens <- strsplit(gsub(" *$", "", x), " ")[[1]]
    label <- tokens[2]
    x <- as.numeric(tokens[3])
    y <- as.numeric(tokens[4])
    if (label == "zref.origin") {
        TeXset("zref.origin", list(x=x, y=y), state)
    } else {
        objList <- TeXget("zref.objList", state)
        zrefObj <- list(name=label, x=x, y=y)
        TeXset("zref.objList", c(objList, list(zrefObj)), state)
    }
}

zrefSpecial <- function(specialString, state) {
    ## Ignore any other specials
    if (grepl(paste0("^", zrefSpecialPrefix), specialString)) {
        special <- gsub(zrefSpecialPrefix, "", specialString)
        if (grepl("^mark", special)) {
            zrefRecordObj(special, state)
        } else {
            warning("Unsupported zref special")
        }
    }
}

zrefInit <- function(state) {
    TeXset("zref.objList", NULL, state)
}

zrefFinal <- function(state) {
    ## Now we know final left/top etc, we can add zref objects for real
    ## AND create anchors from zref marks
    objList <- TeXget("zref.objList", state)
    origin <- TeXget("zref.origin", state)
    lapply(objList,
           function(obj) {
               x <- TeXget("left", state) + (obj$x - origin$x)
               y <- TeXget("top", state) + (origin$y - obj$y)
               ## Add an object to create a nullGrob and add marks
               zrefObj <- list(name=obj$name, x=x, y=y)
               class(zrefObj) <- "XDVIRzrefObj"
               addDVIobj(zrefObj, state)
               ## Add anchors
               addAnchor(x, obj$name, "h", state)
               addAnchor(y, obj$name, "v", state)
           })
}

## Define \zmark command to output saved positions to DVI
## Record zref origin using \zmark
zrefPreamble <- sprintf(r"(
\usepackage{zref-savepos}
\newcommand{\xdvirzmark}[1]{\special{%smark #1 \zposx{#1} \zposy{#1}}}
\usepackage{atbegshi}
\AtBeginShipoutFirst{\zsavepos{zref.origin}\xdvirzmark{zref.origin}}
)", zrefSpecialPrefix)

zrefPackage <- function() {
    LaTeXpackage(name="zref",
                 preamble=zrefPreamble,
                 suffix=r"()",
                 special=zrefSpecial,
                 init=zrefInit,
                 final=zrefFinal)
}
