
## Package zref-savepos for recording positions

zrefSpecialPrefix <- "xdvir::zref:: "

objToGrob.XDVIRzrefObj <- function(obj, dx, dy, ..., state) {
    x <- TeX2pt(TeXget("left", state) +
                (obj$x - TeXget("zref.origin", state)$x), state) + dx
    y <- TeX2pt(-TeXget("top", state) -
                (TeXget("zref.origin", state)$y - obj$y), state) + dy
    nullGrob(x=x, y=y, default.units="bigpts", name=obj$name)
}

zrefAddObj <- function(x, state) {
    tokens <- strsplit(gsub(" *$", "", x), " ")[[1]]
    label <- tokens[2]
    x <- as.numeric(tokens[3])
    y <- as.numeric(tokens[4])
    if (label == "zref.origin") {
        TeXset("zref.origin", list(x=x, y=y), state)
    } else {
        zrefObj <- list(name=label, x=x, y=y)
        class(zrefObj) <- "XDVIRzrefObj"
        addDVIobj(zrefObj, state)
    }
}

zrefSpecial <- function(specialString, state) {
    ## Ignore any other specials
    if (grepl(paste0("^", zrefSpecialPrefix), specialString)) {
        special <- gsub(zrefSpecialPrefix, "", specialString)
        if (grepl("^mark", special)) {
            zrefAddObj(special, state)
        } else {
            warning("Unsupported zref special")
        }
    }
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
                 special=zrefSpecial)
}
