
## ggplot2 theme element supporting latex syntax

latex_grob <- function(label, x, y, hjust, vjust,
                       angle, family, fontface, colour, size, lineheight,
                       margin, rotMargins) {
    if (rotMargins) {
        ## ggplot2 margin is tlbr;  grid.latex() margin is bltr
        latexMargin <- margin[c(3, 4, 1, 2)]
    } else {
        latexMargin <- 0
    }
    packages <- list("preview")
    prefix <- preset(family, fontface, size, lineheight, colour)
    tex <- paste(prefix, label)
    packages <- c(packages, attr(prefix, "packages"))
    ## Set gp=NULL so that, unless width is specified, and a relative unit,
    ## the calculation of widths and heights etc will be a LOT more efficient.
    child <- latexGrob(tex, x, y, hjust=hjust, vjust=vjust, rot=angle,
                       margin=latexMargin,
                       packages=packages,
                       gp=NULL)
    if (rotMargins) {
        vp <- NULL
    } else {
        vp <- viewport(x=margin[4], y=margin[3],
                       width=unit(1, "npc") - margin[2] - margin[4],
                       height=unit(1, "npc") - margin[1] - margin[3],
                       just=c("left", "bottom"))
    }
    gTree(children=gList(child),
          vp=vp,
          ## Record these for width/heightDetails (x/yDetails not required)
          margin=margin, rotMargins=rotMargins,
          cl="latex_grob")
}

widthDetails.latex_grob <- function(x) {
    w <- widthDetails(x$children[[1]])
    if (!x$rotMargins) {
        w <- w + x$margin[2] + x$margin[4]
    }
    w
}

heightDetails.latex_grob <- function(x) {
    h <- heightDetails(x$children[[1]])
    if (!x$rotMargins) {
        h <- h + x$margin[1] + x$margin[3]
    }
    h
}

## Code below modelled on code from {marquee} and {ggtext} and {ggplot2}

element_latex <- function(family = NULL,
                          fontface = NULL,
                          colour = NULL,
                          size = NULL,
                          hjust = NULL, vjust = NULL,
                          angle = NULL,
                          lineheight = NULL,
                          color = NULL,
                          margin = NULL,
                          rotate_margins = FALSE,
                          inherit.blank = FALSE) {
    if (!is.null(color))
        colour <- color
    n <- max(length(family),
             length(fontface),
             length(colour), 
             length(hjust), length(vjust),
             length(angle))
    if (n > 1) {
        cli::cli_warn(c("Vectorized input to {.fn element_text} is not officially supported.",
                        i = "Results may be unexpected or may change in future versions of ggplot2."))
    }
    structure(list(family = family,
                   face = fontface,
                   colour = colour,
                   size = size,
                   hjust = hjust, vjust = vjust,
                   angle = angle, 
                   margin = margin,
                   rotate_margins = rotate_margins,
                   inherit.blank = inherit.blank),
              class = c("element_latex", "element_text", "element"))
}

element_grob.element_latex <- function(element,
                                       label = "",
                                       x = NULL, y = NULL,
                                       family = NULL,
                                       fontface = NULL,
                                       colour = NULL,
                                       alpha = NULL,
                                       size = NULL,
                                       hjust = NULL, vjust = NULL,
                                       angle = NULL,
                                       lineheight = NULL,
                                       margin = NULL,
                                       margin_x = FALSE, margin_y = FALSE,
                                       ...) {
    if (is.null(label))
        return(ggplot2::zeroGrob())
    family <- family %||% element$family
    fontface <- fontface %||% element$face
    alpha <- alpha %||% element$alpha %||% 1
    colour = ggplot2::alpha(colour %||% element$colour, alpha)
    size = size %||% element$size %||% 12
    lineheight = lineheight %||% element$lineheight %||% 1
    margin <- margin %||% element$margin %||% ggplot2::margin(0, 0, 0, 0)
    angle <- (angle %||% element$angle %||% 0) %% 360

    ## NOTE to self:  hjust and vjust should be numeric if sent
    ##                from ggplot2
    ##                (i.e., 'element$hjust' and 'element$vjust'),
    ##                but could be character if sent by user
    ##                (i.e., 'hjust' or 'vjust')

    if (is.character(hjust)) {
        if (!all(hjust %in% c("left", "bbleft",
                              "centre", "bbcentre", "center",
                              "right", "bbright")))
            stop("Unknown hjust")
    }
    if (is.character(vjust)) {
        if (!all(vjust %in% c("bottom", "baseline", "centre", "center", "top")))
            stop("Unknown vjust")
    }
    vjust <- vjust %||% element$vjust
    hjust <- hjust %||% element$hjust

    ## Often called with missing x or y that is then inferred from hjust/vjust
    numjust <- rotate_just(angle, hjust, vjust)
  
    n <- max(length(x), length(y), 1)
    x <- x %||% unit(rep(numjust$hjust, n), "npc")
    y <- y %||% unit(rep(numjust$vjust, n), "npc")

    if (!element$rotate_margins) {
        margin <- unit.c(if (margin_y) margin[1] else unit(0, "pt"),
                          if (margin_x) margin[2] else unit(0, "pt"),
                          if (margin_y) margin[3] else unit(0, "pt"),
                          if (margin_x) margin[4] else unit(0, "pt"))
    }

    latex_grob(label,
               x = x, y = y,
               hjust = hjust, vjust = vjust,
               angle = angle,
               family = family,
               fontface = fontface,
               colour = colour,
               size = size,
               lineheight = lineheight,
               margin = margin,
               rotMargins = element$rotate_margins)
}

on_load({
    on_package_load("ggplot2",
                    registerS3method("element_grob", "element_latex",
                                     element_grob.element_latex,
                                     asNamespace("ggplot2")))
})

## Copy of ggtext::rotate_just()
rotate_just <- function(angle, hjust, vjust) {
    angle <- (angle %||% 0) %% 360

    if (is.character(hjust)) {
        hjust <- ifelse(hjust %in% c("left", "bbleft"), 0,
                        ifelse(hjust %in% c("centre", "bbcentre", "center"), .5,
                               ## "right" or "bbright"
                               1))
    }
    if (is.character(vjust)) {
        vjust <- ifelse(vjust %in% c("bottom", "baseline"), 0,
                        ifelse(vjust %in% c("centre", "center"), .4,
                               ## top
                               1))
    }
    
    hnew <- ifelse(
        0 <= angle & angle < 90,
        hjust,
            ifelse(
                90 <= angle & angle < 180,
                1 - vjust,
            ifelse(
                180 <= angle & angle < 270,
                1 - hjust,
                vjust
            )
            )
    )
    
    vnew <- ifelse(
        0 <= angle & angle < 90,
        vjust,
            ifelse(
                90 <= angle & angle < 180,
                hjust,
            ifelse(
                180 <= angle & angle < 270,
                1 - vjust,
                1 - hjust
            )
            )
    )
    
    list(hjust = hnew, vjust = vnew)
}

