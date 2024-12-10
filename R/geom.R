
## ggplot2 geom supporting latex syntax

geom_latex <- function(mapping=NULL, data=NULL, stat="identity",
                       position="identity", ...,
                       nudge_x=0, nudge_y=0,
                       width=NA,
                       dpi=NA, packages=NULL,
                       engine=getOption("xdvir.engine"),
                       na.rm=FALSE,
                       show.legend=NA, inherit.aes=TRUE) {
    check_installed("ggplot2")

    if (!missing(nudge_x) || !missing(nudge_y)) {
        if (!missing(position)) {
            stop(paste("You must specify either 'position' or",
                       "'nudge_x'/'nudge_y' but not both."))
        }

        position <- ggplot2::position_nudge(nudge_x, nudge_y)
    }
    
    ggplot2::layer(data=data, mapping=mapping, stat=stat,
                   geom=GeomLatex,
                   position=position, show.legend=show.legend,
                   inherit.aes=inherit.aes,
                   params=list(width=width, dpi=dpi,
                               packages=packages, engine=engine,
                               na.rm=na.rm, ...))
}

## Trick borrowed from {marquee}
on_load(makeActiveBinding("GeomLatex",
                          function() geom_env$geom,
                          environment(geom_latex)))

geom_env <- new.env(parent=emptyenv())
assign("geom", NULL, envir=geom_env)

draw_panel_latex <- function(data, panel_params, coord, 
                             width, dpi, packages, engine,
                             na.rm=FALSE) {
    data <- coord$transform(data, panel_params)

    data$vjust <- compute_just(data$vjust, data$y, data$x, data$angle)
    data$hjust <- compute_just(data$hjust, data$x, data$y, data$angle)
    
    size <- 72 * data$size / 25.4

    col <- ggplot2::alpha(data$colour, data$alpha)
    
    packages <- c(resolvePackages(packages),
                  list("preview"))
    prefix <- preset(data$family, data$fontface, size, data$lineheight, col)
    tex <- paste(prefix, data$label, "\n", sep="")
    packages <- c(packages, attr(prefix, "packages"))

    ## Set gp=NULL so that, unless width is specified, and a relative unit,
    ## the calculation of widths and heights etc will be a LOT more efficient.
    latexGrob(tex, data$x, data$y,
              rot=data$angle, width=width, dpi=dpi,
              hjust=data$hjust, vjust=data$vjust,
              packages=packages, engine=engine,
              gp=NULL)
}

draw_key_latex <- function(data, params, size) {
    size <- 72 * data$size / 25.4
    col <- ggplot2::alpha(data$colour, data$alpha)
   
    packages <- c(resolvePackages(params$packages), list("preview"))
    prefix <- preset(data$family, data$fontface, size, data$lineheight, col)
    tex <- paste(prefix, "a", sep="")
    packages <- c(packages, attr(prefix, "packages"))

    grob  <- latexGrob(tex,
                       x=.5, y=.5,
                       margin=unit(0.1, "lines"),
                       width=params$width,
                       rot=0,
                       hjust=.5,
                       vjust=.5,
                       packages=packages,
                       engine=params$engine,
                       gp=NULL)
    attr(grob, "width")  <- convertWidth(grobWidth(grob),
                                         "cm", valueOnly=TRUE)
    attr(grob, "height") <- convertHeight(grobHeight(grob),
                                          "cm", valueOnly=TRUE)
    grob
}

make_latex_geom <- function() {
    required_aes <- c("x", "y", "label")
    default_aes <- ggplot2::aes(colour="black",
                                size=3.88,
                                angle=0,
                                hjust=0.5,
                                vjust=0.5,
                                width=NA,
                                alpha=NA,
                                family="",
                                fontface="plain",
                                lineheight=1.2)
    geom_env$geom <-
        ggplot2::ggproto("GeomLatex", ggplot2::Geom,
                         required_aes=required_aes,
                         default_aes=default_aes,
                         draw_panel=draw_panel_latex,
                         draw_key=draw_key_latex)
}

on_load(on_package_load("ggplot2", { make_latex_geom() }))


compute_just <- function (just, a = 0.5, b = a, angle = 0) {
  if (!is.character(just)) {
    return(just)
  }
  if (any(grepl("outward|inward", just))) {
    angle <- angle%%360
    angle <- ifelse(angle > 180, angle - 360, angle)
    angle <- ifelse(angle < -180, angle + 360, angle)
    rotated_forward <- grepl("outward|inward", just) & (angle > 45 & angle < 135)
    rotated_backwards <- grepl("outward|inward", just) & (angle < -45 & angle > -135)
    ab <- ifelse(rotated_forward | rotated_backwards, b, a)
    just_swap <- rotated_backwards | abs(angle) > 135
    inward <- (just == "inward" & !just_swap | just == "outward" & just_swap)
    just[inward] <- c("left", "center", "right")[just_dir(ab[inward])]
    outward <- (just == "outward" & !just_swap) | (just == "inward" & just_swap)
    just[outward] <- c("right", "center", "left")[just_dir(ab[outward])]
  }
  just
}

just_dir <- function (x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}

