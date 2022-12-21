

dvir::readDVI("test.dvi")



library(dvir)
grid.newpage()
grid.latex("$x - \\mu$")
downViewport("dvi.vp")
grid.rect(gp=gpar(col="grey", fill=NA))


library(dvir)
grid.newpage()
grid.tikzpicture("\\draw (0,0)--(1,1);")
downViewport("dvi.vp")
grid.rect(gp=gpar(col="grey", fill=NA))
           

library(dvir)
grid.newpage()
grid.tikzpicture("\\draw (0,0)--(1,1);", bbox=c(.2, .2, .8, .8))
downViewport("dvi.vp")
grid.rect(gp=gpar(col="grey", fill=NA))
           

