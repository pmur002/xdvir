
library(dvi)

readDVI(system.file("DVI", "test-pdftex.dvi", package="dvi"))

readDVI(system.file("DVI", "test-luatex.dvi", package="dvi"))

readDVI(system.file("DVI", "test-xetex.xdv", package="dvi"))

readDVI(system.file("DVI", "test-uptex.dvi", package="dvi"))
