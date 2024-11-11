
library(dvi)

readDVI(system.file("DVI", "test-pdftex.dvi", package="xdvir"))

readDVI(system.file("DVI", "test-luatex.dvi", package="xdvir"))

readDVI(system.file("DVI", "test-xetex.xdv", package="xdvir"))

readDVI(system.file("DVI", "test-uptex.dvi", package="xdvir"))
