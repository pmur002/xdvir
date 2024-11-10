
library(dvi)

dvi <- readDVI(system.file("DVI", "test-pdftex.dvi", package="dvi"))

opCodes(dvi)

opParams(dvi)

opNames(opCodes(dvi))
