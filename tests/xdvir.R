
## Code for testing development of 'xdvir'

## ALSO DEPENDENT ON
## kpsewhich
## ttx
## fc-match

library(hexView)
library(xml2)
library(systemfonts)
library(grid)

## Source source code
src <- file.path("../R",
                 c("dvi.R", "vf.R", "read.R", 
                   "state.R", "convert.R", "font.R", "metric.R",
                   "ttx.R", "font-api.R",
                   "engine.R", "luatex.R", "uptex.R",
                   "ops.R",
                   "print.R",
                   "adobe.R", "glyph.R", "grob.R",
                   "grid.R"))
for (i in src) source(i)

initTTX()
initUpTeX()

## LuaLaTeX
## (fontdef description more detailed)
## system("lualatex --output-format=dvi test.tex; mv test.dvi test-lualatex.dvi")
luadvi <- readDVI("test-lualatex.dvi")
luadvigrob <- dviGrob(luadvi)

## upTeX
## (op255)
## system("uplatex uptest.tex; mv uptest.dvi uptest-uplatex.dvi")
updvi <- readDVI("uptest-uplatex.dvi")
updvigrob <- dviGrob(updvi, engine=uplatexEngine, y=0)

grid.newpage()
grid.draw(luadvigrob)
grid.draw(updvigrob)

## XeTeX
## (op252, op253, op254 [and font def specifies font file])
## system("xelatex --no-pdf test.tex; mv test.xdv test-xelatex.xdv")
readDVI("test-xelatex.xdv")

## Plain LaTeX
## system("latex test.tex; mv test.dvi test-latex.dvi")
readDVI("test-latex.dvi")

