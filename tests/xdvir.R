
## Code for testing development of 'xdvir'

## ALSO DEPENDENT ON
## kpsewhich
## ttx

library(xdvir)

user <- Sys.getenv("USERNAME")
if (user == "pmur002")
    options(xdvir.ttxCacheDir="/scratch/TTXfonts/")

if (.Platform$OS.type == "unix")
    fontMap("Ryumin-Light"="IPAMincho", "GothicBBB-Medium"="IPAGothic")

## LuaLaTeX
## (fontdef description more detailed)
## system("lualatex --output-format=dvi test.tex; mv test.dvi test-lualatex.dvi")
luadvi <- readDVI("test-lualatex.dvi")
luadvigrob <- dviGrob(luadvi, engine=lualatexEngine)

## NOTE the \usepackage{unicode-maths} to force TrueType math font
## system("lualatex --output-format=dvi eqn.tex; mv eqn.dvi eqn-lualatex.dvi")
eqndvi <- readDVI("eqn-lualatex.dvi")
eqndvigrob <- dviGrob(eqndvi, engine=lualatexEngine)

## upTeX
## (op255)
## system("uplatex uptest.tex; mv uptest.dvi uptest-uplatex.dvi")
updvi <- readDVI("uptest-uplatex.dvi")
updvigrob <- dviGrob(updvi, engine=uplatexEngine)

grid.newpage()
## DEBUG
grid.rect(width=unit(luadvigrob$children[[1]]$glyphInfo$width["width"],
                     "bigpts"),
          height=unit(luadvigrob$children[[1]]$glyphInfo$height["height"],
                      "bigpts"))
grid.rect(width=unit(updvigrob$children[[1]]$glyphInfo$width["width"],
                     "bigpts"),
          height=unit(updvigrob$children[[1]]$glyphInfo$height["height"],
                      "bigpts"))
grid.segments(0, .5, 1, .5, gp=gpar(col="grey"))
grid.segments(.5, 0, .5, 1, gp=gpar(col="grey"))
## Text
grid.draw(luadvigrob)
grid.draw(updvigrob)

          
## XeTeX
## (op252, op253, op254 [and font def specifies font file])
## system("xelatex --no-pdf test.tex; mv test.xdv test-xelatex.xdv")
library(xdvir)
xdvi <- readDVI("test-xelatex.xdv")
## debug(xdvir:::grob_op_253)
## debug(xdvir:::getTable)
## debug(xdvir:::generateGLYF)
xdvigrob <- dviGrob(xdvi, engine=xelatexEngine)
grid.rect(width=unit(xdvigrob$children[[1]]$glyphInfo$width["width"],
                     "bigpts"),
          height=unit(xdvigrob$children[[1]]$glyphInfo$height["height"],
                      "bigpts"))
grid.draw(xdvigrob)

## Plain LaTeX
## system("latex test.tex; mv test.dvi test-latex.dvi")
readDVI("test-latex.dvi")

