
## Record glyph info

glyph <- function(x, y, index, fontindex, size, colour=NA,
                  rotation=0, scaleX=1, scaleY=1, skewX=0, skewY=0) {
    data.frame(x, y, index, fontindex, size, colour,
               rotation, scaleX, scaleY, skewX, skewY)
}

addGlyph <- function(glyph, state) {
    glyphs <- TeXget("glyphs", state)
    glyphs[[length(glyphs) + 1]] <- glyph
    TeXset("glyphs", glyphs, state)
}

