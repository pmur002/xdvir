
## Generic functions for generating and working with glyph info
## (regardless of which engine we are using)

glyph <- function(x, y, char, index, fontindex, size, colour=NA,
                  rotation=0, scaleX=1, scaleY=1, skewX=0, skewY=0) {
    data.frame(x, y, char, index, fontindex, size, colour,
               rotation, scaleX, scaleY, skewX, skewY)
}

addGlyph <- function(glyph) {
    glyphs <- get("glyphs")
    glyphs[[length(glyphs) + 1]] <- glyph
    set("glyphs", glyphs)
}

