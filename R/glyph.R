
## Generic functions for generating and working with glyph info
## (regardless of which engine we are using)

glyph <- function(x, y, char, index, fontindex, size, colour=NA) {
    data.frame(x, y, char, index, fontindex, size, colour)
}

addGlyph <- function(glyph) {
    glyphs <- get("glyphs")
    glyphNum <- get("glyphNum")
    glyphs[[glyphNum]] <- glyph
    set("glyphs", glyphs)
    set("glyphNum", glyphNum + 1)
}

