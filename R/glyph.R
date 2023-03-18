
## Generic functions for generating and working with glyph info
## (regardless of which engine we are using)

glyph <- function(x, y, char, index, fontindex, size, colour=NA) {
    data.frame(x, y, char, index, fontindex, size, colour)
}

addGlyph <- function(glyph) {
    glyphs <- get("glyphs")
    glyphs[[length(glyphs) + 1]] <- glyph
    set("glyphs", glyphs)
}

