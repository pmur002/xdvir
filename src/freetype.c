
#include "freetype.h"

SEXP glyphMetrics(SEXP index, SEXP font) {
    FT_Library  library;
    FT_Face     face; 
    int err;

    SEXP result;
    PROTECT(result = allocVector(INTSXP, 6));

    err = FT_Init_FreeType(&library);
    if (err) {
        error("Intialisation failed");
    } 

    err = FT_New_Face(library,
                      CHAR(STRING_ELT(font, 0)),
                      0,
                      &face);
    if (err == FT_Err_Unknown_File_Format) {
        error("Unknown font format");
    } else if (err) {
        error("Font read failed");
    } 
    INTEGER(result)[0] = face->units_per_EM;
    
    err = FT_Set_Char_Size(face, 0, 12*64, 96, 0);
    if (err) {
        error("Set char size failed");
    }
    
    err = FT_Load_Glyph(face, INTEGER(index)[0], FT_LOAD_NO_SCALE);
    if (err) {
        error("Glyph load failed");
    }
    INTEGER(result)[1] = face->glyph->metrics.horiAdvance;
    INTEGER(result)[2] = face->glyph->metrics.horiBearingX;
    INTEGER(result)[3] = face->glyph->metrics.horiBearingX + 
        face->glyph->metrics.width;
    INTEGER(result)[4] = face->glyph->metrics.horiBearingY;
    INTEGER(result)[5] = face->glyph->metrics.horiBearingY - 
        face->glyph->metrics.height;

    err = FT_Done_Face(face);
    if (err) {
        error("Face clean up failed");
    }

    err = FT_Done_FreeType(library);
    if (err) {
        error("Shut down failed");
    }

    UNPROTECT(1);
    return result;
}
